;;; quarto-preview.el --- Live Quarto previews in WebKit -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep one Quarto preview process and browser per source buffer.  Quarto's
;; own file watcher and browser reload connection handle changes on save.

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'shell)
(require 'subr-x)
(require 'url-util)
(declare-function xwidget-webkit-current-session "xwidget" ())

(defgroup quarto nil "Editing Quarto documents." :group 'text)

(require 'quarto-python)
(require 'quarto-context)

(defcustom quarto-preview-flags ""
  "Extra arguments for Quarto preview, with shell-style quoting.
Shell expansion is not performed.  Use a prefix argument when starting
the preview to apply changes to these flags.  The command always serves
locally with file watching enabled, without opening an external browser."
  :group 'quarto
  :type 'string)

(defvar-local quarto-preview--process nil)
(defvar-local quarto-preview--url nil)
(defvar-local quarto-preview--browser nil)
(defvar-local quarto-preview--source nil
  "Source buffer associated with a preview browser or process log.")

(defun quarto-preview--source-buffer ()
  "Return the source even when called from a chunk, browser or log."
  (or quarto-preview--source (quarto-source-buffer)))

(defun quarto-preview--arguments (file flags)
  "Build the preview argument list for FILE and FLAGS."
  (let ((args (awc-project-split-arguments flags)) extra)
    ;; These options conflict with finding the URL or keeping a live server.
    ;; A custom port, profile, execution options and HTML variants are fine.
    (while args
      (let ((arg (pop args)))
        (cond
         ((equal arg "--no-browser"))
         ((or (equal arg "--host") (string-prefix-p "--host=" arg))
          (unless (member (if (equal arg "--host") (pop args) (substring arg 7))
                          '("127.0.0.1" "localhost"))
            (user-error "Embedded preview requires a loopback host")))
         ((string-match-p
           "\\`--\\(?:no-serve\\|no-watch\\(?:-inputs\\)?\\|no-render\\|no-navigate\\|quiet\\|browser\\|log-level\\|log-format\\)\\(?:=\\|\\'\\)"
           arg)
          (user-error "%s conflicts with an embedded live preview" arg))
         (t (push arg extra)))))
    (append (list "preview" file) (nreverse extra)
            (with-current-buffer (quarto-source-buffer)
              (append '("--to" "html") (when quarto-profile (list "--profile" quarto-profile))))
            '("--no-browser" "--host" "127.0.0.1"))))

(defun quarto-preview--show (source)
  "Display SOURCE's preview alongside the editing buffer."
  (when (buffer-live-p source)
    (with-current-buffer source
      (if (not (and (display-graphic-p) (featurep 'xwidget-internal)))
          (browse-url quarto-preview--url)
      (require 'xwidget)
      (let* ((browser quarto-preview--browser)
             (url quarto-preview--url)
             (target (if (buffer-live-p browser) browser
                       (process-buffer quarto-preview--process)))
             (window (or (and (buffer-live-p browser) (get-buffer-window browser t))
                         (display-buffer
                          target
                          '((display-buffer-reuse-window display-buffer-pop-up-window)
                            (inhibit-same-window . t))))))
        (unless (window-live-p window)
          (user-error "Cannot display preview; URL: %s" url))
        (with-selected-window window
          (if (buffer-live-p browser)
              ;; Navigating only after a server restart preserves scroll and
              ;; form state when C-c C-p merely redisplays a running preview.
              (with-current-buffer browser
                (unless (equal quarto-preview--url url)
                  (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url)
                  (setq quarto-preview--url url)))
            (xwidget-webkit-browse-url url t)
            (setq browser (current-buffer))
            (setq-local quarto-preview--source source)
            (setq-local quarto-preview--url url)
            (with-current-buffer source
              (setq quarto-preview--browser browser)))))))))

(defun quarto-preview--ready (process)
  "Open the browser once PROCESS has announced its URL."
  (let ((source (process-get process 'quarto-source))
        (frame (process-get process 'quarto-frame)))
    (when (and (process-live-p process) (buffer-live-p source)
               (eq process (buffer-local-value 'quarto-preview--process source))
               (frame-live-p frame))
      (condition-case err
          (with-selected-frame frame
            (quarto-preview--show source))
        (error (message "Cannot open Quarto preview: %s (URL: %s)"
                        (error-message-string err)
                        (buffer-local-value 'quarto-preview--url source)))))))

(defun quarto-preview--watch-output ()
  "Find Quarto's ready URL in the current compilation buffer.
Scan accumulated output so split process chunks and ANSI colours work.
Wait for the complete line rather than opening a partially received URL."
  (let ((source quarto-preview--source)
        (process (get-buffer-process (current-buffer))))
    (when (and (buffer-live-p source) process
               (eq process (buffer-local-value 'quarto-preview--process source))
               (not (buffer-local-value 'quarto-preview--url source)))
      (let* ((ansi-color-context nil)
             (output (ansi-color-filter-apply
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (when (string-match
               "Browse at \\(http://\\(?:localhost\\|127\\.0\\.0\\.1\\):[0-9]+/[^\r\n]*\\)[\r\n]"
               output)
          (let ((url (replace-regexp-in-string
                      "\\`http://localhost:" "http://127.0.0.1:"
                      (url-encode-url (string-trim-right (match-string 1 output))))))
            (with-current-buffer source (setq quarto-preview--url url))
            (run-at-time 0 nil #'quarto-preview--ready process)))))))

(defun quarto-preview--finished (process status)
  "Report preview PROCESS completion with STATUS."
  (let ((source (process-get process 'quarto-source))
        (log (process-buffer process)))
    (when (and (not (process-live-p process)) (buffer-live-p source)
               (buffer-live-p log)
               (eq process (buffer-local-value 'quarto-preview--process source))
               (not (process-get process 'quarto-stopping)))
      (message "Quarto preview %s; see %s" (string-trim status) (buffer-name log))
      (display-buffer log))))

;;;###autoload
(defun quarto-stop-preview ()
  "Stop the preview associated with this source, chunk, browser or log.
The browser remains available, but no longer refreshes until restarted."
  (interactive)
  (let ((source (quarto-preview--source-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (process-live-p quarto-preview--process)
          (process-put quarto-preview--process 'quarto-stopping t)
          (delete-process quarto-preview--process))
        (setq quarto-preview--url nil)))
    (when (called-interactively-p 'interactive)
      (message "Quarto preview stopped"))))

;;;###autoload
(defun quarto-preview--start (&optional restart context)
  "Save this Quarto document and open a live preview inside Emacs.
Reuse its server and browser if already running.  With prefix RESTART,
restart the server (for example after changing `quarto-preview-flags').
Python chunks use the project's virtual environment or
`quarto-preview-python', without changing other Emacs sessions.
Saving the document triggers Quarto's live refresh.  Use
`quarto-stop-preview' to stop; killing the source buffer also stops it."
  (let ((source (quarto-preview--source-buffer)))
    (unless (buffer-live-p source) (user-error "The Quarto source buffer was closed"))
    (with-current-buffer source
      (unless (and buffer-file-name
                   (string-equal (downcase (or (file-name-extension buffer-file-name) "")) "qmd"))
        (user-error "Open a saved .qmd file to preview"))
      (when (file-remote-p buffer-file-name)
        (user-error "Embedded preview currently requires a local .qmd file"))
      (let* ((program (or (executable-find "quarto")
                          (user-error "Cannot find quarto on exec-path")))
             (args (quarto-preview--arguments buffer-file-name quarto-preview-flags))
             (python (if (and (not restart) (process-live-p quarto-preview--process))
                         (process-get quarto-preview--process 'quarto-python)
                       (if context (plist-get context :python) (quarto-preview--python buffer-file-name)))))
        (save-buffer)
        (when restart (quarto-stop-preview))
        (if (process-live-p quarto-preview--process)
            (if quarto-preview--url
                (quarto-preview--show source)
              (display-buffer (process-buffer quarto-preview--process))
              (message "Quarto is still preparing this preview"))
          (setq quarto-preview--url nil)
          ;; Reconnect the browser even if a restarted server gets the same
          ;; port: its previous live-reload WebSocket has been closed.
          (when (buffer-live-p quarto-preview--browser)
            (with-current-buffer quarto-preview--browser
              (setq quarto-preview--url nil)))
          (let* ((default-directory (or (plist-get context :root) (awc-project-root)))
                 (compilation-environment
                  (append (quarto-preview--python-environment python) compilation-environment))
                 (name (format "*Quarto preview: %s*" (abbreviate-file-name buffer-file-name)))
                 (log (compilation-start
                       (mapconcat #'shell-quote-argument (cons program args) " ")
                       'compilation-mode (lambda (_) name)))
                 (process (get-buffer-process log)))
            (setq quarto-preview--process process)
            (process-put process 'quarto-source source)
            (process-put process 'quarto-python python)
            (process-put process 'quarto-frame (selected-frame))
            (let ((sentinel (process-sentinel process)))
              (set-process-sentinel
               process (lambda (proc event)
                         (funcall sentinel proc event)
                         (quarto-preview--finished proc event))))
            (with-current-buffer log
              (setq-local quarto-preview--source source)
              (when (fboundp 'quarto-render-error-links) (quarto-render-error-links))
              (when python (setq-local header-line-format (list " Quarto Python: " python)))
              (add-hook 'compilation-filter-hook #'quarto-preview--watch-output nil t)
              (quarto-preview--watch-output))
            (add-hook 'kill-buffer-hook #'quarto-stop-preview nil t)))))))

(defun quarto-output-start (function restart)
  "Save and resolve the document before invoking output FUNCTION with RESTART."
  (with-current-buffer (quarto-preview--source-buffer)
    (unless (and buffer-file-name (string-match-p "\\.qmd\\'" buffer-file-name))
      (user-error "Open a saved .qmd document"))
    (save-buffer)
    (let ((source (current-buffer)) (frame (selected-frame)))
      (quarto-context-resolve
       (lambda (context error)
         (if error (message "Quarto output: %s" error)
           (when (and (buffer-live-p source) (frame-live-p frame))
             (with-selected-frame frame
               (with-current-buffer source (funcall function restart context))))))))))

(defun quarto-preview-document (&optional restart)
  "Preview HTML in WebKit, or in the external browser when WebKit is unavailable."
  (interactive "P")
  (quarto-output-start #'quarto-preview--start restart))

(provide 'quarto-preview)
;;; quarto-preview.el ends here

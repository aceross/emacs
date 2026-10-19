;;; awc-writing.el --- Explicit project prose QA with Vale -*- lexical-binding: t; -*-

;;; Commentary:
;; No hooks start this checker.  Project rules own the editorial policy;
;; compilation-mode owns display and navigation.  Nothing rewrites the source.

;;; Code:
(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'subr-x)

(defgroup awc-writing nil "Project academic writing checks." :group 'text)
(defcustom awc-writing-program "vale"
  "Vale executable.  Version 3.24.0 is the validated version."
  :type 'string)
(defcustom awc-writing-timeout 30
  "Seconds before an explicit buffer check is cancelled."
  :type 'number)

(defvar-local awc-writing--process nil)
(defvar-local awc-writing--results nil)
(defvar-local awc-writing--source nil)
(defvar-local awc-writing--tick nil)
(defvar-local awc-writing--file nil)

(defun awc-writing--base ()
  "Return the source document from prose, an indirect chunk or its QA report."
  (let ((source (if (derived-mode-p 'awc-writing-results-mode)
                    (if (buffer-live-p awc-writing--source) awc-writing--source
                      (user-error "Writing source is closed"))
                  (current-buffer))))
    (or (buffer-base-buffer source) source)))

(defun awc-writing--config (file)
  "Find the nearest readable Vale configuration above FILE."
  (let* ((names '(".vale.ini" "_vale.ini" "vale.ini" ".vale" "_vale"))
         (dir (locate-dominating-file
               (file-name-directory file)
               (lambda (directory)
                 (cl-some (lambda (name)
                            (file-regular-p (expand-file-name name directory))) names))))
         (config (and dir (cl-find-if
                           #'file-regular-p
                           (mapcar (lambda (name) (expand-file-name name dir)) names)))))
    (unless (and config (file-readable-p config))
      (user-error "No readable project .vale.ini; see doc/academic-writing-qa.org and etc/writing/"))
    config))

(defun awc-writing--current-p (source tick file)
  "Whether SOURCE still represents the checked TICK and FILE."
  (and (buffer-live-p source)
       (with-current-buffer source
         (and (= (buffer-chars-modified-tick) tick)
              (equal buffer-file-name file)))))

(defun awc-writing--guard ()
  "Refuse to navigate diagnostics for an edited or closed source."
  (unless (awc-writing--current-p awc-writing--source awc-writing--tick awc-writing--file)
    (user-error "Writing QA is stale; check the source buffer again")))

(defun awc-writing--next-error (n reset)
  "Navigate N compilation errors, respecting RESET and source revision."
  (awc-writing--guard)
  (compilation-next-error-function n reset))

(defun awc-writing-goto-finding (&optional event)
  "Visit the finding at point, or at mouse EVENT, if still current."
  (interactive (list last-input-event))
  (awc-writing--guard)
  (compile-goto-error event))

(defun awc-writing--results-killed ()
  "Cancel only this result buffer's running check."
  (when (buffer-live-p awc-writing--source)
    (with-current-buffer awc-writing--source
      (awc-writing-cancel))))

(defun awc-writing--compilation-file (_filename)
  "Resolve findings to the checked buffer, including a never-saved file."
  awc-writing--source)

(define-derived-mode awc-writing-results-mode compilation-mode "Writing QA"
  "Located project prose findings.  RET visits; M-g n/p navigates.
Errors report spelling or explicit project policy.  Suggestions need review."
  (setq-local compilation-error-regexp-alist '(awc-writing)
              compilation-error-regexp-alist-alist
              '((awc-writing "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): \\(?:error\\|\\(warning\\)\\|\\(suggestion\\)\\):" 1 2 (3 . 4) (5 . 6)))
              compilation-error-screen-columns nil
              compilation-parse-errors-filename-function #'awc-writing--compilation-file
              compilation-skip-threshold 0
              next-error-function #'awc-writing--next-error)
  (add-hook 'kill-buffer-hook #'awc-writing--results-killed nil t))
(define-key awc-writing-results-mode-map (kbd "RET") #'awc-writing-goto-finding)
(define-key awc-writing-results-mode-map [mouse-2] #'awc-writing-goto-finding)
(define-key awc-writing-results-mode-map (kbd "g") #'awc-writing-recheck)
(define-key awc-writing-results-mode-map (kbd "k") #'awc-writing-cancel)

(defun awc-writing--write (buffer text)
  "Replace result BUFFER with TEXT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (compilation-forget-errors)
        (insert text)
        (goto-char (point-min))))))

(defun awc-writing--parse (output file source)
  "Read Vale JSON OUTPUT for FILE and validate its ranges in SOURCE.
Return rendered compilation lines and a finding count."
  (let* ((data (json-parse-string output :object-type 'hash-table :array-type 'list))
         (findings (gethash file data))
         lines)
    ;; --path must identify precisely the source supplied on stdin.  A valid
    ;; empty report is {}, not arbitrary JSON or another file's diagnostics.
    (maphash (lambda (key _)
               (unless (equal key file) (error "Unexpected Vale report path: %s" key))) data)
    (dolist (finding findings)
      (let* ((line (gethash "Line" finding))
             (span (gethash "Span" finding))
             (start (car span)) (end (cadr span))
             (severity (gethash "Severity" finding))
             (rule (gethash "Check" finding))
             (message (gethash "Message" finding)))
        (unless (and (integerp line) (> line 0)
                     (= (length span) 2)
                     (integerp start) (integerp end) (<= 1 start end)
                     (member severity '("error" "warning" "suggestion"))
                     (stringp rule) (stringp message)
                     (with-current-buffer source
                       (save-restriction
                         (widen)
                         (save-excursion
                           (goto-char (point-min))
                           (and (zerop (forward-line (1- line)))
                                (<= end (- (line-end-position) (line-beginning-position))))))))
          (error "Vale returned an unsupported source range or malformed finding"))
        (push (format "%s:%d:%d-%d: %s: %s: %s\n"
                      file line start end severity rule
                      (replace-regexp-in-string "[\n\r]+" " " message)) lines)))
    (cons (apply #'concat (nreverse lines)) (length findings))))

(defun awc-writing--finish (process _event)
  "Publish completed PROCESS output, disposing its owned helpers."
  (when (memq (process-status process) '(exit signal failed))
    (let* ((source (process-get process 'source))
           (results (process-get process 'results))
           (out (process-buffer process))
           (err (process-get process 'stderr)))
      (unwind-protect
          (unless (process-get process 'cancelled)
            (condition-case problem
                (cond
                 ((not (awc-writing--current-p source (process-get process 'tick)
                                                (process-get process 'file)))
                  (awc-writing--write results "Writing QA discarded: source changed or closed. Check again.\n"))
                 ((not (and (eq (process-status process) 'exit)
                            (zerop (process-exit-status process))
                            (with-current-buffer err (string-empty-p (string-trim (buffer-string))))))
                  (error "Vale could not run (exit %s). Check its version, project rules and dictionaries.\n%s\n%s"
                         (process-exit-status process)
                         (with-current-buffer err (buffer-string))
                         (with-current-buffer out (buffer-string))))
                 (t
                  (let* ((parsed (awc-writing--parse
                                  (with-current-buffer out (buffer-string))
                                  (process-get process 'path) source))
                         (seconds (- (float-time) (process-get process 'started))))
                    (awc-writing--write
                     results
                     (concat
                      (if (zerop (cdr parsed))
                          "No findings returned. Rules may be absent, disabled, or have nothing to report.\nReview project rules with C-c w W v; this is not a validation pass.\n"
                        (format "Writing QA: %d findings in %.3fs (buffer snapshot)\n" (cdr parsed) seconds))
                      (format "Rules: %s\nErrors: spelling/project policy; suggestions: editorial review.\nRET or M-g n/p: source; g: recheck; k: cancel.\n\n%s"
                              (process-get process 'config) (car parsed))))
                    (if (zerop (cdr parsed))
                        (message "Writing QA returned no findings; review configured rule coverage")
                      (message "Writing QA: %d findings (%.3fs)" (cdr parsed) seconds)))))
              (error (awc-writing--write results (format "Writing QA failed: %s\n" (error-message-string problem)))
                     (message "Writing QA failed; see %s" (buffer-name results)))))
        (when (timerp (process-get process 'timer)) (cancel-timer (process-get process 'timer)))
        (when (buffer-live-p source)
          (with-current-buffer source
            (when (eq process awc-writing--process) (setq awc-writing--process nil))))
        (when (buffer-live-p out) (kill-buffer out))
        (when (buffer-live-p err) (kill-buffer err))))))

(defun awc-writing-cancel ()
  "Cancel this source/result buffer's check without touching other jobs."
  (interactive)
  (let ((source (or awc-writing--source (awc-writing--base))))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (processp awc-writing--process)
          (process-put awc-writing--process 'cancelled t)
          (awc-writing--write awc-writing--results "Writing QA cancelled. Check again when ready.\n")
          (delete-process awc-writing--process))))))

;;;###autoload
(defun awc-writing-check ()
  "Explicitly check the full local Markdown, Quarto or Org buffer with Vale.
Use the nearest project configuration, without global rules or downloads.
Check unsaved text through stdin; do not save or rewrite the document."
  (interactive)
  (with-current-buffer (awc-writing--base)
    (unless (and buffer-file-name (not (file-remote-p buffer-file-name))
                 (not (file-remote-p default-directory))
                 (member (file-name-extension buffer-file-name) '("md" "markdown" "qmd" "org")))
      (user-error "Writing QA supports local .md, .markdown, .qmd and .org file buffers"))
    (let* ((program (or (executable-find awc-writing-program)
                        (user-error "Vale is missing; install it explicitly or set awc-writing-program (see doc/academic-writing-qa.org)")))
           (file buffer-file-name)
           (config (awc-writing--config file))
           (default-directory (file-name-directory config))
           (path (file-relative-name file default-directory))
           (process-environment (copy-sequence process-environment))
           (source (current-buffer))
           (tick (buffer-chars-modified-tick))
           (text (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
           (results (or (and (buffer-live-p awc-writing--results) awc-writing--results)
                        (generate-new-buffer (format "*Writing QA: %s*" (buffer-name)))))
           out err process)
      (awc-writing-cancel)
      (setq awc-writing--results results)
      (add-hook 'kill-buffer-hook #'awc-writing-cancel nil t)
      (setenv "VALE_CONFIG_PATH" nil)
      (setenv "VALE_STYLES_PATH" nil)
      (with-current-buffer results
        (awc-writing-results-mode)
        (setq default-directory (file-name-directory config)
              awc-writing--source source awc-writing--tick tick awc-writing--file file))
      (awc-writing--write results (format "Writing QA running…\nRules: %s\n" config))
      (condition-case problem
          (progn
            (setq out (generate-new-buffer " *writing-stdout*")
                  err (generate-new-buffer " *writing-stderr*")
                  process (make-process
                           :name "writing-qa" :buffer out :stderr err :noquery t
                           :connection-type 'pipe :coding 'utf-8-unix
                           :command (list program "--no-global" "--no-exit" "--output=JSON"
                                          (concat "--config=" config) (concat "--path=" path))
                           :sentinel #'awc-writing--finish))
            ;; The default pipe sentinel inserts "Process … stderr finished"
            ;; into an otherwise empty stderr buffer, depending on exit order.
            (when-let* ((stderr-process (get-buffer-process err)))
              (set-process-sentinel stderr-process #'ignore)
              (set-process-query-on-exit-flag stderr-process nil))
            (setq awc-writing--process process)
            (dolist (entry `((source . ,source) (results . ,results) (stderr . ,err)
                             (tick . ,tick) (file . ,file) (path . ,path) (config . ,config)
                             (started . ,(float-time))))
              (process-put process (car entry) (cdr entry)))
            (process-put process 'timer
                         (run-at-time awc-writing-timeout nil
                                      (lambda ()
                                        (when (process-live-p process)
                                          (with-current-buffer source (awc-writing-cancel))
                                          (awc-writing--write results "Writing QA timed out. Check project rules, or increase awc-writing-timeout.\n")))))
            (process-send-string process text)
            (process-send-eof process)
            (display-buffer results))
        (error
         (when process (process-put process 'cancelled t) (delete-process process))
         (dolist (buffer (list out err)) (when (buffer-live-p buffer) (kill-buffer buffer)))
         (awc-writing--write results (format "Writing QA failed to start: %s\n" (error-message-string problem)))
         (signal (car problem) (cdr problem)))))))

;;;###autoload
(defun awc-writing-show-results ()
  "Show this document's last writing report, including any failure."
  (interactive)
  (let ((results (with-current-buffer (awc-writing--base) awc-writing--results)))
    (unless (buffer-live-p results) (user-error "No writing report; run awc-writing-check"))
    (pop-to-buffer results)))

(defalias 'awc-writing-recheck #'awc-writing-check)

;;;###autoload
(defun awc-writing-open-config ()
  "Visit this source's nearest project Vale configuration for review."
  (interactive)
  (find-file
   (with-current-buffer (awc-writing--base)
     (unless (and buffer-file-name (not (file-remote-p buffer-file-name)))
       (user-error "Visit a local prose file first"))
     (awc-writing--config buffer-file-name))))

(provide 'awc-writing)
;;; awc-writing.el ends here

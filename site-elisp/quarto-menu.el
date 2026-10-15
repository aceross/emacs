;;; quarto-menu.el --- Quarto commands and status -*- lexical-binding: t; -*-
(require 'transient)
(require 'quarto-runtime)

(defun quarto-select-format (format)
  (interactive (list (completing-read "Output format (empty for document default): " '("html" "pdf") nil nil)))
  (with-current-buffer (quarto-source-buffer)
    (setq quarto-format (unless (string-empty-p format) format) quarto-context--value nil)
    (quarto-references--schedule (current-buffer))))
(defun quarto-select-profile (profile)
  (interactive
   (list (with-current-buffer (quarto-source-buffer)
           (let* ((root (or (locate-dominating-file default-directory "_quarto.yml") (awc-project-root)))
                  (profiles (mapcar (lambda (file) (substring (file-name-base file) 8))
                                    (directory-files root nil "\\`_quarto-.+\\.ya?ml\\'"))))
             (completing-read "Profile (empty for default): " profiles nil nil)))))
  (with-current-buffer (quarto-source-buffer)
    (setq quarto-profile (unless (string-empty-p profile) profile) quarto-context--value nil)
    (quarto-references--schedule (current-buffer))))

(defun quarto-document-status ()
  "Report the resolved engine, environment and active document capabilities."
  (interactive)
  (let ((source (quarto-source-buffer)))
    (quarto-context-resolve
     (lambda (context error)
       (when (buffer-live-p source)
         (with-current-buffer source
           (let ((buffer (get-buffer-create (format "*Quarto status: %s*" (buffer-name))))
                 (text (format (concat "Document: %s\nEngine: %s\nFormat: %s\nProfile: %s\n"
                                       "Project startup: %s\nExecution directory: %s\nSelected Python: %s\n"
                                       "Runtime: %s\nRuntime identity: %S\nQueued requests: %d\n"
                                       "Language documents: %s\nLanguage assistance: %s\nYAML assistance: %s\n"
                                       "Bibliographies: %S\nEnvironment resolution: %s\n")
                               buffer-file-name (plist-get context :engine) (or quarto-format (plist-get context :format))
                               (or quarto-profile "default") (plist-get context :root) (plist-get context :directory)
                               (or (plist-get context :python) "not selected") quarto-runtime--state quarto-runtime--identity
                               (length quarto-runtime--queue) (mapcar #'car quarto-lsp--documents)
                               (or quarto-lsp-error "available when each server is initialised")
                               (or quarto-yaml-error (quarto-yaml--bundle)) quarto-authoring--bibliographies (or error "resolved"))))
             (with-current-buffer buffer
               (let ((inhibit-read-only t)) (erase-buffer) (insert text) (special-mode)
                 (setq-local quarto-document--owner source)))
             (display-buffer buffer))))))))

(defun quarto-language-restart ()
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (quarto-lsp-close) (quarto-lsp-start)))

(transient-define-prefix quarto-menu ()
  "Quarto source editing and document results."
  [["Execute"
    ("c" "Current chunk" quarto-run-chunk)
    ("n" "Chunk and advance" quarto-run-and-advance)
    ("s" "Selection / statement" quarto-run-statement)
    ("u" "Preceding chunks" quarto-run-preceding)
    ("a" "All eligible chunks" quarto-run-all)
    ("i" "Interrupt / cancel queue" quarto-interrupt)
    ("R" "Restart document runtime" quarto-restart)]
   ["Source"
    ("+" "Insert chunk" quarto-insert-chunk)
    ("]" "Next chunk" quarto-next-chunk)
    ("[" "Previous chunk" quarto-previous-chunk)
    ("m" "Select chunk" quarto-select-chunk)
    ("f" "Fold chunk" quarto-fold-chunk)
    ("o" "Outline" quarto-outline)
    ("t" "Authoring template" quarto-insert-template)]
   ["Assistance"
    ("r" "Safe code / label rename" quarto-rename)
    ("h" "Signature help" quarto-signature-help)
    ("C" "Insert citation" quarto-insert-citation)
    ("B" "Refresh bibliography" quarto-authoring-refresh-bibliographies)
    ("L" "Reconnect language documents" quarto-language-restart)
    ("Y" "Restart YAML assistance" quarto-yaml-restart)
    ("e" "Environment and status" quarto-document-status)]
   ["Results and output"
    ("v" "Results pane" quarto-results-show)
    ("d" "Inspect data frame" quarto-inspect-table)
    ("l" "Full execution log" quarto-results-log)
    ("b" "Render" quarto-build-document)
    ("p" "Live HTML preview" quarto-preview-document)
    ("P" "View rendered PDF" quarto-view-pdf)
    ("F" "Select format" quarto-select-format)
    ("g" "Select profile" quarto-select-profile)]])
(provide 'quarto-menu)

;;; quarto-health.el --- Readiness checks and targeted recovery -*- lexical-binding: t; -*-
(require 'quarto-runtime)
(require 'button)
(defvar-local quarto-health--generation 0)
(defvar-local quarto-health--report nil)
(defvar-local quarto-health--origin nil)
(defvar-local quarto-health--yaml "Not checked")
(defvar-local quarto-health--version "Not checked")
(defvar-local quarto-health--environment "Not checked")
(defvar-local quarto-health--process nil)

(defun quarto-health--language (source language)
  "Describe LANGUAGE using live process and initialisation state."
  (let ((buffer (cdr (assoc language (buffer-local-value 'quarto-lsp--documents source)))))
    (cond
     ((not (buffer-live-p buffer)) "Not connected — use Reconnect")
     ((not (fboundp 'lsp-workspaces)) "lsp-mode is unavailable")
     (t
      (with-current-buffer buffer
        (let ((workspaces (lsp-workspaces)))
          (if (not workspaces) "Disconnected — use Reconnect"
            (mapconcat
             (lambda (workspace)
               (let ((process (lsp--workspace-proc workspace))
                     (status (lsp--workspace-status workspace))
                     (capabilities (lsp--workspace-server-capabilities workspace)))
                 (cond
                  ((not (process-live-p process)) "Stopped — use Reconnect and inspect the server log")
                  ((not (eq status 'initialized)) (format "Starting (%s)" status))
                  (t (format "Ready · %s\n    Environment: %S"
                             (mapconcat (lambda (key)
                                          (let ((value (if (hash-table-p capabilities) (gethash key capabilities)
                                                         (plist-get capabilities (intern (concat ":" key))))))
                                            (format "%s %s" (string-remove-suffix "Provider" key)
                                                    (if (and value (not (eq value :json-false))) "yes" "no"))))
                                        '("completionProvider" "hoverProvider" "definitionProvider" "referencesProvider" "renameProvider") ", ")
                             quarto-lsp--environment))))) workspaces "; "))))))))

(defun quarto-health--action (label source function)
  (insert-text-button label 'follow-link t
                      'action (lambda (_button)
                                (unless (buffer-live-p source) (user-error "Source document has closed"))
                                (with-current-buffer source (funcall function))))
  (insert "  "))

(defun quarto-health--render (source)
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (buffer-live-p quarto-health--report)
        (let* ((report quarto-health--report) (origin quarto-health--origin)
               (context quarto-context--value)
               (languages (delete-dups (cl-remove-if-not
                                       (lambda (language) (member language '("r" "python")))
                                       (mapcar #'quarto-chunk-language (quarto-document-chunks (quarto-document-model))))))
               (text (format (concat "Quarto readiness · %s\n\nQuarto: %s\nEmacs: %s (%s)\n"
                                     "\nCompletion at the checked position: %s\n"
                                     "YAML schema probe: %s\nLast document YAML error: %s\n"
                                     "\nEnvironment: %s\nEngine: %s · Format: %s · Profile: %s\n"
                                     "Project startup: %s\nExecution directory: %s\nPython: %s\n"
                                     "\nRuntime: %s · Process: %s · Queued: %d\nRuntime identity: %S\n"
                                     "\nReferences: %d citation keys, %d labels\nBibliographies: %S\nReference status: %s\n\n")
                             (or buffer-file-name (buffer-name)) quarto-health--version emacs-version window-system
                             (plist-get origin :description) quarto-health--yaml (or quarto-yaml-error "none")
                             quarto-health--environment (or (plist-get context :engine) "unresolved")
                             (or quarto-format (plist-get context :format) "document default") (or quarto-profile "default")
                             (or (plist-get context :root) "unresolved") (or (plist-get context :directory) "unresolved")
                             (or (plist-get context :python) "not selected") quarto-runtime--state
                             (if (process-live-p quarto-runtime--process) "running" "not running")
                             (length quarto-runtime--queue) quarto-runtime--identity
                             (length quarto-authoring--citations) (length (quarto-document-labels (quarto-document-model)))
                             quarto-authoring--bibliographies (or quarto-references-error "available")))
               (language-text (mapconcat (lambda (language) (format "%s: %s" (upcase language) (quarto-health--language source language))) languages "\n"))
               (language-error quarto-lsp-error))
          (with-current-buffer report
            (let ((inhibit-read-only t) (position (point)))
              (erase-buffer) (insert text language-text "\n")
              (when language-error (insert "Last language assistance error: " language-error "\n"))
              (insert "\nRecovery (document code is never executed by these actions):\n")
              (quarto-health--action "Check again" source #'quarto-document-status)
              (quarto-health--action "Repair completion hooks" source #'quarto-repair-completion)
              (insert "\n")
              (quarto-health--action "Restart shared YAML helper" source
                                    (lambda () (quarto-yaml-restart) (quarto-document-status)))
              (quarto-health--action "YAML log" source (lambda () (pop-to-buffer (get-buffer-create "*Quarto editor log*"))))
              (insert "\n")
              (dolist (language languages)
                (quarto-health--action (concat "Reconnect " (upcase language)) source
                                      (lambda () (quarto-language-restart language) (quarto-document-status))))
              (quarto-health--action "Language logs" source #'quarto-language-logs)
              (insert "\n")
              (quarto-health--action "Refresh references" source
                                    (lambda () (quarto-authoring-refresh-bibliographies) (quarto-document-status)))
              (quarto-health--action "Inspect log" source
                                    (lambda () (pop-to-buffer (get-buffer-create (format "*Quarto inspect: %s*" (buffer-name))))))
              (insert "\n\nMissing dependencies must be installed explicitly. Runtime restart remains C-c q R.\n")
              (goto-char (min position (point-max))))))))))

(defun quarto-health--update (source generation variable value)
  "Ignore checks belonging to an older report or a closed document."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (= generation quarto-health--generation)
        (set variable value)
        (quarto-health--render source)))))

(defun quarto-health--check-version (source generation)
  (condition-case err
      (let* ((executable (or (executable-find "quarto") (error "Quarto executable is missing")))
             (buffer (generate-new-buffer " *Quarto version*"))
             (process
              (make-process :name "quarto-version" :buffer buffer :noquery t
                            :command (list executable "--version")
                            :sentinel
                            (lambda (process _event)
                              (when (memq (process-status process) '(exit signal))
                                (when-let* ((timer (process-get process 'quarto-timeout))) (cancel-timer timer))
                                (unwind-protect
                                    (quarto-health--update
                                     source generation 'quarto-health--version
                                     (if (= (process-exit-status process) 0)
                                         (string-trim (with-current-buffer buffer (buffer-string)))
                                       "Version check failed or timed out; check the Quarto executable"))
                                  (kill-buffer buffer)))))))
        (setq quarto-health--process process)
        (process-put process 'quarto-timeout
                     (run-at-time 5 nil (lambda () (when (process-live-p process) (delete-process process))))))
    (error (quarto-health--update source generation 'quarto-health--version (error-message-string err)))))

(defun quarto-document-status ()
  "Check actual readiness asynchronously without running document code."
  (interactive)
  (let* ((source (quarto-source-buffer))
         (yaml (quarto-yaml-position-p))
         (description
          (format "%s · mode %s · Corfu %s · automatic %s · CAPF %s\n    %s"
                  (if yaml "YAML" (if (quarto-chunk-at) "code" "prose/references")) major-mode
                  (if (bound-and-true-p corfu-mode) "on" "off") (if (bound-and-true-p corfu-auto) "on" "off")
                  (if (memq (if yaml #'quarto-yaml-completion-at-point #'quarto-reference-completion-at-point)
                            completion-at-point-functions) "installed" "missing — repair completion hooks")
                  (if yaml "Type two characters; use M-x completion-at-point for an empty prefix."
                    "YAML suggestions appear only inside front matter or #| options."))))
    (with-current-buffer source
      (let ((generation (cl-incf quarto-health--generation)))
        (setq quarto-health--origin (list :description description)
              quarto-health--yaml "Checking…" quarto-health--version "Checking…"
              quarto-health--environment "Checking…"
              quarto-health--report (get-buffer-create (format "*Quarto status: %s*" (buffer-name))))
        (with-current-buffer quarto-health--report
          (special-mode) (setq-local quarto-document--owner source))
        (quarto-health--render source)
        (display-buffer quarto-health--report)
        (when (process-live-p quarto-health--process) (delete-process quarto-health--process))
        (quarto-health--check-version source generation)
        (quarto-yaml-request
         "health" nil
         (lambda (value error)
           (quarto-health--update source generation 'quarto-health--yaml
                                 (if error (concat "Unavailable: " error " — inspect YAML log, then restart the helper")
                                   (format "Ready · Node %s · %s" (quarto-get value "node") (quarto-get value "bundle"))))))
        (condition-case err
            (if (and buffer-file-name (file-readable-p buffer-file-name))
                (quarto-context-resolve
                 (lambda (_context error)
                   (quarto-health--update source generation 'quarto-health--environment
                                         (or error "Resolved (runtime starts only on execution)"))))
              (quarto-health--update source generation 'quarto-health--environment
                                    "Save once for project resolution; schema completion works before saving"))
          (error (quarto-health--update source generation 'quarto-health--environment (error-message-string err))))))))

(defun quarto-repair-completion ()
  "Reinstall Quarto completion locally, without restarting language/runtime state."
  (interactive)
  (let ((source (quarto-source-buffer)))
    (dolist (buffer (buffer-list))
      (when (or (eq buffer source) (eq (buffer-base-buffer buffer) source))
        (with-current-buffer buffer
          (if (quarto-source-p) (quarto-editing-setup) (quarto-project-yaml-setup))
          (corfu-mode 1) (setq-local corfu-auto t))))
    (with-current-buffer source (quarto-document-status))))

(defun quarto-language-logs ()
  "Open an existing language log without starting or stopping any server."
  (interactive)
  (let ((buffers (cl-remove-if-not (lambda (buffer) (string-match-p "quarto-.*stderr" (buffer-name buffer))) (buffer-list))))
    (unless buffers (user-error "No Quarto language server log yet; reconnect the affected language"))
    (pop-to-buffer (completing-read "Language log: " (mapcar #'buffer-name buffers) nil t))))

(provide 'quarto-health)

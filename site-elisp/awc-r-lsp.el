;;; awc-r-lsp.el --- R language servers in ESS project environments -*- lexical-binding: t; -*-
(require 'awc-r)

(defcustom awc-r-lsp-auto-start t
  "Start R language servers after file and directory local settings.
Only standalone R source files are eligible.  No packages are installed."
  :type 'boolean :group 'ess)
(put 'awc-r-lsp-auto-start 'safe-local-variable #'booleanp)

(defvar lsp-auto-guess-root)
(defvar lsp-enable-suggest-server-download)
(defvar lsp-enabled-clients)
(defvar lsp-clients)
(defvar lsp-completion--cache)
(defvar lsp-completion--last-result)
(defvar-local awc-r-lsp-environment-error nil)
(declare-function lsp--workspace-metadata "lsp-mode" (workspace))
(declare-function lsp--workspace-root "lsp-mode" (workspace))
(declare-function lsp--workspace-client "lsp-mode" (workspace))
(declare-function lsp--client-server-id "lsp-mode" (client))
(declare-function lsp--session-workspaces "lsp-mode" (session))
(declare-function lsp-session "lsp-mode" ())
(declare-function lsp-stdio-connection "lsp-mode" (command &optional test-command))
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function make-lsp-client "lsp-mode" (&rest arguments))
(declare-function lsp-deferred "lsp-mode" ())

(defun awc-r-lsp--source-p ()
  "Whether this buffer is a standalone local R source."
  (and (awc-r--source-p) (not (buffer-base-buffer))
       (string-match-p "\\.[rR]\\'" buffer-file-name)))

(defun awc-r-lsp--command ()
  "Build a server command using the same R executable as ESS.
Normal R startup reads .Renviron and .Rprofile, including renv activation."
  (list (nth 2 (awc-r--selection)) "--slave" "--no-save" "--no-restore"
        "-e"
        (concat "if (!requireNamespace('languageserver', quietly = TRUE)) "
                "stop('R LSP: languageserver is unavailable in this project. "
                "Install it explicitly with install.packages(\"languageserver\") "
                "or renv::install(\"languageserver\"), then retry C-c w L.', "
                "call. = FALSE); languageserver::run()")))

(defun awc-r-lsp--check-workspace (workspace)
  "Refuse to attach this buffer to WORKSPACE with a different environment."
  (unless (equal (car (awc-r--selection))
                 (gethash 'awc-r-selection (lsp--workspace-metadata workspace)))
    (user-error "R LSP already uses another environment here; stop that workspace with M-x lsp-workspace-shutdown, then C-c w L")))

(defun awc-r-lsp--connect (filter sentinel name environment-fn workspace)
  "Launch WORKSPACE with FILTER, SENTINEL, NAME and ENVIRONMENT-FN."
  (let* ((selection (awc-r--selection))
         (root (nth 1 selection))
         (default-directory root)
         (process-environment (append (awc-project-environment (nth 3 selection))
                                      process-environment))
         (exec-path (append (split-string (getenv "PATH") path-separator t)
                            (list exec-directory))))
    (unless (equal (file-truename (file-name-as-directory (lsp--workspace-root workspace)))
                   root)
      (user-error "R LSP root differs from ESS root %s; remove the stale LSP folder and retry" root))
    (puthash 'awc-r-selection (car selection) (lsp--workspace-metadata workspace))
    ;; lsp-stdio-connection derives stderr buffer names from NAME before
    ;; make-process uniquifies it.  Give each project its own readable log.
    (setq name (awc-project-process-name name (concat (nth 2 selection) "\0" (nth 3 selection))))
    (funcall (plist-get (lsp-stdio-connection (awc-r-lsp--command)) :connect)
             filter sentinel name environment-fn workspace)))

(defun awc-r-lsp--register ()
  "Register the project client only when LSP is requested."
  (require 'lsp-mode)
  (unless (gethash 'awc-r lsp-clients)
    (lsp-register-client
     (make-lsp-client
      :new-connection (list :connect #'awc-r-lsp--connect
                            :test? (lambda () (executable-find (nth 2 (awc-r--selection)))))
      :major-modes '(ess-r-mode)
      :server-id 'awc-r
      :multi-root nil
      :before-file-open-fn #'awc-r-lsp--check-workspace))))

(defun awc-r-start-lsp ()
  "Start or reuse this project's R language server.
Install languageserver explicitly in the project's R library first.
Use M-x lsp-workspace-restart after changing its packages or R profiles."
  (interactive)
  (unless (awc-r-lsp--source-p)
    (user-error "R LSP is enabled for local .R files; Quarto chunks use ESS completion"))
  (awc-r-lsp--register)
  (let ((root (nth 1 (awc-r--selection))))
    (dolist (workspace (lsp--session-workspaces (lsp-session)))
      (when (and (eq (lsp--client-server-id (lsp--workspace-client workspace)) 'awc-r)
                 (equal root (file-name-as-directory (lsp--workspace-root workspace))))
        (awc-r-lsp--check-workspace workspace))))
  (setq-local lsp-auto-guess-root t
              lsp-enable-suggest-server-download nil
              lsp-enabled-clients '(awc-r)
              awc-r-lsp-environment-error nil)
  ;; This lsp-mode version caches CAPF results globally by prefix position.
  ;; Switching projects at the same column can reuse another buffer's items.
  (awc-r-lsp--isolate-completion)
  (add-hook 'lsp-completion-mode-hook #'awc-r-lsp--isolate-completion nil t)
  (lsp-deferred))

(defun awc-r-lsp--isolate-completion ()
  "Keep R completion caches in the buffer owning the server connection."
  (setq-local lsp-completion--cache nil lsp-completion--last-result nil))

(defun awc-r-lsp-after-locals ()
  "Start R LSP after local settings, when enabled for this source."
  (when (and awc-r-lsp-auto-start (awc-r-lsp--source-p))
    (condition-case err (awc-r-start-lsp)
      (user-error
       (setq-local awc-r-lsp-environment-error (error-message-string err))
       (message "R LSP: %s" awc-r-lsp-environment-error)))))

(defun awc-r-lsp-prepare-buffer ()
  "Defer server selection until file and directory locals have been read."
  (when (awc-r-lsp--source-p)
    ;; Both are needed: files without local settings skip the second hook.
    (add-hook 'find-file-hook #'awc-r-lsp-after-locals nil t)
    (add-hook 'hack-local-variables-hook #'awc-r-lsp-after-locals nil t)))

(provide 'awc-r-lsp)
;;; awc-r-lsp.el ends here

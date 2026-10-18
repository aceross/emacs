;;; awc-python.el --- Project Python execution -*- lexical-binding: t; -*-
(require 'awc-project)
(require 'python)

(defvar-local awc-python-managed nil)
(defvar-local awc-python-environment-error nil)
(defvar-local awc-python-selected nil)
(defvar lsp-pyright-python-search-functions)
(defvar lsp-pyright-python-executable-cmd)
(defvar lsp-auto-guess-root)
(defvar lsp-enable-suggest-server-download)
(defvar lsp-enabled-clients)
(defvar org-babel-python-command)
(declare-function lsp-deferred "lsp-mode")

(defun awc-python-configure ()
  "Configure this buffer from the shared interpreter selection."
  (let ((python (awc-project-python)))
    (setq-local awc-python-selected python
                awc-python-environment-error nil
                python-shell-interpreter python
                python-shell-interpreter-args "-i"
                python-shell-buffer-name (awc-project-process-name "Python" python)
                python-shell-dedicated nil
                python-shell-virtualenv-root nil
                python-shell-process-environment (awc-project-environment python)
                python-shell-exec-path (list (file-name-directory python))
                lsp-pyright-python-executable-cmd python
                lsp-pyright-python-search-functions '(awc-project-python))
    ;; Flycheck and deferred language-server startup read buffer-local values.
    (let ((keys (mapcar (lambda (entry) (car (split-string entry "=")))
                        python-shell-process-environment)))
      (setq-local process-environment
                  (append python-shell-process-environment
                          (cl-remove-if
                           (lambda (entry) (member (car (split-string entry "=")) keys))
                           process-environment))))
    (setq-local exec-path
                (append (split-string (getenv "PATH") path-separator t)
                        (list exec-directory)))
    python))

(defun awc-python-prepare-buffer ()
  "Prepare isolated execution without running user code on file open."
  (when (with-current-buffer (awc-project-source-buffer) buffer-file-name)
    (setq-local awc-python-managed t)
    (setq-local python-shell-buffer-name (awc-project-process-name "Python unavailable" nil))
    (condition-case err (awc-python-configure)
      (user-error (setq-local awc-python-environment-error (error-message-string err))))
    ;; Files without local settings skip hack-local-variables-hook.
    (add-hook 'find-file-hook #'awc-python-after-locals nil t)
    (add-hook 'hack-local-variables-hook #'awc-python-after-locals nil t)))

(defun awc-python-after-locals ()
  "Apply directory locals before configuring or starting Pyright."
  (condition-case err
      (progn (awc-python-configure)
             (unless (buffer-base-buffer) (awc-python-start-lsp)))
    (user-error
     (setq-local awc-python-environment-error (error-message-string err))
     (message "Python: %s (C-c w e for details)" awc-python-environment-error))))

(defun awc-python-before-command (&rest _)
  "Revalidate managed buffers before native Python command/process lookup."
  (when awc-python-managed (awc-python-configure)))

(defun awc-python-get-buffer (original &rest arguments)
  "Find a managed REPL with ORIGINAL and ARGUMENTS, or nil if unavailable.
Eldoc also performs process lookup, so missing environments must not signal
on every cursor move.  Starting/evaluating still fails explicitly."
  (if (not awc-python-managed) (apply original arguments)
    (condition-case err
        (progn (awc-python-configure) (apply original arguments))
      (user-error
       (setq-local awc-python-environment-error (error-message-string err))
       nil))))

;; Native evaluation commands share the project process.  Preserve all output.
(advice-add 'python-shell-calculate-command :before #'awc-python-before-command)
(advice-add 'python-shell-get-buffer :around #'awc-python-get-buffer)

(defun awc-python-run-at-root (original &rest arguments)
  "Start native Python ORIGINAL with ARGUMENTS at the project root."
  (if (not awc-python-managed) (apply original arguments)
    (awc-python-configure)
    (let ((default-directory (awc-project-root))) (apply original arguments))))
(advice-add 'run-python :around #'awc-python-run-at-root)

(defun awc-python-run (&optional restart)
  "Show the project REPL; with RESTART discard that REPL's state."
  (interactive "P")
  (let* ((python (awc-python-configure))
         (default-directory (awc-project-root))
         (name (format "*%s*" python-shell-buffer-name))
         (process (get-buffer-process name)))
    ;; Native run-python reinitializes an ended REPL's major mode. Dispose
    ;; its hidden fontification buffer before that loses the local reference.
    (when (or restart (not (process-live-p process)))
      (when-let* ((buffer (get-buffer name)))
        (with-current-buffer buffer (python-shell-font-lock-kill-buffer))))
    (when (and restart (process-live-p process)) (delete-process process))
    (setq process (run-python nil nil nil))
    ;; Emacs 31 sends its evaluation helpers on the first prompt.  Sending a
    ;; chunk before that prompt races the bootstrap (__PYTHON_EL_eval).
    (let ((deadline (+ (float-time) 5)))
      (while (and (process-live-p process)
                  (not (buffer-local-value 'python-shell--first-prompt-received
                                           (process-buffer process)))
                  (< (float-time) deadline))
        (accept-process-output process 0.05))
      (unless (and (process-live-p process)
                   (buffer-local-value 'python-shell--first-prompt-received
                                       (process-buffer process)))
        (user-error "Python has not reached its first prompt; inspect %s and retry" name)))
    (with-current-buffer (process-buffer process)
      (setq-local awc-project-python python)
      (setq-local header-line-format (concat " Python: " python)))
    (display-buffer (process-buffer process))
    process))

(defun awc-python-send-region-or-line (start end)
  "Send region START END, or the current line, to the project's Python."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (awc-python-run)
  (python-shell-send-region start end))

(defun awc-python-interrupt ()
  "Interrupt this project's running Python command."
  (interactive)
  (awc-python-configure)
  (let ((process (get-process python-shell-buffer-name)))
    (unless (process-live-p process) (user-error "No Python REPL for this environment"))
    (interrupt-process process)))

(defun awc-python-inspect (expression)
  "Print EXPRESSION in the project's REPL.
For a dataframe, enter df.head(), df.info() or df.describe()."
  (interactive (list (read-string "Python expression: " (thing-at-point 'symbol t))))
  (python-shell-send-string (format "print(%s)" expression) (awc-python-run)))

(defun awc-python-test (arguments)
  "Run the selected Python's pytest with shell-quoted ARGUMENTS."
  (interactive (list (read-string "pytest arguments: " "-q")))
  (awc-project-compile
   (mapconcat #'shell-quote-argument
              (append (list (awc-project-python) "-m" "pytest")
                      (awc-project-split-arguments arguments)) " ") t))

(defun awc-python-start-lsp ()
  "Start Pyright after environment resolution without downloading a server.
Indirect Quarto chunks use REPL completion.  A workspace must use a single
interpreter; conflicting nested environments produce an explanatory error."
  (interactive)
  (when (buffer-base-buffer)
    (user-error "Python LSP is enabled for .py files; chunks use REPL completion"))
  (let* ((python (awc-python-configure)) (root (awc-project-root)))
    (unless (executable-find "pyright-langserver")
      (user-error "pyright-langserver is missing; install it explicitly to enable Python LSP"))
    (require 'lsp-pyright)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p lsp-mode) awc-python-selected
                   (equal root (awc-project-root))
                   (not (equal python awc-python-selected)))
          (user-error "Pyright already uses another Python in %s; stop that workspace first" root))))
    (setq-local lsp-auto-guess-root t lsp-enable-suggest-server-download nil
                lsp-enabled-clients '(pyright))
    (lsp-deferred)))

(defun awc-python-babel (original body params)
  "Run non-session Org Python BODY and PARAMS in the project environment.
Explicit :python headers and named sessions retain Org's existing behavior."
  (let ((session (cdr (assq :session params))))
    (if (or (assq :python params) (and session (not (equal session "none"))))
        (funcall original body params)
      (let* ((python (awc-project-python))
             (org-babel-python-command (shell-quote-argument python))
             (process-environment (append (awc-project-environment python) process-environment)))
        (funcall original body params)))))

(with-eval-after-load 'ob-python
  (advice-add 'org-babel-execute:python :around #'awc-python-babel))

(provide 'awc-python)
;;; awc-python.el ends here

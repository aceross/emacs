;;; awc-lsp.el --- Language servers for new file buffers -*- lexical-binding: t; -*-

(defvar-local awc-lsp--pending-arguments nil
  "Arguments for an LSP start waiting for the file's directory.")
(declare-function lsp "lsp-mode" (&optional arg))
(defvar lsp-enabled-clients)
(defvar lsp-disabled-clients)
(defvar lsp-enable-suggest-server-download)

(defun awc-lsp-start-prose ()
  "Explicitly start LTeX for this prose buffer, without other servers.
Notes and citations do not need this optional grammar checker.  Quarto
buffers manage their own language servers and are excluded."
  (interactive)
  (unless (and (derived-mode-p 'text-mode)
               buffer-file-name
               (not (buffer-base-buffer))
               (not (string-match-p "\\.qmd\\'\\|/_quarto\\(?:-[^.]+\\)?\\.ya?ml\\'"
                                    buffer-file-name)))
    (user-error "Visit a prose file outside Quarto to start grammar checking"))
  (require 'lsp-ltex)
  (setq-local lsp-enabled-clients '(ltex-ls)
              lsp-disabled-clients (remq 'ltex-ls (copy-sequence lsp-disabled-clients))
              lsp-enable-suggest-server-download nil)
  (lsp))

(defun awc-lsp--after-save ()
  "Retry the pending LSP start after saving has created the directory."
  (let ((arguments awc-lsp--pending-arguments))
    (remove-hook 'after-save-hook #'awc-lsp--after-save t)
    (setq awc-lsp--pending-arguments nil)
    (apply #'lsp arguments)))

(defun awc-lsp--wait-for-directory (original &rest arguments)
  "Call ORIGINAL with ARGUMENTS once a local file's directories exist.
Guard the actual LSP entry point so already queued startup timers are
covered too.  Saving is responsible for creating the directories."
  (if (and buffer-file-name
           (not (file-remote-p buffer-file-name))
           (not (file-remote-p default-directory))
           (or (not (file-directory-p default-directory))
               (not (file-directory-p (file-name-directory buffer-file-name)))))
      (progn
        (setq awc-lsp--pending-arguments arguments)
        (add-hook 'after-save-hook #'awc-lsp--after-save nil t))
    (remove-hook 'after-save-hook #'awc-lsp--after-save t)
    (setq awc-lsp--pending-arguments nil)
    (apply original arguments)))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp :around #'awc-lsp--wait-for-directory))

(provide 'awc-lsp)
;;; awc-lsp.el ends here

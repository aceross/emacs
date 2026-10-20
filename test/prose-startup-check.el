;;; prose-startup-check.el --- Notes do not start language servers -*- lexical-binding: t; -*-
;; Run in a fresh -Q Emacs with the existing packages; never download/rebuild.
(setq user-emacs-directory
      (file-name-directory
       (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
      straight-use-package-by-default t
      straight-check-for-modifications nil)
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory) nil t)
(setq straight-safe-mode t)
(straight-use-package 'use-package)
(require 'use-package)
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))
(require 'init-lsp)
(straight-use-package 'markdown-mode)
(require 'markdown-mode)
(require 'lsp-mode)
(unless (memq 'ltex-ls lsp-disabled-clients)
  (error "LTeX should be disabled until explicitly requested"))
(cl-letf (((symbol-function 'lsp)
           (lambda (&rest _) (error "Opening a note must not start LSP")))
          ((symbol-function 'lsp-deferred)
           (lambda (&rest _) (error "Opening a note must not queue LSP"))))
  (dolist (mode '(text-mode markdown-mode org-mode))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/prose-startup-note.md")
      (funcall mode))))
(when (featurep 'lsp-ltex)
  (error "Opening notes should not load the optional LTeX client"))
(when (featurep 'awc-writing)
  (error "Opening prose should not load or start the explicit Vale adapter"))
(when (featurep 'awc-paper)
  (error "Opening prose should not load the explicit manuscript runner"))
(princ "PASS: text, Markdown and Org modes do not start or queue LSP; LTeX requires explicit opt-in.\n")

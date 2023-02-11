(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :hook
  ((python-mode . lsp)
   (ess-mode    . lsp)
   (LaTeX-mode  . lsp)
   (c-mode      . lsp)
   (c++-mode    . lsp)
   (lsp-mode    . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook
  ((lsp-mode . lsp-ui-mode))
  :bind
  (("C-h ." . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-header t)
  (lsp-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-position 'bottom)
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ;

(provide 'init-lsp)

;;; init-lsp.el ends here.

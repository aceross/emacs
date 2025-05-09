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
   (julia-mode  . lsp)
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
  ;; (lsp-ui-header t)
  ;; (lsp-eldoc-enable-hover t)
  (lsp-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-enable t)
  ;; (lsp-ui-doc-position 'bottom)
  )

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
  )

(use-package lsp-julia)

(use-package lsp-ltex
  :after lsp-mode
  :ensure t
  :custom
  (lsp-ltex-language "en-GB")
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  ;; :init
  ;; (setq lsp-ltex-version "18.4.0")
  )  ; make sure you have set this

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets)

(provide 'init-lsp)

;;; init-lsp.el ends here.

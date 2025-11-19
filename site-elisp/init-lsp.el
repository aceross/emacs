;;; init-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package lsp-mode
  :init
  ;; Prefix for `lsp-command-map'
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil          ;; we use yasnippet directly
        lsp-prefer-flymake nil          ;; use flycheck instead
        lsp-idle-delay 0.3)
  :hook
  ;; Let lsp-pyright handle python-mode; don't hook python here.
  ((ess-r-mode . lsp-deferred)
   (LaTeX-mode . lsp-deferred)
   (c-mode     . lsp-deferred)
   (c++-mode   . lsp-deferred)
   (julia-mode . lsp-deferred)
   (lsp-mode   . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-h ." . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-enable t))

;; Python: use Pyright
(use-package lsp-pyright
  :after lsp-mode
  :ensure t
  :custom
  ;; adjust if needed
  (lsp-pyright-langserver-command "pyright")
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

;; Julia
(use-package lsp-julia
  :after lsp-mode)

;; Grammar / spell / style checking via LTeX
(use-package lsp-ltex
  :after lsp-mode
  :ensure t
  :custom
  (lsp-ltex-language "en-GB")
  :hook
  (text-mode . (lambda ()
                 (require 'lsp-ltex)
                 (lsp-deferred))))

;; Snippets
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

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-lsp)

;;; init-lsp.el ends here.

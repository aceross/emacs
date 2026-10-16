;;; init-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'awc-lsp)

(use-package lsp-mode
  :init
  ;; Prefix for `lsp-command-map'
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil          ;; we use yasnippet directly
        lsp-prefer-flymake nil          ;; use flycheck instead
        lsp-idle-delay 0.3)
  :hook
  ;; Python and R start after directory locals select their environments.
  ;; Their project helpers exclude indirect Quarto chunks.
  ((LaTeX-mode . lsp-deferred)
   (c-mode     . lsp-deferred)
   (c++-mode   . lsp-deferred)
   (julia-mode . lsp-deferred)
   (lsp-mode   . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  ;; This installed optional client calls (cl-remove-if #'nil ...), aborting
  ;; ALL first LSP starts.  Upstream now uses #'null.  Keep it out of automatic
  ;; loading until a deliberate package update; query-language editing is unused.
  (setq lsp-client-packages (remq 'lsp-ts-query lsp-client-packages))
  ;; Grammar checking is optional; starting a coding server must not also
  ;; start LTeX.  `awc-lsp-start-prose' enables it only in the chosen buffer.
  (add-to-list 'lsp-disabled-clients 'ltex-ls))

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
  :commands lsp-pyright-locate-python
  :custom
  (lsp-pyright-langserver-command "pyright")
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-multi-root nil))
;; awc-python starts Pyright after directory locals, including on the first file.

;; Julia
(use-package lsp-julia
  :after lsp-mode)

;; Optional grammar checking.  Plain notes use Flyspell and Citar without LSP.
(use-package lsp-ltex
  :defer t
  :ensure t
  :custom
  (lsp-ltex-language "en-GB"))

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

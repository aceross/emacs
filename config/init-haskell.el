;;; init-haskell.el --- Configurations for Haskell
;;
;;; Commentary:
;;
;;; Code:

;; lifted in large part from https://writequit.org/org/settings.html#sec-1-4-9
(use-package haskell-mode
  :defer t
  :init
  (progn
    (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook #'subword-mode)
    (add-hook 'haskell-mode-hook 'company-mode))
  :config
  (use-package hindent
    :ensure t
    :config
    (add-hook 'haskell-mode-hook #'hindent-mode))
  (use-package company-ghc
    :ensure t
    :config
    (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
    (custom-set-variables '(company-ghc-show-info t)))
  (progn
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
      (add-to-list 'exec-path my-cabal-path))
    (custom-set-variables '(haskell-tags-on-save t))

    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))
    (define-key haskell-mode-map (kbd "C-c C-l")
      'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z")
      'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t")
      'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i")
      'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c")
      'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c")
      'haskell-process-cabal)

    (eval-after-load 'haskell-cabal
      '(progn
         (define-key haskell-cabal-mode-map (kbd "C-c C-z")
           'haskell-interactive-switch)
         (define-key haskell-cabal-mode-map (kbd "C-c C-k")
           'haskell-interactive-mode-clear)
         (define-key haskell-cabal-mode-map (kbd "C-c C-c")
           'haskell-process-cabal-build)
         (define-key haskell-cabal-mode-map (kbd "C-c c")
           'haskell-process-cabal)))

    (custom-set-variables '(haskell-process-type 'cabal-repl))

    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

(provide 'init-haskell)

;;; init-haskell ends here

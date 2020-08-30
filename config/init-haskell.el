;;; init-haskell.el --- Configurations for Haskell
;;
;;; Commentary:
;;
;;; Code:

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
    (use-package intero
      :ensure t
      :config
      (progn
        (add-hook 'haskell-mode-hook 'intero-mode)))
    (use-package hindent
      :ensure t
      :config
      (progn
        (add-hook 'haskell-mode-hook #'hindent-mode)))))

(provide 'init-haskell)

;;; init-haskell ends here

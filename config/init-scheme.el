;;; init-scheme.el --- Initialise Scheme
;;
;;; Commentary:
;;
;;; Code:

(use-package geiser
  :ensure t
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode))

(provide 'init-scheme)

;;; init-scheme.el ends here

;;; init-sml.el --- Set-up for SML editing
;;;
;;; Commentary:
;;;
;;; Code:

(use-package sml-mode
  :defer t)

(use-package company-sml
  :config
  (add-hook 'company-sml 'company-sml-setup))

(provide 'init-sml)

;;; init-sml.el ends here

;;; init-cc.el --- Module for C/C++ mode customisations
;;
;;; Commentary:
;;

;;; Code:

;; C/C++ programming
(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 4
          c-basic-offset 2
          gdb-many-windows t)))

(use-package irony
  :ensure t
  :defer t
  :diminish irony-mode
  :init
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'eldoc-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (custom-set-variables
   '(irony-additional-clang-options
     '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))
  (use-package company-irony
    :ensure t
    :config (add-to-list 'company-backends '(company-irony)))
  (use-package flycheck-irony
    :ensure t
    :config (flycheck-irony-setup))
  (use-package company-irony-c-headers
    :ensure t
    :config (add-to-list 'company-backends '(company-irony-c-headers)))
  (use-package irony-eldoc
    :ensure t
    :config (add-hook 'irony-mode-hook 'irony-eldoc)))

;; Shows an inline arguments hint for the C/C++ function at point
;; https://github.com/abo-abo/function-args
(use-package function-args
  :ensure t
  :config
  (fa-config-default)
  :bind
  (:map c-mode-map
        ("M-o" . fa-show))
  (:map c++-mode-map
        ("M-o" . fa-show)))

(provide 'init-cc)

;;; init-cc.el ends here

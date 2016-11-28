;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;

;;; Code:

(use-package python :ensure t)

(use-package company-jedi
  :ensure t
  :config (add-to-list 'company-backends 'company-jedi))


(use-package elpy
  :ensure t
  :config
  (progn
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (setq elpy-rpc-backend "jedi"
	  elpy-rpc-python-command "python3")
    (setq py-python-command "/usr/bin/python3")
    (setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")
    (elpy-enable)
    (elpy-use-ipython)
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))

(use-package flycheck-pyflakes
    :ensure t
    :config (add-hook 'python-mode-hook 'flycheck-mode))

(use-package py-autopep8
  :ensure t
  :defer t
  :config (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; ;;; Python
;; (elpy-enable)
;; (setq elpy-rpc-backend "jedi")
;; (elpy-use-ipython)
;; (setq elpy-rpc-python-command "python3")
;; (setq python-shell-interpreter "ipython3"
;;       python-shell-interpreter-args "--simple-prompt --pprint")

;; ;; use flycheck and not flymake
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (require 'flycheck-pyflakes)
;; (add-hook 'python-mode-hook 'flycheck-mode)

;; ;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide 'init-python)

;;; init-python.el ends here

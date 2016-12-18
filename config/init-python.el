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
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (setq py-python-command "/usr/bin/python3")
    (setq python-shell-interpreter "ipython3"
	  python-shell-interpreter-args "--simple-prompt --pprint")
    (elpy-enable)
    (elpy-use-ipython)
    (add-hook 'elpy-mode-hook 'elpy-use-ipython "ipython3")
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")
    ))

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;   (setq elpy-rpc-backend "jedi"
;;      elpy-rpc-python-command "python3")
;;   (setq py-python-command "/usr/bin/python3")
;;   (setq python-shell-interpreter "ipython3"
;;      python-shell-interpreter-args "--simple-prompt --pprint")
;;   (elpy-use-ipython)
;;   (add-hook 'elpy-mode-hook 'elpy-use-ipython "ipython3")
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   (elpy-enable)
;;   )

(use-package flycheck-pyflakes
    :ensure t
    :config (add-hook 'python-mode-hook 'flycheck-mode))

(use-package py-autopep8
  :ensure t
  :defer t
  :config (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(provide 'init-python)

;;; init-python.el ends here
;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;
;;   Interactive python session and completion.
;;
;;; Code:

(use-package python :ensure t)

(use-package company-jedi
  :ensure t
  :defer t
  :config (add-to-list 'company-backends 'company-jedi))

(use-package elpy
  :ensure t
  :defer t
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    ;; Default to Python 3 instead of Python 2
    (setq elpy-rpc-python-command "python3")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt -i")
    (add-hook 'elpy-mode-hook 'elpy-use-ipython "ipython3")
    (setq elpy-rpc-backend "jedi")
    (elpy-enable)
    ))

(use-package flycheck-pyflakes
  :ensure t
  :defer t
  :config (add-hook 'python-mode-hook 'flycheck-mode))

(use-package py-autopep8
  :ensure t
  :defer t
  :config (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(provide 'init-python)

;;; init-python.el ends here

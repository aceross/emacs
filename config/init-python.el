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

;; (use-package elpy
;;   :ensure t
;;   :config
;;   ;; Use Flycheck instead of Flymake
;;   (when (require 'flycheck nil t)
;;     (remove-hook 'elpy-modules 'elpy-module-flymake)
;;     (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;     (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   (elpy-enable)
;;   (setq python-shell-interpreter "ipython")
;;   ;; (setq python-shell-interpreter "ipython3")
;;   (setq python-shell-interpreter-args "--simple-prompt")
;;   (setq elpy-rpc-backend "jedi")
;;   (setq jedi:complete-on-dot t)
;;   )

; from http://people.duke.edu/~aql3/emacs-there-and-back-again/
(use-package elpy
  :ensure t
  :config
;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (elpy-enable)
  ;; Enable elpy in a Python mode
  (add-hook 'python-mode-hook 'elpy-mode)
  ; (setq py-python-command "/usr/bin/python3")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (setq jedi:complete-on-dot t)
  ;; Open the Python shell in a buffer after sending code to it
  (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
  ;; Use IPython as the default shell, with a workaround to accommodate IPython 5
  ;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  ;; Enable pyvenv, which manages Python virtual environments
  (pyvenv-mode 1)
  ;; Tell Python debugger (pdb) to use the current virtual environment
  ;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
  (setq gud-pdb-command-name "python -m pdb "))

;; jupyter notebook integration
(use-package ein)

;; cell-like objects in python
(use-package python-x
  :ensure t
  :config
  (python-x-setup))

(use-package flycheck-pyflakes
  :ensure t
  :defer t
  :config (add-hook 'python-mode-hook 'flycheck-mode))

(use-package py-autopep8
  :ensure t
  :config (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(provide 'init-python)

;;; init-python.el ends here

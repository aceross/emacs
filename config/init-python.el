;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;
;;   Interactive python session and completion.
;;
;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4))

(use-package company-jedi
  :ensure t
  :defer t
  :config (add-to-list 'company-backends 'company-jedi))

(use-package elpy
  :ensure t
  :config
  ;;Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'python-mode-hook 'elpy-mode)
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        elpy-shell-echo-output nil
        elpy-test-discover-runner-command '("python3" "-m" "unittest"))
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (pyvenv-mode 1))

;; jupyter notebook integration
(use-package jupyter
  :ensure t)

;; cell-like objects in python
(use-package python-x
  :ensure t
  :config
  (python-x-setup))

(use-package flycheck-pyflakes
  :ensure t
  :hook (python-mode . flycheck-mode))

(use-package py-autopep8
  :ensure t
  :hook (elpy-mode . py-autopep8-enable-on-save))

(provide 'init-python)

;;; init-python.el ends here

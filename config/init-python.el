;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;
;;   Interactive python session and completion.
;;
;;; Code:

;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;         ("\\.wsgi$" . python-mode)
;;   ; :interpreter ("python3" . python-mode)
;;   :init
;;   (setq-default indent-tabs-mode nil)
;;   :config
;;   (setq python-indent-offset 4)
;;   :hook (python-mode color-identifiers))

(use-package company-jedi
  :ensure t
  :defer t
  :config (add-to-list 'company-backends 'company-jedi))

;; (use-package anaconda-mode
;;   :ensure t
;;   :hook
;;   (python-mode . anaconda-mode)
;;   (python-mode-hook . anaconda-eldoc-mode))

;; (use-package company-anaconda
;;   :ensure t
;;   :hook (python-mode anaconda-mode)
;;   :config (add-to-list 'company-backends '(company-anaconda :with company-capf)))

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
  (add-hook 'python-mode-hook 'elpy-mode)
  (setq py-python-command "python3")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (setq jedi:complete-on-dot t)
  ;; Open the Python shell in a buffer after sending code to it
  (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
  (setq python-shell-interpreter "python3")
  ;; (setq python-shell-interpreter-args "console --simple-prompt")
  ;; (setq python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (pyvenv-mode 1)
)

;; jupyter notebook integration
(use-package ein
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

;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;
;;   Interactive python session and completion.
;;
;;; Code:
(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi\\'" . python-mode))
  :interpreter ("python3" . python-mode)
  :hook ((inferior-python-mode . fix-python-password-entry)
         (python-mode . my/python-mode-hook))
  :config
  (defun my/set-python-interpreter ()
    "Set the Python interpreter to Jupyter if available, otherwise to python3."
    (if (executable-find "jupyter")
        (setq python-shell-interpreter "jupyter"
              python-shell-interpreter-args "console --simple-prompt")
      (setq python-shell-interpreter "python3"
            python-shell-interpreter-args "-i")))

  (my/set-python-interpreter)

  (setq python-shell-prompt-detect-failure-warning nil
        python-indent-offset 4
        python-shell-completion-native-enable nil) ;; Disable native completions globally
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "python3") ;; Disable native completions for python3
  (defun fix-python-password-entry ()
    (push 'comint-watch-for-password-prompt comint-output-filter-functions))
  (defun my/python-mode-hook ()
    "Hook to set up python-mode with the correct virtual environment."
    (when (bound-and-true-p pyvenv-virtual-env)
      (setq python-shell-interpreter (concat pyvenv-virtual-env "/bin/python")))))

(use-package pyvenv
  :config
  (pyvenv-mode t))

;; Optional: Automatically activate a virtual environment when opening a project
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Documents/work/")) ;; Set your project directory
  (projectile-mode +1)
  (add-hook 'projectile-after-switch-project-hook 'my/projectile-after-switch-project-hook))

(defun my/projectile-after-switch-project-hook ()
  "Hook to activate virtualenv when switching projects."
  (let ((venv-path (concat (projectile-project-root) ".venv")))
    (when (file-exists-p venv-path)
      (pyvenv-activate venv-path))))

(add-hook 'quarto-mode-hook (lambda () (pyvenv-activate (concat (projectile-project-root) ".venv"))))

(provide 'init-python)
;;; init-python.el ends here.

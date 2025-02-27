;;; init-python.el --- Module for Python mode customisation
;;
;;; Commentary:
;;
;;   Interactive python session and completion.
;;
;;; Code:
(use-package python
  :defer t
  :init
    (defun python-comint-filter (output)
    "Filter out '__PYTHON_EL_' when sending region to inferior Python shell.
     See also: https://stackoverflow.com/questions/75103221/emacs-remove-python-el-eval-message"
  (let* ((regexp "^.*__PYTHON_EL_\\(.*\\)\\(.*\\)[[:space:]]*$")
         (lines (split-string output "\n"))
         (filtered-lines (cl-remove-if (lambda (line)
                                      (or (string-match-p regexp line)
                                          (string-match-p "^\\s-*$" line)))
                                    lines)))
    (if (equal (length lines) (length filtered-lines))
        output
      (mapconcat 'identity filtered-lines "\n"))))
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi\\'" . python-mode))
  :interpreter ("python3" . python-mode)
  :hook ((inferior-python-mode . fix-python-password-entry)
       (python-mode . my/python-mode-hook))
  :config
  (defun my/set-python-interpreter ()
    "Set the Python interpreter to Jupyter if available, otherwise to python3."
    (if (and (executable-find "jupyter")
             (string-match-p "console" (shell-command-to-string "jupyter --version")))
        (setq python-shell-interpreter "jupyter"
              python-shell-interpreter-args "console --simple-prompt")
      (setq python-shell-interpreter "python3"
            python-shell-interpreter-args "-i")))

  (my/set-python-interpreter)

  (setq python-shell-prompt-detect-failure-warning nil
        python-indent-offset 4
        python-shell-completion-native-enable nil)
  (dolist (interpreter '("jupyter-console" "jupyter" "python3"))
    (add-to-list 'python-shell-completion-native-disabled-interpreters interpreter))

(defun fix-python-password-entry ()
  "Fix password entry in Python shells and set up comint filters."
  (push 'comint-watch-for-password-prompt comint-output-filter-functions)
  (add-to-list 'comint-preoutput-filter-functions 'python-comint-filter))

  (defun my/python-mode-hook ()
    "Set up python-mode with virtual environment if available."
    (when (bound-and-true-p pyvenv-virtual-env)
      (setq python-shell-interpreter (concat pyvenv-virtual-env "/bin/python")))))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setq pyvenv-tracking-ask-before-change t))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Documents/work/"))
  (projectile-mode +1)
  (add-hook 'projectile-after-switch-project-hook
            'my/projectile-after-switch-project-hook))

(defun my/projectile-after-switch-project-hook ()
  "Hook to activate virtualenv when switching projects."
  (let ((venv-path (concat (projectile-project-root) ".venv")))
    (when (file-exists-p venv-path)
      (pyvenv-activate venv-path))))

(provide 'init-python)
;;; init-python.el ends here.

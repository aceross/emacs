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
  :hook (inferior-python-mode . fix-python-password-entry)
  :config
  (setq python-shell-interpreter "jupyter-console"
        python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        python-indent-offset 4)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (defun fix-python-password-entry ()
	(push
	 'comint-watch-for-password-prompt comint-output-filter-functions)))

(use-package elpy
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block))
    :hook ((elpy-mode . flycheck-mode))
    :init
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2))

(provide 'init-python)
;;; init-python.el ends here

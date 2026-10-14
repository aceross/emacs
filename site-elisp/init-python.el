;;; init-python.el --- Project-local Python configuration -*- lexical-binding: t; -*-
(require 'awc-python)

(use-package python
  :straight nil
  :mode (("\\.py\\'" . python-mode) ("\\.wsgi\\'" . python-mode))
  :interpreter ("python3" . python-mode)
  :custom (python-indent-offset 4)
  :hook ((python-mode . awc-python-prepare-buffer)
         (python-ts-mode . awc-python-prepare-buffer))
  :bind (:map python-mode-map
              ("C-c C-z" . awc-python-run)
              ("C-j" . awc-python-send-region-or-line)
              ("C-c C-k" . awc-python-interrupt)
              ([remap run-python] . awc-python-run)))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Documents/work/"))
  (projectile-mode +1))

(provide 'init-python)
;;; init-python.el ends here

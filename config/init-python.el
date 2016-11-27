;; TO DO - Fix python set up

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

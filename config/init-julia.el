 ;; (defvar julia-executable "julia")

;; (use-package julia-mode
;;   :ensure t
;;   :init
;;   (push '("\\.jl\\'" . julia-mode) auto-mode-alist)
;;   ;; (delete-dups auto-mode-alist)
;;   )

(use-package vterm
  :ensure t)

(use-package julia-snail
  :ensure t
  :requires vterm
  :hook (julia-mode . julia-snail-mode))

;; (use-package flycheck-julia
;;   :ensure t
;;   :init
;;   (flycheck-julia-setup)
;; ;  (add-to-list 'flycheck-global-modes 'julia-mode)
;;   (add-hook 'julia-mode-hook #'flycheck-julia-setup))

;; (use-package julia-repl
;;   :commands julia-repl julia-repl-mode
;;   :config (load-library "julia-mode"))

  ;; (use-package julia-repl
  ;;   :ensure t
  ;;   :init
  ;;   (with-eval-after-load 'julia-mode
  ;;     (add-hook 'flycheck-mode-hook #'flycheck-julia-setup))
  ;;   :config (add-hook 'julia-mode-hook 'julia-repl-mode))

;; (use-package julia-repl
;;   :ensure t
;;   :config (add-hook 'julia-mode-hook 'julia-repl-mode))

;; (use-package julia-repl
;;   :commands julia-repl julia-repl-mode
;;   :config
;;   (load-library "julia-mode")
;;   (add-hook 'julia-mode-hook 'julia-repl-mode))

;; (use-package flycheck-julia
;;     :ensure t
;;     :init
;;     (progn
;;       (setq flycheck-julia-executable julia-executable)
;;       (flycheck-julia-setup)))

(provide 'init-julia)

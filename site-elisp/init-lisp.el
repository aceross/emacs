(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

;; The Superior Lisp Interaction Mode for Emacs.
;; (use-package slime
;;   :defer t
;;   :if sbcl-executable-path
;;   :commands slime-mode
;;   :hook
;;   ((lisp-mode . slime-mode))
;;   :custom
;;   (slime-complete-symbol*-fancy t)
;;   (slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
;;   (slime-net-coding-system 'utf-8-unix)
;;   :config
;;   (setq inferior-lisp-program sbcl-executable-path)
;;   (slime-setup '(slime-asdf
;; 		 slime-fancy
;; 		 slime-indentation
;; 		 slime-sbcl-exts
;; 		 slime-scratch)))

(use-package sly
  :commands (sly)
  :config (setq inferior-lisp-program "sbcl"))

(provide 'init-lisp)

;;; init-lisp.el ends here.

;;;; init-org.el

(use-package org
  :ensure t
  :defer t
  :config
  (setq org-startup-indented)
  (setq org-hide-leading-stars)
  (setq org-odd-level-only nil)
  (setq org-completion-use-ido)
  (setq org-indent-mode)
  (setq org-startup-folded nil)
  (setq org-startup-truncated nil)
  (setq auto-fill-mode -1)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-log-done t
      org-todo-keywords '((sequence "TODO" "IN PROGRESS" "OVERDUE" "DONE"))
      org-todo-keyword-faces
      (quote
       (("IN PROGRESS" . (:foreground "blue"
		       :background "deep sky blue"
		       :weight bold))
     ("OVERDUE" . (:foreground "goldenrod3"
		   :background "yellow2"
		   :weight bold)))))

  (add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (writegood-mode)))
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode)))
  )

(provide 'init-org)

;;; init-org.el ends here.

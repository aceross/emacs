;;; init-org.el --- Org-mode customisations
;;
;;; Commentary:
;;

;;; Code:

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :config
  (setq org-startup-indented t
	org-hide-leading-stars t
	org-use-speed-commands t
	org-src-fontify-natively t
	org-odd-level-only nil
	org-completion-use-ido t
	org-indent-mode t
	org-startup-folded nil
	org-startup-truncated nil
	auto-fill-mode -1
	org-confirm-babel-evaluate nil
	)
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
   :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-tree-slide
   :ensure t
   :init
   (setq org-tree-slide-skip-outline-level 4)
   (org-tree-slide-simple-profile))

(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)

(provide 'init-org)

;;; init-org.el ends here

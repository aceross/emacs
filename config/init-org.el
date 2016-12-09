;;; init-org.el --- Org-mode customisations
;;
;;; Commentary:
;;
;;  A lot of this comes from Howard Abrams' set-up:
;;  https://github.com/howardabrams/dot-files/blob/master/emacs-org.org

;;; Code:

;; display results in a block instead of prefixed with :
(setq org-babel-min-lines-for-block-output 1)

(use-package org
  :ensure t
  :init
  (setq org-startup-indented t
	org-hide-leading-stars t
	org-use-speed-commands t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-hide-emphasis-markers t
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
	     (add-hook 'flyspell-mode 'writegood-mode))))

(use-package org-plus-contrib)

(use-package org-bullets
   :ensure t
   :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package ox-tufte)

;; presentations

(use-package ox-reveal
   :init
   (setq org-reveal-root (concat "file://" (getenv "HOME") "/Public/js/reveal.js"))
   (setq org-reveal-postamble "Aaron Ceross"))

(use-package org-tree-slide
   :ensure t
   :init
   (setq org-tree-slide-skip-outline-level 4)
   (org-tree-slide-simple-profile))

(use-package org
  :init
  :config
  (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

(org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (emacs-lisp . t)
				 (R          . t)
				 (python     . t)
				 (dot        . t)
				 (plantuml   . t))))

(setq org-confirm-babel-evaluate nil)

;; export options

;; syntax highlight code blocks
(setq org-src-fontify-natively t)
;; put caption below in tables
(setq org-export-latex-table-caption-above nil)
(setq org-latex-table-caption-above nil)
(setq org-latex-listings t)
;; don't export tags
(setq org-export-with-tags nil)

(setq org-src-tab-acts-natively t)

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "microtype"))

(setq org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(provide 'init-org)

;;; init-org.el ends here

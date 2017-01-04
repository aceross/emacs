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
			      :weight bold))
	    ("OVERDUE" . (:foreground "goldenrod3"
			  :weight bold)))))
  (add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'flyspell-mode 'writegood-mode)))
  (add-hook 'org-shiftup-final-hook    'windmove-up)
  (add-hook 'org-shiftleft-final-hook  'windmove-left)
  (add-hook 'org-shiftdown-final-hook  'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package org-plus-contrib)

(use-package org-bullets
   :ensure t
   :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package ox-tufte)
(use-package ox-gfm)

;; bibliography
(use-package org-ref)

;; presentations

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  (setq org-reveal-postamble "Aaron Ceross"))

(use-package htmlize
:ensure t)

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
				 (ditaa      . t)
				 (plantuml   . t))))

(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/src/org/contrib/scripts/plantuml.jar"))

;;; export options

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
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;; skeletons

(define-skeleton org-skeleton-header
  "Insert document headers."
  "Title: "
  "#+TITLE: " str | (buffer-name) "\n"
  "#+AUTHOR: " (user-full-name) "\n"
  "#+DATE: \n"
  "#+OPTIONS: ':true *:true toc:nil num:nil" _)


;; hugo blog skeleton
(define-skeleton org-skeleton-blog-frontmatter
  "Insert hugo frontmatter"
  "title:"
  "#+BEGIN_EXPORT md\n"
  "+++\n"
  "title = \"" str "\"\n"
  "description = \""_"\"\n"
  "date = \n"
  "tags = []\n"
  "+++\n"
  "#+END_EXPORT\n")

(define-skeleton org-skeleton-latex-header
  "Insert document headers and essential LaTeX header options."
  "options"
  '(org-skeleton-header)
  "\n#+LaTeX_HEADER: \\usepackage{booktabs}\n"
  "#+LaTeX_HEADER: \\usepackage[style=british]{csquotes}\n"
  "#+LaTeX_HEADER: \\usepackage[dvipsnames,table,xcdraw]{xcolor}\n"
  "#+LaTeX_HEADER: \\hypersetup{colorlinks=true,linkcolor=Maroon,citecolor=PineGreen}\n"
  "#+LaTeX_HEADER: \\usepackage[UKenglish]{babel}\n"
  "#+LaTeX_HEADER: \\usepackage[UKenglish]{isodate}\n"
  "#+LaTeX_HEADER: \\usepackage{sectsty}\n"
  "#+LaTeX_HEADER: \\subsectionfont{\normalfont\itshape}\n"
  )

(define-skeleton org-skelton-R-src
  "Basic R source code block"
  "#+begin_src R :session R :cache yes\n"
  " \n"
  "#+end_src"
  )

(define-skeleton org-skeleton-ditaa-src-block
  "Source block for ditaa"
  "#+begin_src ditaa :file <name>.png\n"
  " \n"
  "#+end_src")

(provide 'init-org)

;;; init-org.el ends here

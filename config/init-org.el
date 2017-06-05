;;; init-org.el --- Org-mode customisations
;;
;;; Commentary:
;;
;;  A lot of this comes from Howard Abrams' set-up:
;;  https://github.com/howardabrams/dot-files/blob/master/emacs-org.org

;;; Code:

;; display results in a block instead of prefixed with :
(setq org-babel-min-lines-for-block-output t)

(use-package org
  :ensure org-plus-contrib
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-src-fontify-natively t
        org-startup-indented t
        org-hide-leading-stars t
        org-use-speed-commands t
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
         (("IN PROGRESS" . (:foreground "steel blue"
                            :weight bold))
          ("OVERDUE"     . (:foreground "goldenrod3"
                            :weight bold)))))
  (add-hook 'org-shiftup-final-hook    'windmove-up)
  (add-hook 'org-shiftleft-final-hook  'windmove-left)
  (add-hook 'org-shiftdown-final-hook  'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (require 'ox-latex)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (setq org-export-latex-table-caption-above nil)
  (setq org-latex-table-caption-above nil)
  (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(
                                 (emacs-lisp . t)
                                 (R          . t)
                                 (C          . t)
                                 (python     . t)
                                 (clojure    . t)
                                 (latex      . t)
;                                 (julia      . t)
                                 (dot        . t)
                                 (ditaa      . t)
                                 (plantuml   . t)))
  (add-to-list 'org-latex-classes
               '("awc-article"
                 "
               \\documentclass{article}
               \\usepackage[utf8]{inputenc}
               \\usepackage[T1]{fontenc}
               \\usepackage{graphicx}
               \\usepackage{longtable}
               \\usepackage{hyperref}
               \\usepackage{natbib}
               \\usepackage{amssymb}
               \\usepackage{amsmath}
               \\usepackage{geometry}
               \\geometry{
                    a4paper,
                    left=2.5cm,
                    top=2cm,
                    right=2.5cm,
                    bottom=2cm,
                    marginparsep=7pt,
                    marginparwidth=.6in
                 }
               \\usepackage{booktabs}
               \\usepackage[style=british]{csquotes}
               \\usepackage[dvipsnames,table,xcdraw]{xcolor}
               \\hypersetup{
                    colorlinks=true,
                    linkcolor=Maroon,
                    citecolor=PineGreen
                 }
               \\usepackage[UKenglish]{babel}
               \\usepackage[UKenglish]{isodate}
               "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("IEEE"
                 "\\documentclass[conference]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))
               )
  (add-to-list 'org-latex-classes
               '("ACM"
                 "\\documentclass[conference]{sig-alternate}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))
               )
  (add-to-list 'org-latex-classes
               '("tuftehandout"
                 "\\documentclass{tufte-handout}
                  \\usepackage{color}
                  \\usepackage{amssymb}
                  \\usepackage{amsmath}
                  \\usepackage{gensymb}
                  \\usepackage{nicefrac}
                  \\usepackage{units}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-plus-contrib
  :ensure t
  :defer t)

(use-package org-bullets
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package ox-tufte)
(use-package ox-gfm)
(use-package org-pomodoro
  :ensure t
  :defer t)

;; bibliography
(use-package org-ref)

;; presentations
(use-package ox-reveal
  :ensure t
  :defer t
  :init
  (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  (setq org-reveal-postamble "Aaron Ceross"))

(use-package htmlize
  :ensure t
  :defer t)

(use-package org-tree-slide
  :ensure t
  :defer t
  :init
   (setq org-tree-slide-skip-outline-level 4)
   (org-tree-slide-simple-profile))

(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/src/org/contrib/scripts/plantuml.jar"))

;;; export options

;; put caption below in tables

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
  "#+OPTIONS: ':true *:true toc:nil" _)

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

(define-skeleton org-skeleton-R-src
  "Basic R source code block."
  "\n"
  "#+begin_src R :session :cache yes :exports none :tangle yes\n"
  "\n"
  "\n"
  "#+end_src\n")

(define-skeleton org-skeleton-dot-src
  "Source block for dot."
  "file name: "
  "\n"
  "#+begin_src dot :file img/" str ".png\n"
  " \n"
  "#+end_src")

(provide 'init-org)

;;; init-org.el ends here

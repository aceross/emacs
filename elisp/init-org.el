;;; package --- Summary
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :defer t
  :config
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)"  "|" "DRAFT(d)")
              (sequence "REVISE(r)" "|" "DONE")
              )))
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-latex-listings t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-odt-preferred-output-format "doc")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.65))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((R      . t)
				 (python . t)))
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 70)))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  :hook (org-mode . visual-line-mode)
  )

;; common LaTeX templates I use in my work.
(use-package org
  :init
  (with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-classes
		 '("IEEE"
                   "
\\documentclass[conference]{IEEEtran}
\\usepackage[hidelinks]{hyperref}
\\bibliographystyle{IEEEtran}
                 "
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")))
    (add-to-list 'org-latex-classes
		   '("ACM"
                    "
\\documentclass{acmart}
\\bibliographystyle{ACM-Reference-Format}
\\usepackage{subcaption}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))
		   ))
  )

(use-package org-superstar
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  :hook (org-mode . org-superstar-mode))

(use-package org-pdftools)

(use-package org-sidebar)

(use-package org-ref-prettify
  :hook (org-mode . org-ref-prettify-mode))

(use-package org-pomodoro)

;; TODO: Input keybindings
(use-package org-ref
  :defer t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq bibtex-completion-bibliography
	'("~/MEGA/bibliography/references.bib"))
  (setq bibtex-completion-additional-search-fields '(keywords))
  :bind(:map org-mode-map
	     ("C-q c" . org-ref-insert-link)
	     ("C-q r" . org-ref-insert-ref-function)   ; FIXME
	     ("C-q l" . org-ref-insert-label-function) ; FIXME
	     ))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-drill
  :defer t
  :commands (org-drill)
  :config
  (setq org-drill-hide-item-headings-p t)
  )

;;; skeletons

(define-skeleton org-skeleton-header
  "Insert document headers."
  "Title: "
  "#+TITLE: " str | (buffer-name) "\n"
  "#+AUTHOR: " (user-full-name) "\n"
  "#+DATE: \n"
  "#+OPTIONS: ':true *:true toc:nil" _)
(define-key org-mode-map (kbd "C-q h") 'org-skeleton-header)

(define-skeleton org-skeleton-R-src
  "Basic R source code block for org-mode."
  nil
  "#+begin_src R :session :cache yes :exports none :tangle yes\n"
  > _
  "\n#+end_src")
(define-key org-mode-map (kbd "C-q s r") 'org-skeleton-R-src)

(define-skeleton org-inline-R-src
  "Insert inline R in org-mode document."
  _"src_R[:session :results raw]{"> _ "}")
(define-key org-mode-map (kbd "C-q i r") 'org-inline-R-src)

(define-skeleton org-ref-insert-default-bib
  "Insert the default bibliography in an org document."
  _"bibliography:~/MEGA/bibliography/references.bib")
(define-key org-mode-map (kbd "C-q b") 'org-ref-insert-default-bib)

(provide 'init-org)
;;; init-org.el ends here

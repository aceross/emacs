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
  (setq org-image-actual-width nil)
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

(use-package org-modern
  :after org
  :config
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"
	org-modern-table t
	)
  (global-org-modern-mode)
  )

(use-package org-pdftools)

(use-package org-sidebar)

(use-package org-ref :defer t)

(use-package org-roam
  :after org

  :custom
  (org-roam-directory "~/MEGA/org")
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  (:map org-mode-map
        (("C-c n i" . org-roam-node-insert)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n b" . org-roam-buffer-toggle)
		 ("C-c n t" . org-roam-tag-add)
		 ("C-c n r" . org-roam-ref-add)
		 ))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "${slug}.org"
                                                         "#+TITLE: ${title}\n#+DATE: %T\n")
                                      :unnarrowed t)))
  ;; this sets up various file handling hooks so your DB remains up to date
  (org-roam-setup))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))


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

(provide 'init-org)
;;; init-org.el ends here

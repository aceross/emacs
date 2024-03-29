;;; package --- Summary
;;
;;; Commentary:
;;
;;; Code:

;; (use-package ob-julia-vterm)

(use-package org
  :defer t
  :config
  (setq org-todo-keywords
		'((sequence "TODO(t)"       ; a task to be tackled
					"PROGRESS(p)"   ; a task in progress
					"WAITING(w)"    ; someone needs to do something
					"DELEGATED(d)"  ; task has been given to someone else
					"REVIEW(r)"     ; task is in review for completion
					"OVERDUE(o)"    ; task is overdue
					"MEETING(m)"    ; task is a meeting
					"|"             ; delimits active/inactive states
					"DONE(d)"       ; task completed
					"CANCELLED(c)"  ; task is terminated
					"GRAVEYARD(g)"  ; task is abandoned
					)))
  (setq org-todo-keyword-faces
		'(("WAITING"   . (:foreground "#77327d" :weight bold))
		  ("DELEGATED" . (:background "#d09dc0" :weight bold))
		  ("OVERDUE"   . (:background "#918b04" :weight bold))
		  ("MEETING"   . (:foreground "#0087af" :background "#bfefff" :weight bold))
		  ("CANCELLED" . (:background "#d00000" :weight bold))
		  ("GRAVEYARD" . (:background "#b0b0b0" :weight bold))
		 ))
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)
 ;; (setq org-cite-global-bibliography "~/MEGA/bibliography/references.bib")
  ;;(setq org-pretty-entities-include-sub-superscripts t)
  (setq org-agenda-files '("~/Documents/zettelkasten/agenda/"))
  (setq org-log-done-with-time t)
  ; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (setq org-latex-listings t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-odt-preferred-output-format "doc")
  (setq org-image-actual-width nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.65))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((R            . t)
					 (python       . t)
					 ;;	 (julia-vterm  . t)
					 )
				   )
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 70)))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  :hook (org-mode . visual-line-mode)
  :custom
  (org-cite-global-bibliography
   '("~/Documents/bibliography/references.bib"))
  )

;; (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;; (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)

;; org-agenda settings
;; (use-package org
;;   :init
;;   (setq org-agenda-files '("~/Documents/zettelkasten/"))
;;   (setq org-log-done-with-time t)
;;   ; Set default column view headings: Task Total-Time Time-Stamp
;;   (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
;; )

;; (use-package org-modern
;;   :after org
;;   :config
;;   (setq org-auto-align-tags nil
;;         org-tags-column 0
;;         org-catch-invisible-edits 'show-and-error
;;         org-special-ctrl-a/e t
;;         org-insert-heading-respect-content t
;;         ;; Org styling, hide markup etc.
;;         org-hide-emphasis-markers t
;;         ;; org-pretty-entities t
;;         org-ellipsis "…"
;; 	org-modern-table t
;; 	)
;;   (global-org-modern-mode)
;;   )
;; (let* ((variable-tuple
;;           (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :weight bold))
;; 		 )

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-modern-hide-stars 'leading)
  (org-modern-todo nil)
  (org-modern-tag t)
  ;; Customize this per your font
  (org-modern-label-border .25)
  ;; Note that these stars allow differentiation of levels
  ;; "①" "②" "③" "④" "⑤" "⑥" "⑦"
  (org-modern-star ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"])
  )

(use-package org-appear
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

(use-package org-pdftools)

(use-package org-sidebar)

(use-package org-ref :defer t)

(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/Documents/zettelkasten/zettelkasten/")
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

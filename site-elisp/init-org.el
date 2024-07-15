;;; init-org.el --- Configuration file for Emacs org-mode
;;
;; Author: Aaron Ceross
;;
;; This file configures Emacs org-mode with various customizations and integrations,
;; including org-modern, org-appear, org-roam, and dynamic org-todo faces based on theme.
;;

;;; Commentary:
;;
;; This Emacs configuration file sets up org-mode with various customizations:
;; - Configures org-todo-keywords and their faces.
;; - Integrates org-modern for modernisng org-mode visuals.
;; - Enables org-appear for auto-emphasis and other visual tweaks.
;; - Sets up org-roam for note-taking and linking.
;;
;; Dynamic faces for org-todo keywords are adjusted based on the current Emacs theme's
;; background color to ensure visibility and aesthetics in both light and dark modes.
;;

;;; Code:

(use-package org
  :defer t
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "DELEGATED(d)"
               "REVIEW(r)" "OVERDUE(o)" "MEETING(m)" "|"
               "DONE(d)" "CANCELLED(c)" "GRAVEYARD(g)")))
  :config
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/Documents/zettelkasten/agenda/"))
  (setq org-log-done-with-time t)
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
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
  (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 70)))
  :hook (org-mode . visual-line-mode))

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
  (org-modern-label-border .25)
  (org-modern-star ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"]))

(use-package org-appear
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
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
         ("C-c n r" . org-roam-ref-add)))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "${slug}.org"
                                                         "#+TITLE: ${title}\n#+DATE: %T\n")
                                      :unnarrowed t)))
  (org-roam-setup))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

;;; Dynamic Faces based on Theme
(defun set-org-todo-faces ()
  "Set org todo faces based on theme background."
  (let ((bg (face-background 'default nil 'default)))
    (setq org-todo-keyword-faces
          `(("WAITING"
             . (:foreground ,(if (eq bg 'dark) "#ffffff" "#000000") :weight bold))
            ("DELEGATED"
             . (:background ,(if (eq bg 'dark) "#444444" "#dddddd") :weight bold))
            ("OVERDUE"
             . (:background ,(if (eq bg 'dark) "#550000" "#ffdddd") :weight bold))
            ("MEETING"
             . (:foreground ,(if (eq bg 'dark) "#00a0af" "#0087af")
                            :background ,(if (eq bg 'dark) "#002f3f" "#bfefff") :weight bold))
            ("CANCELLED"
             . (:background ,(if (eq bg 'dark) "#770000" "#ffaaaa") :weight bold))
            ("GRAVEYARD"
             . (:background ,(if (eq bg 'dark) "#707070" "#d0d0d0") :weight bold))))))

;; Call the function to set faces initially
(set-org-todo-faces)

;; Hook the function to theme changes
(add-hook 'after-init-hook 'set-org-todo-faces)
(add-hook 'after-make-frame-functions 'set-org-todo-faces)

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
  _ "src_R[:session :results raw]{" > _ "}")
(define-key org-mode-map (kbd "C-q i r") 'org-inline-R-src)

(provide 'init-org)
;;; init-org.el ends here

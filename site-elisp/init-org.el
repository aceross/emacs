;;; init-org.el --- Optimised configuration file for Emacs org-mode
;;
;; Author: Aaron Ceross
;;
;; This file configures Emacs org-mode with various customisations and integrations,
;; including org-modern, org-appear, org-roam, and dynamic org-todo faces based on theme.
;;

;;; Commentary:
;;
;; This Emacs configuration file sets up org-mode with various customisations:
;; - Configures org-todo-keywords and their faces dynamically based on theme background.
;; - Integrates org-modern for modernising org-mode visuals.
;; - Enables org-appear for auto-emphasis and other visual tweaks.
;; - Sets up org-roam for note-taking and linking.
;; - Performance optimisations via deferred loading and lazy language configurations.
;;

;;; Code:

;; Ensure 'use-package' is installed and available
(eval-when-compile
  (require 'use-package))

;; ORG-MODE CONFIGURATION
(use-package org
  :defer t
  :commands (org-mode)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "DELEGATED(d)"
               "REVIEW(r)" "OVERDUE(o)" "MEETING(m)" "|"
               "DONE(d)" "CANCELLED(c)" "GRAVEYARD(g)")))
  (org-hide-leading-stars t)
  (org-src-fontify-natively t)
  (org-hide-emphasis-markers t) ; Consolidated setting
  (org-startup-with-inline-images t)
  (org-src-tab-acts-natively t)
  (org-startup-indented t)
  (org-agenda-files '("~/Documents/zettelkasten/agenda/"))
  (org-log-done-with-time t)
  (org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (org-latex-listings t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-confirm-babel-evaluate nil) ; Ensure caution with evaluation!
  (org-odt-preferred-output-format "doc")
  (org-image-actual-width nil)
  ;; (org-format-latex-options (plist-put org-format-latex-options :scale 1.65))
  (org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  :hook
  (org-mode . (lambda ()
                (setq fill-column 70)
                (visual-line-mode 1)))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((R . t) (python . t)))
  :custom
  (org-cite-global-bibliography
   '("~/Documents/bibliography/references.bib"))
  )

;; Set org-format-latex-options after org-mode is loaded
(with-eval-after-load 'org
  (plist-put org-format-latex-options :scale 1.65))

;; Custom Faces for Org Levels
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.8))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; ORG-MODERN
(use-package org-modern
  :defer t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-modern-hide-stars 'leading)
  (org-modern-todo nil)
  (org-modern-tag t)
  (org-modern-label-border .25)
  (org-modern-fold-stars '(("▶" . "▼")
                           ("▷" . "▽")
                           ("▹" . "▿")
                           ("▸" . "▾"))))

;; ORG-APPEAR for visual emphasis auto-toggling
(use-package org-appear
  :defer t
  :after org
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

;; ORG-ROAM for Zettelkasten
(use-package org-roam
  :defer t
  :after org
  :custom
  (org-roam-directory "~/Documents/zettelkasten/zettelkasten/")
  (org-roam-completion-everywhere t)
  :bind
  (:map org-mode-map
        ("C-c n l" . org-roam-buffer-toggle)
        ("C-c n f" . org-roam-node-find)
        ("C-c n i" . org-roam-node-insert)
        ("C-c n g" . org-roam-graph)
        ("C-c n b" . org-roam-buffer-toggle)
        ("C-c n t" . org-roam-tag-add)
        ("C-c n r" . org-roam-ref-add))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+DATE: %T\n")
           :unnarrowed t)
        ("n" "literature note" plain "%?"
         :target
         (file+head
          "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
          "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
         :unnarrowed t)))
  (org-roam-setup))

(use-package ox-pandoc
  :ensure t)

;; Citar Org-Roam for Citation Management
(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

;; ORG-PDFTOOLS for integrating with PDFTools
(use-package org-pdftools
  :defer t
  :after org)

;; ORG-SIDEBAR for convenient navigation
(use-package org-sidebar
  :defer t
  :after org)

;; ORG-REF for reference management
(use-package org-ref
  :defer t
  :after org)

;; ORG-SUPERSTAR for enhanced bullets and headings
(use-package org-superstar
  :defer t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿")))

;; Dynamic Faces Based on Theme
(defun set-org-todo-faces ()
  "Set org todo faces based on theme background."
  (let ((bg (if (display-graphic-p)
                (frame-parameter nil 'background-mode)
              "light"))) ; Fallback for terminal use
    (setq org-todo-keyword-faces
          `(("WAITING" . (:foreground ,(if (string-equal bg "dark") "#ffffff" "#000000") :weight bold))
            ("DELEGATED" . (:background ,(if (string-equal bg "dark") "#444444" "#dddddd") :weight bold))
            ("OVERDUE" . (:background ,(if (string-equal bg "dark") "#550000" "#ffdddd") :weight bold))
            ("MEETING" . (:foreground ,(if (string-equal bg "dark") "#00a0af" "#0087af")
                                      :background ,(if (string-equal bg "dark") "#002f3f" "#bfefff") :weight bold))
            ("CANCELLED" . (:background ,(if (string-equal bg "dark") "#770000" "#ffaaaa") :weight bold))
            ("GRAVEYARD" . (:background ,(if (string-equal bg "dark") "#707070" "#d0d0d0") :weight bold))))))

;; Apply the dynamic face setting on theme changes and after initialization
;; (use-package cus-theme
;;   :ensure t
;;   :hook (after-init . set-org-todo-faces)
;;   :hook (after-make-frame . set-org-todo-faces))

(set-org-todo-faces)

;; Hook the function to theme changes
(add-hook 'after-init-hook 'set-org-todo-faces)
(add-hook 'after-make-frame-functions 'set-org-todo-faces)


;; Skeletons for Quick Inserts
(defun org-skeleton-header ()
  "Insert document headers."
  (interactive)
  (insert (concat "#+TITLE: " (or (read-string "Title: ") (buffer-name)) "\n"
                  "#+AUTHOR: " (user-full-name) "\n"
                  "#+DATE: \n"
                  "#+OPTIONS: ':true *:true toc:nil\n")))

(defun org-skeleton-R-src ()
  "Insert a basic R source code block for org-mode."
  (interactive)
  (insert "#+begin_src R :session :cache yes :exports none :tangle yes\n\n#+end_src"))

(defun org-inline-R-src ()
  "Insert inline R code in org-mode document."
  (interactive)
  (insert "src_R[:session :results raw]{}"))

;; Bind skeletons within org-mode using use-package
(use-package org
  :after org
  :bind
  (:map org-mode-map
        ("C-q h" . org-skeleton-header)
        ("C-q s r" . org-skeleton-R-src)
        ("C-q i r" . org-inline-R-src)))

;; Define a prefix key for org-related commands
(define-prefix-command 'org-custom-prefix)
(global-set-key (kbd "C-c o") 'org-custom-prefix)

;; Bind additional org-related commands to the prefix
(with-eval-after-load 'org
  (define-key org-custom-prefix (kbd "h") 'org-skeleton-header)
  (define-key org-custom-prefix (kbd "s r") 'org-skeleton-R-src)
  (define-key org-custom-prefix (kbd "i r") 'org-inline-R-src))

(provide 'init-org)
;;; init-org.el ends here

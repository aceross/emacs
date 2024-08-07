;;; Code:

(use-package flyspell
  :defer t
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "british")
  (setq ispell-check-comments t)
  (setq aspell-dictionary "en_GB-ise-w_accents")
  (setq aspell-program-name "/usr/bin/aspell")
  (setq ispell-dictionary "en_GB-ise-w_accents")
  ;;(setq ispell-program-name "/usr/bin/aspell")
  (set-face-attribute 'flyspell-incorrect nil
		      :background "light coral"
		      :foreground "firebrick4"
		      :weight 'bold)
  (add-to-list 'ispell-skip-region-alist
	       '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^# {{{" . "^# }}}"))
  :bind
  (:map flyspell-mode-map
	([down-mouse-3] . flyspell-correct-word)
	("<f12>" . ispell-buffer))
  :hook (text-mode . flyspell-mode)
  )

(use-package consult-flyspell
  :config
  ;; default settings
  (setq consult-flyspell-select-function nil
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package academic-phrases
  :defer t)

(use-package pdf-tools
   :mode (("\\.pdf\\'" . pdf-view-mode))
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (setq-default pdf-view-use-scaling t)
   (setq-default pdf-view-use-imagemagick nil)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

(use-package tex
  :defer t
  :straight auctex
  :config
  (TeX-global-PDF-mode t)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-math-close-double-dollar t)
  (server-start)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (TeX-source-correlate-mode t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  ;; add the tilde when using \citex
  (setq reftex-format-cite-function
     '(lambda (key fmt)
        (let ((cite (replace-regexp-in-string "%l" key fmt)))
          (if (or (= ?~ (string-to-char fmt))
                  (member (preceding-char) '(?\ ?\t ?\n ?~)))
              cite (concat "~" cite)))))
  :hook
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package latex-math-preview)

(use-package latex-preview-pane
  :init (latex-preview-pane-enable))

(use-package bibtex
  :config
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-dialect 'biblatex)
  (setq bib-files-directory (directory-files
                             (concat (getenv "HOME") "/Documents/bibliography/") t
                             "^[A-Z|a-z].+.bib$")
        pdf-files-directory (concat (getenv "HOME") "/Documents/bibliography/pdf")))

(use-package reftex
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '("~/Documents/bibliography/references.bib"))
  (eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\citet[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")
             (?d . "[@%l]")
             ))))
  :hook
  (LaTeX-mode . turn-on-reftex)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package writegood-mode
  :ensure t
  :defer t
  :diminish writegood-mode
  :config
  :bind (("C-c gg" . writegood-grade-level)
         ("C-c ge" . writegood-reading-ease))
  :hook (text-mode org-mode markdown-mode gfm-mode))

(use-package powerthesaurus
  :bind
  ("<f7>"    . powerthesaurus-lookup-synonyms-dwim)
  ("S-<f7>"  . powerthesaurus-lookup-antonyms-dwim)
  ("C-c t d" . powerthesaurus-lookup-definitions-dwim)
  ("C-c t s" . powerthesaurus-lookup-sentences-dwim))

(use-package move-text
    :init
    (move-text-default-bindings))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package ebib
  :commands ebib
  :config
  (setq ebib-default-directory "~/Documents/bibliography/references.bib"
	ebib-bib-search-dirs `(,bibtex-file-path)))

;; (use-package citar
;;   :bind (("C-c b" . citar-insert-citation)
;; 		 ("C-c r" . citar-insert-reference)
;;          :map minibuffer-local-map
;;          ("M-b" . citar-insert-preset))
;;   :custom
;;   (Org-cite-csl-styles-dir
;;    (expand-file-name "~/styles/"))
;;   (citar-bibliography '("~/Documents/bibliography/references.bib"))
;;   (citar-templates
;;  '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
;;    (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
;;    (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
;;    (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
;;    ))
;; ;; Configuring all-the-icons. From
;; ;; https://github.com/bdarcus/citar#rich-ui
;;   (citar-symbols
;;    `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) .
;;            ,(all-the-icons-faicon "file-o" :face 'kb/citar-icon-dim :v-adjust -0.1) )
;; 	 (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
;;            ,(all-the-icons-material "speaker_notes" :face 'kb/citar-icon-dim :v-adjust -0.3))
;; 	 (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
;;            ,(all-the-icons-octicon "link" :face 'kb/citar-icon-dim :v-adjust 0.01))))
;;   (citar-symbol-separator "  ")
;; )

;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode))

(require 'oc-biblatex)
(require 'oc-natbib)
(require 'oc-csl)

(use-package citar
  :bind
  (("C-c b" . citar-insert-citation)
   ("C-c r" . citar-insert-reference))
  :custom
  (org-cite-csl-styles-dir (expand-file-name "~/styles/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  ;; (citar-notes-paths (list (concat org-directory "brain/bib_notes/")))
  (citar-templates `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "    ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                     (note . ,(concat "#+TITLE: ${title}\n"
                                      "#+AUTHOR: ${author editor}\n"
                                      "#+DATE: ${date}\n"
                                      "#+SOURCE: ${doi url}\n"
                                      "#+CUSTOM_ID: ${=key= id}\n"
                                      "#+cite_export: biblatex ieee\n"
                                      (concat "#+bibliography: " (car citar-bibliography) "\n\n")
                                      "* Notes :ignore:\n"
                                      ":PROPERTIES:\n"
                                      ":NOTER_DOCUMENT: ${file} \n"
                                      ":END:\n\n"
                                      "* Summary :childless:showchildren:export:\n"
                                      "This is a summary of [cite/t:@${=key=}].\n"
                                      "** Bibliography :ignore:\n"
                                      ))))
  (citar-symbol-separator "  ")
  :config
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-pencil"
              :face 'nerd-icons-blue
              :v-adjust 0.01)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icons-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  ;;:bind
  ;;(:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :hook
  ((LaTeX-mode . citar-capf-setup)
   (org-mode . citar-capf-setup)))

(use-package citar-embark
  :hook
  ((LaTeX-mode . citar-embark-mode)
   (org-mode . citar-embark-mode)))

(use-package lsp-grammarly
  :init
  (setq lsp-grammarly-dialect "british")
  :config
  (setq lsp-grammarly-domain "academic")
  (setq lsp-grammarly-audience "expert")
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

(provide 'init-text)
;;; init-text.el ends here

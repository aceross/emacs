;;; init-text.el --- Configuration for text editing and bibliography management

;;; Commentary:

;;; Code:

;; Package configuration for Flyspell, a spell-checking tool
(use-package flyspell
  :defer t
  :custom
  ;; Use Aspell for spell checking based on system type
  (ispell-program-name (if (eq system-type 'darwin)
                           "/opt/homebrew/bin/aspell"
                         "/usr/bin/aspell"))
  (ispell-dictionary "en_GB-ise-w_accents")
  (ispell-check-comments t)
  (aspell-dictionary "en_GB-ise-w_accents")
  :config
  ;; Customise the appearance of misspelled words
  ;; (set-face-attribute 'flyspell-incorrect nil
  ;;                     :foreground "firebrick"
  ;;                     :weight 'bold)
  ;; Define regions to skip during spell checking
  (add-to-list 'ispell-skip-region-alist
               '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^# {{{" . "^# }}}"))
  :bind
  (:map flyspell-mode-map
        ([down-mouse-3] . flyspell-correct-word)
        ("<f12>" . ispell-buffer))
  :hook
  (text-mode . flyspell-mode))

;; A package to enhance Flyspell functionality
(use-package consult-flyspell
  :after flyspell
  :custom
  (consult-flyspell-select-function nil)
  (consult-flyspell-set-point-after-word t)
  (consult-flyspell-always-check-buffer nil))

;; Package for managing academic phrases
(use-package academic-phrases
  :defer t)

;; Configuration for PDF tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  (pdf-view-display-size 'fit-width)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook
  (pdf-view-mode . (lambda ()
                   (linum-mode -1)
                   (pdf-view-continuous-mode 1))))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode))

(use-package djvu
  :ensure t
  :mode ("\\.djvu\\'" . djvu-mode))

;; Configuration for AUCTeX, a comprehensive TeX/LaTeX editing package
(use-package tex
  :defer t
  :straight auctex
  :custom
  ;; General AUCTeX settings
  (TeX-global-PDF-mode t)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-math-close-double-dollar t)
  (TeX-command-extra-options "--synctex=1")
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  ;; RefTeX settings for citation formatting
  (reftex-format-cite-function
   '(lambda (key fmt)
      (let ((cite (replace-regexp-in-string "%l" key fmt)))
        (if (or (= ?~ (string-to-char fmt))
                (member (preceding-char) '(?\ ?\t ?\n ?~)))
            cite (concat "~" cite)))))
  :config
  ;; Start Emacs server for PDF syncing
  (server-start)
  (TeX-source-correlate-mode t)
  ;; Custom section title appearance in LaTeX mode
  (defun my-latex-custom-sectioning ()
    "Set LaTeX section faces to inherit from Org-mode heading faces."
    (set-face-attribute 'font-latex-sectioning-1-face nil :inherit 'org-level-1)
    (set-face-attribute 'font-latex-sectioning-2-face nil :inherit 'org-level-2)
    (set-face-attribute 'font-latex-sectioning-3-face nil :inherit 'org-level-3)
    (set-face-attribute 'font-latex-sectioning-4-face nil :inherit 'org-level-4)
    ;; Optionally, if you use a fifth level:
    (set-face-attribute 'font-latex-sectioning-5-face nil :inherit 'org-level-5))
  :hook
  ((LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . my-latex-custom-sectioning)
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)))

;; Preview LaTeX math
(use-package latex-math-preview)

;; Enable the LaTeX preview pane
(use-package latex-preview-pane
  :init (latex-preview-pane-enable))

;; Configuration for BibTeX, a bibliography management tool
(use-package bibtex
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-dialect 'biblatex)
  :config
  ;; Set up directories for BibTeX files and PDFs
  (setq bib-files-directory (expand-file-name "~/Documents/bibliography/")
        pdf-files-directory (expand-file-name "~/Documents/bibliography/pdf")))

;; Configuration for RefTeX, a citation management package
(use-package reftex
  :defer t
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography '("~/Documents/bibliography/references.bib"))
  :config
  ;; Customize citation formats
  (with-eval-after-load 'reftex-vars
    (setq reftex-cite-format
          '((?\C-m . "\\cite[]{%l}")
            (?f . "\\footcite[][]{%l}")
            (?t . "\\citet[]{%l}")
            (?p . "\\parencite[]{%l}")
            (?o . "\\citepr[]{%l}")
            (?n . "\\nocite{%l}")
            (?d . "[@%l]"))))
  :hook
  (LaTeX-mode . turn-on-reftex))

;; Markdown mode configuration
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  ;; Inherit Org-mode heading styles
  (set-face-attribute 'markdown-header-face-1 nil :inherit 'org-level-1)
  (set-face-attribute 'markdown-header-face-2 nil :inherit 'org-level-2)
  (set-face-attribute 'markdown-header-face-3 nil :inherit 'org-level-3)
  (set-face-attribute 'markdown-header-face-4 nil :inherit 'org-level-4)
  (set-face-attribute 'markdown-header-face-5 nil :inherit 'org-level-5)
  (set-face-attribute 'markdown-header-face-6 nil :inherit 'org-level-6))

;; Configuration for Writegood Mode, a tool for improving writing
(use-package writegood-mode
  :ensure t
  :defer t
  :diminish writegood-mode
  :bind (("C-c gg" . writegood-grade-level)
         ("C-c ge" . writegood-reading-ease))
  :hook (text-mode org-mode markdown-mode gfm-mode))

;; Thesaurus lookup tool
(use-package powerthesaurus
  :bind
  ("<f7>"    . powerthesaurus-lookup-synonyms-dwim)
  ("S-<f7>"  . powerthesaurus-lookup-antonyms-dwim)
  ("C-c t d" . powerthesaurus-lookup-definitions-dwim)
  ("C-c t s" . powerthesaurus-lookup-sentences-dwim))

;; Configuration for moving text up and down
(use-package move-text
  :init
  (move-text-default-bindings))

;; Configuration for whitespace cleanup
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Configuration for Ebib, a BibTeX database manager
(use-package ebib
  :commands ebib
  :config
  ;; Set up directories for Ebib
  (setq ebib-default-directory "~/Documents/bibliography/"
        ebib-bib-search-dirs (list bib-files-directory)))

(defun sync-ebib-to-zotero ()
  "Save Ebib and trigger Zotero import."
  (interactive)
  (ebib-save-databases)
  (shell-command "zotero-cli import --file ~/Documents/bibliography/references.bib"))

;; Configuration for Citar, a citation management tool
(use-package citar
  :defer t
  :bind
  (("C-c b" . citar-insert-citation)
   ("C-c r" . citar-insert-reference))
  :custom
  (org-cite-csl-styles-dir (expand-file-name "~/styles/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/Documents/bibliography/references.bib"))
  (citar-notes-paths '("~/Documents/zettelkasten/zettelkasten/"))
  (citar-file-note-extensions '("org"))
  (citar-at-point-function 'embark-act)
  (citar-notes-create-on-selection t)
  (citar-templates `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "    ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                     (note . ,(concat "Notes on ${author editor} ${title}\n"
                                      "#+source: ${doi url}\n"
                                      "#+roam_key: cite:${=key= id}\n"
                                      "#+PDF: ${file}\n\n"
                                      "* Overview\n"))))
  (citar-symbol-separator "  ")
  :config
  ;; Define custom indicators for Citar
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
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
  ;; (Optional) Uncomment and adjust the following to open PDFs in a new buffer
  ;; (setq citar-open-entry-functions '(citar-open-pdf-in-buffer))
  )

;; Configuration for Citar Embark, integration with Embark
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package unfill
  :ensure t)

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width 80)
  :hook
  (text-mode . visual-fill-column-mode))

(provide 'init-text)
;;; init-text.el ends here

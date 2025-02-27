;;; init-ui.el --- User Interface customisations
;;
;;; Commentary:
;;
;;  This configuration file contains customisations related to the
;;  visual aesthetics and functionality of the Emacs user interface.
;;  It includes settings for fonts, themes, modeline, and various
;;  visual enhancements to improve the overall user experience.
;;
;;  Key Features:
;;  - Font customisation with fallback options
;;  - Indentation guides for better code readability
;;  - Highlighting changes and other volatile actions
;;  - Git integration with gutter indicators
;;  - Smooth scrolling
;;  - Rainbow delimiters for distinguishing nested expressions
;;  - Highlighting TODO keywords in code
;;  - Dynamic theme switching based on time of day
;;  - Modeline enhancements with `doom-modeline`
;;  - Which-key integration for discovering keybindings
;;  - Nerd icons for various modes
;;  - Enhanced terminal colors
;;  - Darkroom mode for distraction-free writing
;;  - Pulsar for visual feedback on various actions
;;  - Ligature support for programming modes
;;  - Tree-sitter integration for improved syntax highlighting
;;

;;; Code:

;; Function to set default font with fallback
(defun set-default-font (font-names)
  "Set the default font to the first available font in FONT-NAMES."
  (let ((available-font (seq-find (lambda (font) (find-font (font-spec :name font))) font-names)))
    (when available-font
      (set-face-attribute 'default nil :family available-font :weight 'normal))))

;; Function to set Han font with fallback
(defun set-han-font (font-names size)
  "Set the Han font to the first available font in FONT-NAMES with SIZE."
  (let ((available-font (seq-find (lambda (font) (find-font (font-spec :name font))) font-names)))
    (when available-font
      (set-fontset-font t 'han (font-spec :name available-font :size size)))))

;; List of preferred and fallback fonts
(setq preferred-default-fonts '("Comic Code Ligatures" "Monaco" "Menlo" "Courier"))
(setq preferred-han-fonts '("Hiragino Sans GB" "SimSun" "Microsoft YaHei"))

;; Attempt to set the default font and Han font with fallbacks
(set-default-font preferred-default-fonts)
(set-han-font preferred-han-fonts 16)

;; visually show the indentation within the program
(use-package indent-guide
  :ensure t
  :custom
  (indent-guide-char "|")
  :custom-face
  (indent-guide-face ((t (:foreground "cadet blue"))))
  :config
  (indent-guide-global-mode))

;; volatile highlights - highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (ess-mode  . rainbow-delimiters-mode)))

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-hl-line '(intense))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-completions '((matches   . (extrabold))
                              (selection . (semibold accented))
                              (popup     . (accented intense))))
  (modus-operandi-tinted-palette-overrides '((comment fg-dim))))

;; sometimes want a bit more for themes
;; (use-package ef-themes
;;   :bind ("C-c m m" . ef-themes-toggle))

(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-time-live-icon t)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode))
  :hook
  (after-init . doom-modeline-mode))

(use-package circadian
  :custom
  (calendar-longitude 1.2576288)
  (calendar-latitude 51.7519826)
  (calendar-location-name "Oxford, United Kingdom")
  (circadian-themes '((:sunrise . modus-operandi-tinted)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package minimap)

(use-package darkroom
  :init
  ;; text scaling is a bit jarring, so this might help
  (setq darkroom-text-scale-increase 2)
  :bind ("<f6>" . darkroom-tentative-mode))

(use-package pulsar
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-pulse-on-window-change t)
  (pulsar-pulse t)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-magenta)
  (pulsar-delay 0.15)
  (pulsar-iterations 10)
  :bind
  ("C-x p l" . pulsar-pulse-line)
  ("C-x p h" . pulsar-highlight-line))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(provide 'init-ui)

;;; init-ui.el ends here

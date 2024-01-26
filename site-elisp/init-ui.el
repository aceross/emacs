;;; init-ui.el --- User Interface customisations
;;
;;; Commentary:
;;
;;  The customisations in this file relate to the visual aesthetics
;;  and function of my Emacs configuration.
;;

;;; Code:

;; TODO: Need to gracefully degrade if font not available
(set-face-attribute 'default nil
                    :family "Comic Code Ligatures" :weight 'normal)
(set-fontset-font t 'han (font-spec :name "Hiragino Sans GB" :size 14))

;; visually show the indentation within the program
(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode)
  (setq indent-guide-char "|")
  (set-face-foreground 'indent-guide-face "cadet blue"))

;; volatile highlights - highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (ess-mode  . rainbow-delimiters-mode))

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
  (modus-operandi-tinted-palette-overrides '((comment fg-dim)))
  )

;; sometimes want a bit more for themes
(use-package ef-themes
  :bind ("C-c m m" . ef-themes-toggle))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-continuous-word-count-modes
		'(markdown-mode gfm-mode org-mode))
  :hook
  (after-init . doom-modeline-mode))

(use-package circadian
  :config
  (setq calendar-longitude 1.2576288)
  (setq calendar-latitude 51.7519826)
  (setq calendar-location-name "Oxford, United Kingdom")
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Icons in dired and other places
;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :config
;;   (setq all-the-icons-scale-factor 1.3)
;;   (setq all-the-icons-dired-monochrome nil))

;; (use-package all-the-icons-dired
;;   :if (display-graphic-p)
;;   :hook (dired-mode . all-the-icons-dired-mode)
;;   :config (setq all-the-icons-dired-monochrome nil))

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package all-the-icons-completion
;;   :config
;;   (all-the-icons-completion-mode))

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
  :config
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-magenta)
  (setq pulsar-delay 0.15)
  (setq pulsar-iterations 10)
  :bind
  ("C-x p l" . pulsar-pulse-line)
  ("C-x p h" . pulsar-highlight-line)
  )

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

(provide 'init-ui)

;;; init-ui.el ends here

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
  :init
  (setq modus-themes-paren-match '(bold intense)
        modus-themes-org-blocks 'tinted-background
	modus-themes-completions '((matches   . (extrabold))
                                   (selection . (semibold accented))
                                   (popup     . (accented intense)))
	)
  )

;; sometimes want a bit more for themes
(use-package ef-themes
  :bind ("C-c m m" . ef-themes-toggle))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-major-mode-color-icon t)
  :hook
  (after-init . doom-modeline-mode))

(use-package circadian
  :config
  (setq calendar-longitude 1.2576288)
  (setq calendar-latitude 51.7519826)
  (setq calendar-location-name "Oxford, United Kingdom")
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Icons in dired and other places
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package minimap)

(use-package darkroom
  :bind ("<f6>" . darkroom-tentative-mode))

(use-package pulsar
  :config
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-magenta)
  (setq pulsar-delay 0.15)
  (setq pulsar-iterations 10)
  (pulsar-global-mode 1)
  :bind
  ("C-x p l" . pulsar-pulse-line)
  ("C-x p h" . pulsar-highlight-line)
  :hook
  (consult-after-jump-hook . pulsar-recenter-top)
  (consult-after-jump-hook . pulsar-reveal-entry)
  )

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|>" "<-" "<<-" "::" "!=" "=="))
  (global-ligature-mode t))

(provide 'init-ui)

;;; init-ui.el ends here

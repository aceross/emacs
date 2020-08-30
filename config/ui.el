;;; ui.el --- User Interface customisations
;;
;;; Commentary:
;;
;;  The customisations in this file relate to the visual aesthetics
;;  and function of my Emacs configuration.
;;

;;; Code:

;; customise the window system
(when window-system
  ;; set the size of the emacs window
  (setq inhibit-splash-screen t)  ; remove spash screen
  (tooltip-mode -1)               ; remove tooltip
  (tool-bar-mode -1)              ; remove toolbar
  (menu-bar-mode -1)              ; remove the menu bar
  (scroll-bar-mode -1)            ; remove the scrollbar
  (setq column-number-mode t)     ; display column numbers
  (blink-cursor-mode 0)           ; no blinking cursor
  )

(use-package diminish
  :ensure t)

(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)

(use-package all-the-icons
  :ensure t)
(setq inhibit-compacting-font-caches t)

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-major-mode-color-icon t)
  :hook
  (after-init . doom-modeline-mode))

;; TODO: automatically change according to sunrise and sunset
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("6:00" . modus-operandi)
                           ("17:45" . modus-vivendi)))
  (circadian-setup))

(use-package eterm-256color
  :ensure t)

;; see which are empty lines in the buffer
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; answer yes or no question with a single letter
(defalias 'yes-or-no-p 'y-or-n-p)

;; visually show the indentation within the program
(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode)
  (setq indent-guide-char "|")
  (set-face-foreground 'indent-guide-face "cadet blue"))

;; highlight current line
(global-hl-line-mode 1)

;; volatile highlights - highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; highlight matching parenthesis
(show-paren-mode 1)

;; stop beeping at me
(setq visible-bell t)

;; colour the delimiters to better identify nested constracts
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook 'rainbow-delimiters-mode))

;; show git diff in the buffer
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :commands eldoc-mode)

;; display modeline search information
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :diminish anzu-mode)

(use-package hl-todo
  :ensure t
  :config
  (hl-todo-mode t))


(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Chinese fontset
;; (set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK SC"))

(provide 'ui)

;;; ui.el ends here

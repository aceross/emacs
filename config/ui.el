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

(use-package battery
  :ensure t
  :config
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 99)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  :hook (after-init-hook . display-battery-mode))

(use-package time
  :ensure t
  :config
  (setq display-time-format "%H:%M  %Y-%m-%d")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init-hook . display-time-mode))


(use-package diminish
  :ensure t)

(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-org-blocks nil
        modus-themes-region 'bg-only-no-extend
        modus-themes-paren-match 'subtle-bold
        modus-themes-hl-line 'accented-background
        modus-themes-headings '((t . section))
        modus-themes-scale-headings t
        )
  (modus-themes-load-themes)
  )

;; (use-package modus-operandi-theme
;;   :ensure t
;;   :init
;;   (setq modus-operandi-theme-distinct-org-blocks t
;;         modus-operandi-theme-section-headings t
;;         modus-operandi-theme-scale-heaadings t))

;; (use-package modus-vivendi-theme
;;   :ensure t
;;   :init
;;   (setq modus-vivendi-theme-distinct-org-blocks t
;;         modus-vivendi-theme-section-headings t
;;         modus-vivendi-theme-scale-headings t))

(use-package all-the-icons
  :ensure t)
(setq inhibit-compacting-font-caches t)

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
                           ("17:00" . modus-vivendi)))
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

;; (use-package hl-todo
;;   :ensure t
;;   :config
;;   (hl-todo-mode t))


(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Chinese fontset
;; (set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK SC"))

(provide 'ui)

;;; ui.el ends here

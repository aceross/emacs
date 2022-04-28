;;; ui.el --- User Interface customisations
;;
;;; Commentary:
;;
;;  The customisations in this file relate to the visual aesthetics
;;  and function of my Emacs configuration.
;;

;;; Code:

;; customisations of the window system
(when window-system
  ;; set the size of the emacs window
  (setq inhibit-splash-screen t)  ; remove spash screen
  (tooltip-mode -1)               ; remove tooltip
  (tool-bar-mode -1)              ; remove toolbar
  (menu-bar-mode -1)              ; remove the menu bar
  (scroll-bar-mode -1)            ; remove the scrollbar
  (setq column-number-mode t)     ; display column numbers
  (blink-cursor-mode 0)           ; no blinking cursor
  (setq frame-resize-pixelwise t)
  )

(use-package diminish)

(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))
  
(use-package modus-themes
  :init
  (setq modus-themes-paren-match '(bold intense)
        modus-themes-org-blocks 'tinted-background
	modus-themes-completions '((matches   . (extrabold))
                                   (selection . (semibold accented))
                                   (popup     . (accented intense)))
	)
  (modus-themes-load-themes))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
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

(use-package ligature
  :disabled t
  :load-path "~/.emacs.d/elisp/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<-"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode)

(use-package anzu
  :init (global-anzu-mode +1)
  :diminish anzu-mode)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (ess-mode  . rainbow-delimiters-mode))

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun toggle-transparency ()
  "Switch between transparent and opaque frames."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t t") 'toggle-transparency)

(use-package darkroom
  :bind ("<f6>" . darkroom-tentative-mode))

(use-package minimap)

(provide 'init-ui)

;;; init-ui ends here

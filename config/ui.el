;;; ui.el --- User Interface customisations
;;
;;; Commentary:
;;

;;; Code:

;; customise the window system
(when window-system
  ;; set the size of the emacs window
  (setq initial-frame-alist '((top . 0) (left . 0) (width . 85) (height . 35)))
  (setq inhibit-splash-screen t)  ; remove spash screen
  (tooltip-mode -1)               ; remove tooltip
  (tool-bar-mode -1)              ; remove toolbar
  (menu-bar-mode -1)              ; remove the menu bar
  (scroll-bar-mode -1)            ; remove the scrollbar
  (setq column-number-mode t)     ; display column numbers
  (blink-cursor-mode 0)           ; no blinking cursor
  )

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-light t :no-confirm))

(use-package powerline
  :ensure t
  :config)

;; modeline theming
(use-package airline-themes
  :ensure t
  :config
  (load-theme 'airline-raven t)
  (setq airline-shortened-directory-length 20)
  ;; add the glyphs for git-branch, seperators, line numbers, etc
  (setq airline-utf-glyph-separator-left      #xe0b0
	airline-utf-glyph-separator-right     #xe0b2
	airline-utf-glyph-subseparator-left   #xe0b1
	airline-utf-glyph-subseparator-right  #xe0b3
	airline-utf-glyph-branch              #xe0a0
	airline-utf-glyph-readonly            #xe0a2
	airline-utf-glyph-linenumber          #xe0a1))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; answer yes or no question with a single letter
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode)
  (setq indent-guide-char "|")
  (set-face-foreground 'indent-guide-face "cadet blue")
  )

;; highlight current line
(global-hl-line-mode 1)

;; volatile highlights - highlight changes from pasting etc
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; highlight matching parenthesis
(show-paren-mode 1)

;; show highlights for indentation
(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode
  :config
  (add-hook 'prog-mode-hook #'highlight-indentation-mode)
  (add-hook 'c-mode-hook       'highlight-indentation-mode)
  (add-hook 'c++-mode-hook     'highlight-indentation-mode)
  (add-hook 'emacs-lisp-mode   'highlight-indentation-mode)
  (add-hook 'python-mode-hook  'highlight-indentation-mode)
  (add-hook 'java-mode-hook    'highlight-indentation-mode)
  (add-hook 'clojure-mode-hook 'highlight-indentation-mode)
  (add-hook 'lisp-mode-hook    'highlight-indentation-mode)
  (add-hook 'js-mode-hook      'highlight-indentation-mode)
  )

;; colour the delimiters to better identify nested constracts
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
  )

;; show git diff in the buffer
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode)
  )

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode)
  )

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)

;; display modeline search information
(use-package anzu
  :init (global-anzu-mode +1)
  :diminish anzu-mode)

;; Chinese fontset
(set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK SC"))

(provide 'ui)

;;; ui.el ends here
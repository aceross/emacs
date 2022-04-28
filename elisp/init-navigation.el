;;; navigation.el
;;
;;  Copyright (c) 2022, Aaron Ceross
;;
;;
;;; Commentary:
;;
;;  Navigation module for Emacs providing convenience in moving around
;;
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)
  (setq ivy-dynamic-exhibit-delay-ms 200)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :disabled
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-center)
          (t               . ivy-posframe-display)))
  (setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file)))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package avy
  :bind (("C-c gc". avy-goto-char)
         ("C-'"   . avy-goto-line)
         ("M-g c" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g P" . avy-pop-mark)))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(provide 'init-navigation)

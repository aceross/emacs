;;; navigation.el --- Navigation module for Emacs configuration ----------------
;;
;;
;;  Copyright (c) 2016, Aaron Ceross
;;
;;  URL: https://gitlab.com/awc/emacs
;;
;;
;;; Commentary:
;;
;;  Navigation module for Emacs providing convenience in moving around
;;
;; -----------------------------------------------------------------------------

;;; Code:

;; vim bindings
;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :custom (evil-collection-setup-minibuffer t)
;;   :config (evil-collection-init))

;; (use-package evil-escape
;; ;;  :after evil
;;   :ensure t
;;   :config
;;   (setq-default evil-escape-key-sequence "jk")
;;   (setq evil-escape-mode t))

;; (use-package evil-magit
;;   :after evil
;;   :ensure t)

;; Smex - provide recent and most used commands
(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package ivy
  :ensure t
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

(use-package ivy-posframe
  :ensure t
  :disabled
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-center)
          (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file)))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode))

;; (use-package projectile
;;   :init
;;   (projectile-mode)
;;     :bind (("C-c p s" . projectile-ag)
;;            ("C-c p g" . projectile-grep)
;;            ("C-c p R" . projectile-regenerate-tags))
;;     :config
;;     (setq projectile-switch-project-action 'projectile-commander
;;         projectile-completion-system 'ido
;;         projectile-create-missing-test-files t)
;;   (define-key projectile-mode-map [remap projectile-ack] #'projectile-ag)
;;   :diminish projectile-mode)

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package avy
  :ensure t
  :bind (("C-c gc". avy-goto-char)
         ("C-'"   . avy-goto-line)
         ("M-g c" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g P" . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;; Windmove configuration
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; control window placement with C-c <arrow>
(use-package winner
  :init (winner-mode 1))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(provide 'navigation)

;;; navigation.el ends here

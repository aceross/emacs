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

(use-package ag
  :defines my-ag-keymap
  :bind-keymap ("C-c a" . my-ag-map)
  :config
  (setq ag-reuse-buffers t    ; Don't spam buffer list with ag buffers
	ag-highlight-search t ; A little fanciness
	;; Use Projectile to find the project root
	ag-project-root-function
	(lambda (d)
	  (let ((default-directory d))
	    (projectile-project-root))))
  (defvar my-ag-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") #'ag-regexp)
      (define-key map (kbd "p") #'ag-project-regexp)
      map)))

(use-package ido
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
	ido-enable-flex-matching t
	ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
	ido-max-prospects 10
	ido-default-file-method 'selected-window
	ido-everywhere t
	ido-use-virtual-buffers t  ; only try to match within the work directory
	ido-max-window-height 0.25
	ido-auto-merge-work-directories-length -1)
  (ido-mode t)
  (use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode t))
  (use-package ido-ubiquitous
    :ensure t
    :config (ido-ubiquitous-mode t))
  (use-package flx-ido
    :ensure t
    :init (flx-ido-mode t))
  (use-package idomenu :ensure t))

;; Smex - provide recent and most used commands
(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package projectile
  :init
  (projectile-mode)
    :bind (("C-c p s" . projectile-ag)
	   ("C-c p g" . projectile-grep)
	   ("C-c p R" . projectile-regenerate-tags))
    :config
    (setq projectile-switch-project-action 'projectile-commander
	projectile-completion-system 'ido
	projectile-create-missing-test-files t)
  (define-key projectile-mode-map [remap projectile-ack] #'projectile-ag)
  :diminish projectile-mode)

;; Windmove configuration
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; control window placement with C-c <arrow>
(use-package winner
  :ensure t
  :init (winner-mode 1))

(provide 'navigation)

;;; navigation.el ends here

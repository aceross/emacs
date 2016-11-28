;;; navigation.el --- Navigation module for Emacs configuration ----------------
;;
;;
;;  Copyright (c) 2016 Aaron Ceross
;;
;;  URL: https://gitlab.com/awc/emacs
;;
;;
;;; Commentary:
;;
;;  Navigation module for Emacs providing convenience in moving around
;; -----------------------------------------------------------------------------

;;; Code:

(use-package ido
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only
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

;; Windmove configuration
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; ztree - provides a visual file tree
(use-package ztree
  :ensure t
  :defer t)

(provide 'navigation)

;;; navigation.el ends here

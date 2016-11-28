;;;; navigation.el
;;; moving around Emacs

(use-package smex
  :init (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  )

;; ido
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere)
  (ido-mode)
  (setq ido-use-virtual-buffers t) ; only try to match within the work directory
  (setq ido-max-prospects 50)
  (setq ido-max-window-height 0.25)
  (setq ido-auto-merge-work-directories-length -1)
  ;; see all the buffers
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

(use-package flx-ido
  :config
  (flx-ido-mode))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode)
  ;; use up and down to scroll through ido instead of left-right
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  )

;; Windmove configuration
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
  (funcall fn)))))

(global-set-key [s-left]  (ignore-error-wrapper 'windmove-left))
(global-set-key [s-right] (ignore-error-wrapper 'windmove-right))
(global-set-key [s-up]    (ignore-error-wrapper 'windmove-up))
(global-set-key [s-down]  (ignore-error-wrapper 'windmove-down))

;; file structure
(use-package ztree
  :ensure t
  :defer t)


(provide 'navigation)

;;; navigation.el ends here.

;;; editing.el --- Editing module for my Emacs configuration -------------------
;;
;;  Copywrite (c) 2016, Aaron Ceross
;;
;;; Commentary:
;;
;;  This file contains the editing customisations, base autocompletes, magit
;;
;; -----------------------------------------------------------------------------

;;; Code:

;; magit
(use-package magit
  :ensure t
  :defer t
  :diminish auto-revert-mode
  :bind (("C-x g" . magit-status)))

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

(use-package smartparens
  :ensure t
  :config (show-smartparens-global-mode 1))

;; ensure EOF newline on save
(setq require-final-newline t)

;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq tab-width 2
      indent-tabs-mode nil)

;; turn tabs to spaces on save
(add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))

;; undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; kill ring
(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :config
  (progn
    (browse-kill-ring-default-keybindings)))

;; snippets for various languages
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets/")))
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "C-c yi") 'yas-insert-snippet))

;; company mode
(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
	      ("TAB" . nil)
	      ("<tab>" . nil))
  :init (global-company-mode t)
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  )

(use-package company-quickhelp  ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; error linting
(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (global-flycheck-mode t)
  (use-package flycheck-pos-tip
    :ensure t
    :config (flycheck-pos-tip-mode)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook geiser-mode-hook
				       clojure-mode-hook))
    (add-hook hook 'paredit-mode))
  :config
  (autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  )

;; align with spaces, not tabs
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(progn
	  (delete-file filename)
	  (message "Deleted file %s" filename)
	  (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

;; write-good mode
(use-package writegood-mode
  :ensure t
  :diminish writegood-mode
  :config
  (global-set-key "\C-c\C-gg" 'writegood-grade-level)
  (global-set-key "\C-c\C-ge" 'writegood-reading-ease)
  )

(provide 'editing)

;;; editing.el ends here

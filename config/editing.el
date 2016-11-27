;;;; editing.el
;;; This file contains the editing customisations, base autocompletes, magit

;; magit
(use-package magit
  :ensure t
  :bind (("<f5>" . magit-status))
  :config (diminish 'magit-auto-revert-mode))

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode)
  )

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :config
  (autopair-global-mode)
  )

;; ensure EOF newline on save
(setq require-final-newline t)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
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
  :defer t
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
  (define-key yas-minor-mode-map (kbd "C-c yi") 'yas-insert-snippet)
  )

;; company mode
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (progn
    (setq company-backends
       '(company-capf
	 (company-dabbrev-code company-gtags company-etags company-keywords)
	 company-cmake
	 company-dabbrev
	 company-files
	 company-irony-c-headers
	 company-irony
	 company-tern
	 company-jedi
	 company-web-html)))
  )

;; error linting
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  )

(use-package paredit
  :ensure t
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
  :config
  (global-set-key "\C-c\C-gg" 'writegood-grade-level)
  (global-set-key "\C-c\C-ge" 'writegood-reading-ease)
  )


(provide 'editing)

;;; editing.el ends here.

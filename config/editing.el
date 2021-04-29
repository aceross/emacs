;;; editing.el --- Editing module for my Emacs configuration
;;
;;  Copyright (c) 2016 - 2017, Aaron Ceross
;;
;;; Commentary:
;;
;;  This file contains the editing customisations, base autocompletes, magit
;;
;; -----------------------------------------------------------------------------

;;; Code:

;; set utf-8 as preferred coding system
(set-language-environment "UTF-8")

;; set the language and dictionary
(use-package flyspell
  :ensure t
  :diminish (flyspell-mode . "spell")
  :config
  (set-face-attribute 'flyspell-incorrect nil
                      :underline '(:color "firebrick") :weight 'bold)
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq ispell-dictionary "british")
  (setq ispell-check-comments t)
  (setq ispell-really-hunspell t)
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist
      `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil
         utf-8))))

;; ;; turn on flyspell in desired modes
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; ;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (setq ispell-dictionary "british")
;; (setq ispell-check-comments t)
;; (setq ispell-really-hunspell t)
;; (setq ispell-program-name "hunspell")
;; (setq ispell-local-dictionary-alist
;;       `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil
;; utf-8)))

(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; magit
(use-package magit
  :ensure t
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
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

;; (use-package autopair
;;   :ensure t
;;   :config (autopair-global-mode)
;;   :diminish autopair-mode)

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

;; set all tabs to spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; make tab key indent first, then complete
(setq-default tab-always-indent 'complete)

;; turn tabs to spaces on save
(add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))

;; undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :defer t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; kill ring
(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind ("C-c yi" . 'yas-insert-snippet)
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets/")))
  (yas-global-mode))

(use-package ivy-yasnippet
  :ensure t)

;; company mode
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode t))

(use-package company
  :ensure t
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
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  :diminish company-mode
  )

;; error linting
(use-package flycheck
  :ensure t
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (global-flycheck-mode t)
  (use-package flycheck-pos-tip
    :ensure t
    :config (flycheck-pos-tip-mode))
  :diminish flycheck-mode)

(flycheck-define-checker proselint
  "A linter for prose."
  :ensure t
  :defer t
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode LaTeX-mode))

(add-to-list 'flycheck-checkers 'proselint)

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  geiser-mode-hook
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
  :defer t
  :diminish writegood-mode
  :config
  :bind (("C-c gg" . writegood-grade-level)
         ("C-c ge" . writegood-reading-ease))
  :hook (text-mode org-mode markdown-mode gfm-mode))

(provide 'editing)

;;; editing.el ends here

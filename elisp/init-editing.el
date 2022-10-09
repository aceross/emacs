;; set utf-8 as preferred coding system
(set-language-environment "UTF-8")

(use-package evil-nerd-commenter	
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package company
  :init
  (global-company-mode t)
  :config
  (setq company-show-quick-access t)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; TODO: move to navigation
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/work")
    (setq projectile-project-search-path '("~/Documents/work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (setq smartparens-global-mode t)
  (custom-set-variables
     '(sp-base-key-bindings 'sp)
     '(sp-override-key-bindings
       '(("C-S-<left>" . sp-backward-slurp-sexp)
         ("C-S-<right>" . sp-backward-barf-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-S-k" . sp-kill-hybrid-sexp)
         ("C-c C-<right>" . sp-slurp-hybrid-sexp)
         ("C-(" . sp-rewrap-sexp)
         ("C-M-<backspace>" . sp-splice-sexp-killing-around)
         ("C-S-<backspace>" . nil))))
  :hook
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  :diminish smartparens-mode)

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (use-package flycheck-pos-tip
    :config (flycheck-pos-tip-mode t))
  :diminish flycheck-mode)

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode gfm-mode text-mode)
  )
(add-to-list 'flycheck-checkers 'vale 'append)

;; (use-package flycheck-vale)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))
(add-to-list 'flycheck-checkers 'proselint)

(use-package yasnippet)

(provide 'init-editing)
;;; init-editing.el ends here  

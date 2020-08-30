;;; init-ess.el --- Customisations for the Emacs Speaks Statistics package
;;
;;; Commentary:
;;
;;  Mostly font and readability stuff.

;;; Code:

(use-package ess
  :ensure t
  :defer t
  :init (require 'ess-site)
  :bind
    (:map ess-mode-map
          (";" . ess-insert-assign))
    (:map inferior-ess-mode-map
          (";" . ess-insert-assign))
    :config
    ;; R-specific config
    (setq inferior-R-program-name "/usr/bin/R")
    (setq ess-use-flymake nil)
    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants . t)
            (ess-fl-keyword:fun-calls .t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators)
            (ess-fl-keyword:delimiters)
            (ess-fl-keyword:=)
            (ess-R-fl-keyword:F&T . t)
            ))
    (global-set-key (kbd "C-j") 'ess-eval-line-and-step)
    ;;(global-set-key (kbd "C-M-j") 'ess-eval-region)
    (global-set-key (kbd "C-M-j") 'ess-eval-region-and-go)
    ;; font-lock for R interpreter
    (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)))
    (setq ess-help-own-frame 'one)
    (setq ess-tab-complete-in-script t)
    (setq ess-first-tab-never-complete
          'symbol-or-paren-or-punct)
    )

;; (require 'ess-site)
;; (require 'ess-smart-underscore)

(provide 'init-ess)

;;; init-ess.el ends here

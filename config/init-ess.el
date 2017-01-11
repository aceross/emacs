;;; init-ess.el --- Customisations for the Emacs Speaks Statistics package
;;
;;; Commentary:
;;
;;  Mostly font and readability stuff.

;;; Code:

(use-package ess
  :ensure t
  :defer t
  :commands R
  :config
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
	    (ess-R-fl-keyword:%op% . t)))
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

  (defun my-ess-init ()
    "Init my ess mode."
    (setq ess-help-own-frame 'one)
    (setq ess-tab-complete-in-script t)
    (setq ess-first-tab-never-complete
	  'symbol-or-paren-or-punct))

  (add-hook 'ess-mode-hook #'my-ess-init)

  ;; prettify <- and %>% symbols
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'ess-mode-hook
            (lambda ()
	      (push '("%>%" . ?|) prettify-symbols-alist)
	  ))
    (add-hook 'inferior-ess-mode-hook
	    (lambda ()
	      (push '("%>%" . ?|) prettify-symbols-alist)
	  ))
    (global-prettify-symbols-mode +1))


  )

(provide 'init-ess)

;;; init-ess.el ends here

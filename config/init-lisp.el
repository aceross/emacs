;;; init-lisp.el --- Module for Common Lisp customisations
;;
;;; Commentary:
;;
;;; Code:

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(use-package slime-company
  :ensure t)

(use-package slime
  :commands slime
  :config
  (progn
    (add-hook
     'lisp-mode-hook
     (lambda () (slime-mode t)))
    'inferior-lisp-mode-hook
    (lambda () (inferior-slime-mode t)))
  (setq slime-contribs '(slime-fancy slime-company))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-repl))
  )

(use-package common-lisp-snippets
  :ensure t
  :defer t)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'init-lisp)

;;; init-lisp.el ends here

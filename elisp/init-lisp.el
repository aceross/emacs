;;; init-lisp.el --- Module for Lisp customisations
;;
;;; Commentary:
;;
;;; Code:

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(use-package slime
  :commands slime
  :functions slime-setup
  :config
  (setq slime-contribs '(slime-fancy slime-company))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-repl
		 slime-autodoc
		 slime-fancy
		 slime-fancy-inspector
		 slime-company
		 slime-highlight-edits
		 slime-quicklisp
		 slime-asdf))
  (setq slime-complete-symbol*-fancy t)
  )

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

(provide 'init-lisp)

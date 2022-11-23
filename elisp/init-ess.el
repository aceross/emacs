;;; init-ess.el --- Customisations for the Emacs Speaks Statistics package
;;
;;; Commentary:
;;
;;  Mostly font and readability stuff.

;;; Code:

(defun insert-r-pipe-operator()
  "R - |> operator or 'then' pipe operator."
  (interactive
   (insert "|>")
   (reindent-then-newline-and-indent)))

;; TODO: This is prone to errors and janky. Needs tons of work to act
;; like running tests in elpy, which is what I want. For the moment, this is fine.
(defun run-r-tests()
  "Insert the command for running test in the project. It is always
assumed that we are running in /R folder and the tests are in the
/tests folder in a project."
  (interactive
   (insert "testthat::test_dir('../tests')")
   ))

(use-package ess
  :defer t
  :init (require 'ess-site)
  :bind
    (:map ess-mode-map
          (";" . ess-insert-assign)
	  ("C-;" . insert-r-pipe-operator)
          )
    (:map inferior-ess-mode-map
          (";" . ess-insert-assign)
	  ("C-;" . insert-r-pipe-operator)
          ("C-c r t" . run-r-tests)
	  )
    :config
    (setq ess-use-flymake nil)
    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants . t)
            (ess-fl-keyword:fun-calls .t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
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
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)))
    (setq ess-help-own-frame 'one)
    (setq ess-tab-complete-in-script t)
    (setq ess-first-tab-never-complete
          'symbol-or-paren-or-punct)
    )

;;TODO:
(defun awc/test-R-project (&optional test-whole-project)
  "Run unittests for R in project.")

(use-package poly-markdown)

(use-package poly-R)

(use-package polymode
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode)))

(defun r-function (function-name)
  "Create and name a function in R"
  (interactive "sFunction name: ")
  (insert (concat function-name " <- function() {\n\n}"))
  (forward-line -1))


(define-skeleton rmd-skeleton-header
  "Rmd headers with focus on LaTex output."
  "---\n"
  "title: " str | (buffer-name) "\n"
  "author: " (user-full-name) "\n"
  "documentclass: article\n"
  "---\n"
  )

(defun rmd-insert-r-chunk (header)
  "Insert an r-chunk in markdown mode and name using HEADER."
  (interactive "sLabel: ")
  (insert (concat "```{r " header ", echo=FALSE}\n\n```"))
  (forward-line -1))
(global-set-key (kbd "C-c i") 'rmd-insert-r-chunk)

(provide 'init-ess)

;;; init-ess.el ends here

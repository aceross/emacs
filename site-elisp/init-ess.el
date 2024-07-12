;;; init-ess.el --- Customizations for Emacs Speaks Statistics (ESS)

;;; Commentary:
;;
;;  This file provides customizations and settings for Emacs Speaks Statistics (ESS),
;;  tailored to enhance R programming workflow within Emacs. It includes keybindings,
;;  syntax highlighting configurations, and buffer display settings optimised for
;;  interacting with R scripts, consoles, and environment variables.

;;; Code:

(defun insert-r-pipe-operator()
  "R - |> operator or 'then' pipe operator."
  (interactive
   (insert "|>")
   (reindent-then-newline-and-indent)))

(use-package ess
  :defer t
  :init
  (require 'ess-site)
  :bind
  (:map ess-mode-map
        (";"   . ess-insert-assign)
        ("C-;" . insert-r-pipe-operator))
  (:map inferior-ess-mode-map
        (";"       . ess-insert-assign)
        ("C-;"     . insert-r-pipe-operator)
        ("C-c r t" . run-r-tests))
  :custom
  (ess-use-flymake nil)
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)))
  (inferior-R-font-lock-keywords
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
  (ess-help-own-frame 'one)
  (ess-tab-complete-in-script t)
  (ess-first-tab-never-complete 'symbol-or-paren-or-punct)
  :config
  (global-set-key (kbd "C-j") 'ess-eval-line-and-step)
  (global-set-key (kbd "C-M-j") 'ess-eval-region-and-go))

(use-package tree-sitter-ess-r
  :ensure t
  :after (ess)
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate))

;; TODO:
(defun awc/test-R-project (&optional test-whole-project)
  "Run unittests for R in project.")

;; (defun r-function (function-name)
;;   "Create and name a function in R"
;;   (interactive "sFunction name: ")
;;   (insert (concat function-name " <- function() {\n\n}"))
;;   (forward-line -1))

;; (define-skeleton rmd-skeleton-header
;;   "Rmd headers with focus on LaTex output."
;;   "---\n"
;;   "title: " str | (buffer-name) "\n"
;;   "author: " (user-full-name) "\n"
;;   "documentclass: article\n"
;;   "---\n"
;;   )

;; (defun rmd-insert-r-chunk (header)
;;   "Insert an r-chunk in markdown mode and name using HEADER."
;;   (interactive "sLabel: ")
;;   (insert (concat "```{r " header ", echo=FALSE}\n\n```"))
;;   (forward-line -1))
;; (global-set-key (kbd "C-c i") 'rmd-insert-r-chunk)

;; Quarto is Posit's new literate programming paradigm
;; (use-package quarto-mode
;;   :mode (("\\.qmd" . poly-quarto-mode))
;;   :bind (("C-c q p" . quarto-preview))
;; )

;; (use-package quarto-mode
;;   :mode ("\\.qmd\\'" . poly-quarto-mode)
;;   :bind (("C-c q p" . quarto-preview))
;;   :config
;;   ;; Set up Python execution in Quarto mode
;;   (defun quarto-python-send-chunk ()
;;     "Send the current Python chunk to the Python interpreter."
;;     (interactive)
;;     (save-excursion
;;       (let ((chunk (quarto--current-code-chunk)))
;;         (when (string= (quarto-code-chunk-language chunk) "python")
;;           (let ((code (quarto-code-chunk-body chunk)))
;;             (python-shell-send-string code))))))

;;   ;; Bind key for sending Python chunks
;;   ;; (define-key quarto-mode-map (kbd "C-c C-c") 'quarto-python-send-chunk)

;;   ;; Activate virtual environment when opening Quarto files
;;   (add-hook 'quarto-mode-hook
;;             (lambda ()
;;               (let ((venv-path (concat (projectile-project-root) ".venv")))
;;                 (when (file-exists-p venv-path)
;;                   (pyvenv-activate venv-path)))))
;; )

(provide 'init-ess)

;;; init-ess.el ends here

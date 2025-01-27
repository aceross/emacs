;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; This mode provides basic syntax highlighting, indentation, and setup for Quarto (.qmd) documents.
;; It supports R and Python code chunks, as well as Quarto-specific elements like YAML metadata.

;;; Code:

(require 'generic-x) ;; For generic mode support
(require 'lsp-mode) ;; For LSP integration
(require 'ess) ;; For R support (via ESS)
(require 'python) ;; For Python support

(define-generic-mode 'qmd-mode
  ;; Comments start with # (e.g., in YAML or R/Python chunks)
  '("#")
  ;; Keywords for syntax highlighting
  '("title:" "author:" "date:" "output:" "params:" ;; YAML metadata
    "#" "##" "###" "####" ;; Headings
    "```{r}" "```{python}" "```" ;; Code chunks
    )
  ;; File extensions
  '("\\.qmd$")
  ;; Additional functions to run
  '(qmd-setup)
  "A major mode for editing Quarto (.qmd) files.")

(defun qmd-setup ()
  "Setup function for `qmd-mode`."
  (setq font-lock-defaults '((
                              ;; YAML metadata
                              ("^\(title\|author\|date\|output\):" . font-lock-keyword-face)
                              ;; Headings
                              ("^#.*" . font-lock-function-name-face)
                              ;; Code chunks
                              ("^```{.*}" . font-lock-builtin-face)
                              ))))

(defun qmd-detect-language ()
  "Detect the language of the current code chunk in a Quarto file."
  (save-excursion
    (when (re-search-backward "^```{\(.*?\)}" nil t)
      (match-string 1))))

(defun qmd-lsp-activate ()
  "Activate LSP for the current code chunk based on its language."
  (let ((lang (qmd-detect-language)))
    (cond
     ((string= lang "r")
      (message "Activating LSP for R")
      (lsp-deferred))
     ((string= lang "python")
      (message "Activating LSP for Python")
      (lsp-deferred)))))

(defun qmd-setup-lsp ()
  "Setup LSP support for Quarto code chunks."
  (add-hook 'post-command-hook #'qmd-lsp-activate nil t))

(add-hook 'qmd-mode-hook #'qmd-setup-lsp)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

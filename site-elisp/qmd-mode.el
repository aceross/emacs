;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; This mode provides basic syntax highlighting, indentation, and
;; setup for Quarto (.qmd) documents. It supports R and Python code
;; chunks, as well as Quarto-specific elements like YAML metadata.

;;; Code:

(require 'generic-x) ;; For generic mode support

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

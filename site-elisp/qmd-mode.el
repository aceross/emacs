;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple major mode for editing Quarto (.qmd) files. Provides
;; syntax highlighting for YAML metadata, headings, and code chunk
;; delimiters.

;;; Code:

(require 'generic-x) ;; For generic mode support

(define-generic-mode 'qmd-mode
  ;; Comments start with #
  '("#")
  ;; Keywords for syntax highlighting
  '("title:" "author:" "date:" "output:" "params:" ;; YAML metadata
    "#" "##" "###" "####" ;; Headings
    "```{r}" "```{python}" "```" ;; Code chunks
    )
  ;; File extensions
  '("\\.qmd$")
  ;; Additional functions to run
  nil ;; No functions for now
  "A major mode for editing Quarto (.qmd) files.")

(provide 'qmd-mode)
;;; qmd-mode.el ends here

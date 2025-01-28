;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom major mode for editing Quarto (.qmd) files. Provides syntax
;; highlighting for YAML metadata, headings, and code chunks. Future
;; steps will include REPL integration and LSP support.

;;; Code:

(require 'generic-x) ;; For generic mode support

;;;###autoload
(define-generic-mode 'qmd-mode
  ;; Comments start with #
  '("#")
  ;; Keywords for syntax highlighting
  '("title:" "author:" "date:" "output:" "params:" ;; YAML metadata
    "#" "##" "###" "####" ;; Headings
    "```{r}" "```{python}" "```" ;; Code chunks
    )
  ;; File extensions
  '("\.qmd\'")
  ;; Additional functions to run
  nil ;; Placeholder for later enhancements
  "A major mode for editing Quarto (.qmd) files.")

;;; Ensure .qmd files are associated with qmd-mode
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

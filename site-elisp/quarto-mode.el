;;; quarto-mode.el --- Major mode for Quarto documents in Emacs -*- lexical-binding: t; -*-

;; Author: aceross
;; Version: 0.1
;; Keywords: quarto, markdown, reproducible research
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.3") (poly-markdown "1.1.2") (ess "18.10") (python-mode "0.26.1"))

;;; Commentary:
;; This major mode provides support for editing Quarto documents (.qmd) in Emacs.
;; It extends `markdown-mode` to handle Quarto-specific syntax and integrates
;; support for R and Python execution within code blocks.

;;; Code:

(require 'markdown-mode)
(require 'poly-markdown)
(require 'ess)  ;; For R support
(require 'python)  ;; For Python support

(define-derived-mode quarto-mode markdown-mode "Quarto"
  "Major mode for editing Quarto (.qmd) documents."
  ;; Syntax highlighting and additional settings will be added here.
  )

(provide 'quarto-mode)

;;; quarto-mode.el ends here

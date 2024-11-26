;;; qmd-mode.el --- Major mode for editing Quarto documents -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: Your Name
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1") (polymode "0.2.2") (markdown-mode "2.5") (lsp-mode "8.0.0") (yasnippet "0.14.0") (tree-sitter "0.16.1"))

;;; Commentary:
;; This package provides a major mode for editing Quarto (.qmd) files,
;; with support for syntax highlighting, LSP-based autocompletion, and REPL integration.

;;; Code:

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)


(use-package polymode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(defun qmd-lsp-get-language ()
  "Determine the language for LSP based on the current code chunk."
  (save-excursion
    (when (re-search-backward "^```{\\([^}]+\\)}" nil t)
      (match-string 1))))

(defun qmd-lsp-setup ()
  "Setup LSP for the current Quarto buffer based on the code chunk language."
  (interactive)
  (let ((lang (qmd-lsp-get-language)))
    (cond
     ((string= lang "python")
      (require 'lsp-python-ms)
      (setq-local lsp-language-id-configuration
                  (cons '(qmd-mode . "python") lsp-language-id-configuration))
      (lsp-deferred))

     ((string= lang "r")
      (require 'lsp-r)
      (setq-local lsp-language-id-configuration
                  (cons '(qmd-mode . "r") lsp-language-id-configuration))
      (lsp-deferred))

     ((string= lang "julia")
      (require 'lsp-julia)
      (setq-local lsp-language-id-configuration
                  (cons '(qmd-mode . "julia") lsp-language-id-configuration))
      (lsp-deferred))

     (t (message "No LSP support found for language: %s" (or lang "unknown"))))))

;;;###autoload
(define-derived-mode qmd-mode markdown-mode "Quarto"
  "Major mode for editing Quarto documents."

  ;; Ensure syntax table inherits from markdown-mode
  :syntax-table markdown-mode-syntax-table

  ;; Additional setup for qmd-specific features
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")

  ;; LSP configuration
  (add-hook 'lsp-mode-hook #'qmd-lsp-setup nil t)

  ;; Enable LSP
  (lsp-mode 1)

  ;; Font-lock keywords specific to Quarto
  (font-lock-add-keywords
   nil
   '(("^```{.*}$" . 'font-lock-function-name-face)
     ("^```$" . 'font-lock-function-name-face)
     ("^---\n\\(.*:\n\\)+---" . 'font-lock-keyword-face)))

  ;; Custom keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-c") 'qmd-send-chunk-to-repl)
    (define-key map (kbd "C-c C-r") 'qmd-render-document)
    (define-key map (kbd "C-c C-p") 'qmd-preview-document)
    (use-local-map map)))

;; File association
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . qmd-mode))

;; Rest of the previous implementation remains the same
(defun qmd-send-chunk-to-repl ()
  "Send the current code chunk to the appropriate REPL."
  (interactive)
  (let ((lang (qmd-get-chunk-language)))
    (cond
     ((string= lang "python")
      (unless (get-buffer "*Python*")
        (run-python))
      (when-let ((chunk (qmd-get-current-chunk)))
        (python-shell-send-string chunk)))

     ((string= lang "r")
      (unless (get-buffer "*R*")
        (R))
      (when-let ((chunk (qmd-get-current-chunk)))
        (ess-eval-string chunk)))

     ((string= lang "julia")
      (unless (get-buffer "*julia*")
        (julia-repl))
      (when-let ((chunk (qmd-get-current-chunk)))
        (julia-shell-send-string chunk)))

     (t (message "No REPL found for language: %s" (or lang "unknown"))))))

(defun qmd-get-chunk-language ()
  "Determine the language of the code chunk at point."
  (save-excursion
    (when (re-search-backward "^```{\\([^}]+\\)}" nil t)
      (match-string 1))))

(defun qmd-get-current-chunk ()
  "Extract the content of the current code chunk."
  (save-excursion
    (when (re-search-backward "^```{.*}$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^```$" nil t)
          (forward-line -1)
          (buffer-substring-no-properties start (point)))))))

(defun qmd-render-document ()
  "Render the current Quarto document."
  (interactive)
  (compile (format "quarto render %s" buffer-file-name)))

(defun qmd-preview-document ()
  "Preview the current Quarto document."
  (interactive)
  (compile (format "quarto preview %s" buffer-file-name)))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

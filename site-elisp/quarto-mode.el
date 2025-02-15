;;; quarto-mode.el --- Major mode for Quarto (qmd) documents -*- lexical-binding: t; -*-

;; Author: Aaron Ceross
;; Version: 0.2
;; Keywords: quarto, markdown, polymode
;; Package-Requires: ((emacs "26.1") (polymode "0.2.2") (markdown-mode "2.3") (ess "18.10.2"))

;;; Commentary:
;; This configuration defines a Quarto mode using polymode with inner modes for R and Python.
;; It disables tree-sitter highlighting, highlight-indent-guides, and LSP in the outer (Markdown)
;; buffer to help prevent erratic cursor behavior.
;;
;; It provides:
;;   - A command (C-c C-c) to send the current code block to the appropriate REPL.
;;   - A build command (C-c C-b) to render the current Quarto document.
;;   - A preview command (C-c C-p) to preview the current document.
;;; Code:

(require 'polymode)
(require 'markdown-mode)
(require 'ess)        ;; for ess-r-mode
(require 'python)     ;; for python-mode

;; --- Disable Tree-sitter and Highlight-Indent-Guides in the Outer Mode ---
(defun my/poly-markdown-disable-extras ()
  "Disable tree-sitter-hl, highlight-indent-guides, and LSP in the outer markdown buffer."
  (when (bound-and-true-p tree-sitter-hl-mode)
    (tree-sitter-hl-mode -1))
  (when (bound-and-true-p highlight-indent-guides-mode)
    (highlight-indent-guides-mode -1))
  ;; Optionally disconnect LSP in the outer buffer if active.
  (when (bound-and-true-p lsp-mode)
    (lsp-disconnect)))
(add-hook 'poly-markdown-mode-hook #'my/poly-markdown-disable-extras)

;; --- Define R Innermode ---
(define-innermode poly-quarto-r-innermode
  :mode 'ess-r-mode
  :head-matcher "^```{[rR].*}\\(?:\n\\|$\\)"
  :tail-matcher "^```\\s-*$"
  :head-mode 'host
  :tail-mode 'host)

;; --- Define Python Innermode ---
(define-innermode poly-quarto-python-innermode
  :mode 'python-mode
  :head-matcher "^```{[pP]ython.*}\\(?:\n\\|$\\)"
  :tail-matcher "^```\\s-*$"
  :head-mode 'host
  :tail-mode 'host)

;; --- Define the Quarto Polymode ---
(define-polymode poly-quarto-mode poly-markdown-mode
  "Polymode for Quarto (qmd) files.
Automatically switches to the appropriate mode for code blocks."
  :lighter " Quarto"
  :innermodes '(poly-quarto-r-innermode
                poly-quarto-python-innermode))

;; --- Command: Send Current Code Block to REPL ---
(defun quarto-send-code-block-to-repl ()
  "Send the current Quarto code block to the appropriate REPL.
This command only works if the point is in a code block (inner region).
For R, uses ESS; for Python, uses the Python shell."
  (interactive)
  (if (not (member major-mode '(ess-r-mode python-mode)))
      (message "Not in a code block")
    (let (lang start end code)
      (save-excursion
        (when (re-search-backward "^```{\\([^,}]+\\)" nil t)
          (setq lang (downcase (car (split-string (match-string 1) "[, ]+" t))))
          (forward-line)
          (setq start (point))
          (when (re-search-forward "^```\\s-*$" nil t)
            (setq end (match-beginning 0))
            (setq code (buffer-substring-no-properties start end)))))
      (if (not (and lang code))
          (message "No valid code block found.")
        (cond
         ((string= lang "r")
          (if (fboundp 'ess-eval-linewise)
              (ess-eval-linewise code)
            (message "ESS not available.")))
         ((string= lang "python")
          (if (fboundp 'python-shell-send-string)
              (python-shell-send-string code)
            (message "Python shell not available.")))
         (t (message "Unsupported language: %s" lang)))))))

;; --- Command: Build Quarto Document ---
(defcustom quarto-build-flags ""
  "Extra flags to pass to the Quarto build command."
  :group 'quarto
  :type 'string)

(defun quarto-build-document ()
  "Build the current Quarto document using Quarto.
Runs: `quarto render <current-file> <quarto-build-flags>` asynchronously."
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd (format "quarto render %s %s"
                      (shell-quote-argument file)
                      quarto-build-flags)))
    (message "Executing: %s" cmd)
    (compilation-start cmd 'compilation-mode)))

;; --- Command: Preview Quarto Document ---
(defcustom quarto-preview-flags ""
  "Extra flags to pass to the Quarto preview command."
  :group 'quarto
  :type 'string)

(defun quarto-preview-document ()
  "Preview the current Quarto document using Quarto.
Runs: `quarto preview <current-file> <quarto-preview-flags>` asynchronously."
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd (format "quarto preview %s %s"
                      (shell-quote-argument file)
                      quarto-preview-flags)))
    (message "Executing: %s" cmd)
    (compilation-start cmd 'compilation-mode)))

;; --- Bind the Commands in poly-quarto-mode ---
(define-key poly-quarto-mode-map (kbd "C-c C-c") 'quarto-send-code-block-to-repl)
(define-key poly-quarto-mode-map (kbd "C-c C-b") 'quarto-build-document)
(define-key poly-quarto-mode-map (kbd "C-c C-p") 'quarto-preview-document)

;; --- Improve Cursor Stability: Disable Electric Indent in Inner Modes ---
(add-hook 'ess-r-mode-hook
          (lambda () (setq-local electric-indent-inhibit t)))
(add-hook 'python-mode-hook
          (lambda () (setq-local electric-indent-inhibit t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(provide 'quarto-mode)
;;; quarto-mode.el ends here

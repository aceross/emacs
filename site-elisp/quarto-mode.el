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
;;   - An embedded live preview (C-c C-p); C-u C-c C-p restarts it.
;;; Code:

(require 'polymode)
(require 'poly-markdown)
(require 'poly-R)
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
  "Send this polymode chunk to its language REPL.
Opening/closing fences select their own body; prose signals an error.
Both languages use their own project session."
  (interactive)
  (save-restriction
    (widen)
    (let* ((span (pm-innermost-span))
           (body (pcase (car span)
                   ('head (pm-innermost-span (nth 2 span)))
                   ('tail (pm-innermost-span (1- (nth 1 span))))
                   ('body span))))
      (unless (and body (eq (car body) 'body))
        (user-error "Place point in an R/Python chunk or on its fence"))
      (let ((start (nth 1 body)) (end (nth 2 body))
            (buffer (pm-span-buffer body)))
        (with-current-buffer buffer
          (pcase major-mode
            ('python-mode
             (require 'awc-python)
             (awc-python-run)
             (python-shell-send-region start end))
            ('ess-r-mode
             (require 'awc-r)
             (awc-r-eval-region start end))
            (_ (user-error "Unsupported chunk language: %s" major-mode))))))))

;; --- Command: Build Quarto Document ---
(require 'quarto-render)

;; --- Command: Preview Quarto Document ---
(require 'quarto-preview)

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

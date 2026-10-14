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
(require 'quarto-document)
(require 'quarto-authoring)
(require 'quarto-lsp)
(require 'quarto-diagnostics)
(require 'quarto-runtime)
(declare-function tree-sitter-hl-mode "tree-sitter-hl" (&optional arg))
(declare-function highlight-indent-guides-mode "highlight-indent-guides" (&optional arg))
(declare-function lsp-disconnect "lsp-mode" ())

;; --- Disable Tree-sitter and Highlight-Indent-Guides in the Outer Mode ---
(defun my/poly-markdown-disable-extras ()
  "Disable conflicting highlighting and ordinary LSP in Quarto buffers."
  (when (bound-and-true-p tree-sitter-hl-mode)
    (tree-sitter-hl-mode -1))
  (when (bound-and-true-p highlight-indent-guides-mode)
    (highlight-indent-guides-mode -1))
  ;; Optionally disconnect LSP in the outer buffer if active.
  (when (bound-and-true-p lsp-mode)
    (lsp-disconnect)))
(add-hook 'poly-quarto-mode-hook #'my/poly-markdown-disable-extras)

(defun quarto-r-head (ahead) (quarto-document-match "r" 'head ahead))
(defun quarto-r-tail (ahead) (quarto-document-match "r" 'tail ahead))
(defun quarto-python-head (ahead) (quarto-document-match "python" 'head ahead))
(defun quarto-python-tail (ahead) (quarto-document-match "python" 'tail ahead))

;; --- Define R Innermode ---
(define-innermode poly-quarto-r-innermode
  :mode 'ess-r-mode
  :head-matcher #'quarto-r-head
  :tail-matcher #'quarto-r-tail
  :head-mode 'host
  :tail-mode 'host)

;; --- Define Python Innermode ---
(define-innermode poly-quarto-python-innermode
  :mode 'python-mode
  :head-matcher #'quarto-python-head
  :tail-matcher #'quarto-python-tail
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
  "Execute this complete chunk in its document-owned engine session."
  (interactive)
  (quarto-run-chunk))

;; --- Command: Build Quarto Document ---
(require 'quarto-render)

;; --- Command: Preview Quarto Document ---
(require 'quarto-preview)

;; --- Bind the Commands in poly-quarto-mode ---
(define-key poly-quarto-mode-map (kbd "C-c C-c") 'quarto-send-code-block-to-repl)
(define-key poly-quarto-mode-map (kbd "C-c C-b") 'quarto-build-document)
(define-key poly-quarto-mode-map (kbd "C-c C-p") 'quarto-preview-document)

;; --- Improve Cursor Stability: Disable Electric Indent in Inner Modes ---
(defun quarto-inner-setup ()
  "Restrict Quarto's indentation changes to its indirect buffers."
  (when (and (buffer-base-buffer) (quarto-source-p))
    (setq-local electric-indent-inhibit t)))
(add-hook 'ess-r-mode-hook #'quarto-inner-setup)
(add-hook 'python-mode-hook #'quarto-inner-setup)
(defun quarto-after-locals ()
  "Enable assistance after environment and bibliography settings are known."
  (when (and buffer-file-name (not (buffer-base-buffer)))
    (when quarto-lsp-auto-start (quarto-lsp-start))
    (quarto-yaml-refresh (current-buffer))
    (quarto-authoring-refresh-bibliographies)))

(defun quarto-editing-setup ()
  "Install Quarto features in the host and language buffers."
  (setq-local imenu-create-index-function #'quarto-imenu-index
              flycheck-checker 'quarto
              header-line-format '(:eval (quarto-source-header))
              flyspell-generic-check-word-predicate #'quarto-prose-spelling-p
              completion-at-point-functions '(quarto-yaml-completion-at-point
                                              quarto-reference-completion-at-point
                                              quarto-lsp-completion-at-point cape-file)
              eldoc-documentation-functions '(quarto-yaml-eldoc quarto-lsp-eldoc))
  (add-hook 'xref-backend-functions (lambda () 'quarto) nil t)
  (add-hook 'after-change-functions #'quarto-document-after-change nil t)
  (unless (buffer-base-buffer)
    (add-hook 'kill-buffer-hook #'quarto-lsp-close nil t)
    (add-hook 'kill-buffer-hook #'quarto-runtime-close nil t)
    (add-hook 'find-file-hook #'quarto-after-locals nil t)
    (add-hook 'hack-local-variables-hook #'quarto-after-locals nil t)))
(add-hook 'poly-quarto-mode-hook #'quarto-editing-setup)

(defun quarto-source-header ()
  "Show the current chunk label and document runtime state."
  (let ((chunk (quarto-chunk-at)))
    (format " Quarto · %s%s · runtime %s"
            (if chunk (or (quarto-chunk-language chunk) "example") "prose")
            (if (and chunk (quarto-chunk-label chunk)) (format " · %s" (quarto-chunk-label chunk)) "")
            (buffer-local-value 'quarto-runtime--state (quarto-source-buffer)))))

(defun quarto-project-yaml-setup ()
  (when (and buffer-file-name (string-match-p "_quarto\\(?:-[^.]+\\)?\\.ya?ml\\'" buffer-file-name))
    (setq-local completion-at-point-functions '(quarto-yaml-completion-at-point)
                eldoc-documentation-functions '(quarto-yaml-eldoc)
                flycheck-checker 'quarto)
    (add-hook 'after-change-functions #'quarto-document-after-change nil t)
    (quarto-yaml-refresh (current-buffer))))
(add-hook 'yaml-mode-hook #'quarto-project-yaml-setup)
(add-hook 'yaml-ts-mode-hook #'quarto-project-yaml-setup)
(require 'quarto-menu)
(define-key poly-quarto-mode-map (kbd "C-c q") #'quarto-menu)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(provide 'quarto-mode)
;;; quarto-mode.el ends here

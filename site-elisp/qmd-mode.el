;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; A custom major mode for editing Quarto (.qmd) files.
;; Provides syntax highlighting, code chunk detection, and REPL integration.

;;; Code:

(require 'generic-x) ;; For generic mode support
(require 'ess)       ;; For R support
(require 'python)    ;; For Python support

;; Define a keymap for qmd-mode
(defvar qmd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'qmd-send-chunk-to-repl)
    map)
  "Keymap for `qmd-mode`.")

;;;###autoload
(define-derived-mode qmd-mode text-mode "Quarto"
  "Major mode for editing Quarto (.qmd) files."
  ;; Syntax highlighting
  (setq font-lock-defaults
        '(;; YAML metadata
          ("^\\(title\\|author\\|date\\|output\\):" . font-lock-keyword-face)
          ;; Headings
          ("^#.*" . font-lock-function-name-face)
          ;; Code chunk delimiters
          ("^```{.*}" . font-lock-builtin-face)
          ("^```$" . font-lock-builtin-face)))
  ;; Enable qmd-mode keymap
  (use-local-map qmd-mode-map))

(defun qmd-get-chunk-language ()
  "Determine the language of the current code chunk."
  (save-excursion
    (when (re-search-backward "^```{\\([^}]+\\)}" nil t)
      (match-string 1)))) ;; Capture the language (e.g., "r" or "python")

(defun qmd-detect-code-chunk ()
  "Detect the start and end of the current code chunk."
  (save-excursion
    (when (re-search-backward "^```{.*}" nil t) ;; Find the start of the chunk
      (let ((start (point)))
        (when (re-search-forward "^```$" nil t) ;; Find the end of the chunk
          (list start (point)))))))

(defun qmd-strip-delimiters (chunk)
  "Strip the delimiters from a code chunk."
  (replace-regexp-in-string "^```{.*}\n\\|^```$" "" chunk))

(defun qmd-send-chunk-to-repl ()
  "Send the current code chunk to the appropriate REPL based on its language."
  (interactive)
  (let* ((chunk-bounds (qmd-detect-code-chunk))
         (chunk (and chunk-bounds
                     (buffer-substring-no-properties (car chunk-bounds) (cadr chunk-bounds))))
         (chunk-content (and chunk (qmd-strip-delimiters chunk)))
         (lang (qmd-get-chunk-language))
         (file-dir (file-name-directory (buffer-file-name)))) ;; Get file directory
    (if (and chunk-content lang)
        (cond
         ;; R chunk: Send to ESS
         ((string= lang "r")
          (unless (ess-process-live-p)
            (message "Starting R REPL...")
            (let ((default-directory file-dir)) ;; Set working directory
              (R))
            ;; Set working directory in R
            (ess-command (format "setwd('%s')\n" file-dir)))
          ;; Send the chunk content line by line
          (ess-eval-linewise chunk-content)
          (message "Sent R chunk to REPL."))
         ;; Python chunk: Send to Python REPL
         ((string= lang "python")
          (unless (python-shell-get-process)
            (message "Starting Python REPL...")
            (let ((default-directory file-dir)) ;; Set working directory
              (run-python)))
          ;; Send chunk content using stripped content
          (python-shell-send-string chunk-content)
          (message "Sent Python chunk to REPL."))
         ;; Unsupported language
         (t (message "Unsupported language: %s" lang)))
      (message "No valid code chunk detected."))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean and simple major mode for Quarto (.qmd) files.
;; Supports YAML metadata, headings, and code chunk delimiters.

;;; Code:

;; Define a keymap for qmd-mode
(defvar qmd-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybinding for sending the current chunk to REPL
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

(defun qmd-detect-code-chunk ()
  "Detect the start and end of the current code chunk."
  (save-excursion
    (when (re-search-backward "^```{.*}" nil t) ;; Find the start of the chunk
      (let ((start (point)))
        (when (re-search-forward "^```$" nil t) ;; Find the end of the chunk
          (let ((end (point)))
            (list start end)))))))

(defun qmd-extract-current-chunk ()
  "Extract the content of the current code chunk."
  (interactive)
  (let ((chunk (qmd-detect-code-chunk)))
    (if chunk
        (buffer-substring-no-properties (car chunk) (cadr chunk))
      (message "No code chunk detected."))))

(defun qmd-send-chunk-to-repl ()
  "Send the current code chunk to the appropriate REPL."
  (interactive)
  (let ((chunk (qmd-extract-current-chunk)))
    (if chunk
        (message "Sending chunk to REPL:\n%s" chunk) ;; Placeholder for REPL integration
      (message "No code chunk detected."))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

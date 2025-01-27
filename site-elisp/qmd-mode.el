;;; qmd-mode.el --- Major mode for editing Quarto (.qmd) files -*- lexical-binding: t; -*-

;;; Commentary:
;; This mode provides basic syntax highlighting, indentation, and setup for Quarto (.qmd) documents.
;; It supports R and Python code chunks, as well as Quarto-specific elements like YAML metadata.
;; Includes LSP integration and interactive REPL support.

;;; Code:

(require 'generic-x) ;; For generic mode support
(require 'lsp-mode) ;; For LSP integration
(require 'ess) ;; For R support (via ESS)
(require 'python) ;; For Python support
(require 'pyvenv) ;; For Python virtual environment support

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
  nil ;; Leave this empty and use `qmd-mode-hook` instead
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

(defun qmd-detect-language ()
  "Detect the language of the current code chunk in a Quarto file."
  (save-excursion
    (when (re-search-backward "^```{\(.*?\)}" nil t)
      (match-string 1))))

(defun qmd-lsp-activate ()
  "Activate LSP for the current code chunk based on its language."
  (let ((lang (qmd-detect-language)))
    (cond
     ((string= lang "r")
      (message "Activating LSP for R")
      (lsp-deferred))
     ((string= lang "python")
      (message "Activating LSP for Python")
      (lsp-deferred)))))

(defun qmd-setup-lsp ()
  "Setup LSP support for Quarto code chunks."
  (add-hook 'post-command-hook #'qmd-lsp-activate nil t))

(defun qmd-send-to-repl ()
  "Send the current code chunk to the appropriate REPL based on its language."
  (interactive)
  (let ((lang (qmd-detect-language)))
    (cond
     ((string= lang "r")
      (message "Sending R chunk to REPL")
      (ess-eval-region (save-excursion
                         (re-search-backward "^```{r}" nil t)
                         (match-end 0))
                       (save-excursion
                         (re-search-forward "^```" nil t)
                         (match-beginning 0)))))
     ((string= lang "python")
      (message "Sending Python chunk to REPL")
      (python-shell-send-region (save-excursion
                                  (re-search-backward "^```{python}" nil t)
                                  (match-end 0))
                                (save-excursion
                                  (re-search-forward "^```" nil t)
                                  (match-beginning 0)))))))

(defun qmd-setup-repl ()
  "Setup REPL support for Quarto code chunks."
  (local-set-key (kbd "C-c C-c") #'qmd-send-to-repl)
  (when (string= (qmd-detect-language) "python")
    (pyvenv-mode)))

(add-hook 'qmd-mode-hook #'qmd-setup)
(add-hook 'qmd-mode-hook #'qmd-setup-lsp)
(add-hook 'qmd-mode-hook #'qmd-setup-repl)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\'" . qmd-mode))

(provide 'qmd-mode)
;;; qmd-mode.el ends here

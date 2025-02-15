;;; quarto-mode.el --- Major mode for Quarto documents in Emacs -*- lexical-binding: t; -*-

;; Author: aceross
;; Version: 0.1
;; Keywords: quarto, markdown, reproducible research
;; Package-Requires: ((emacs "26.1") (polymode "0.2.2") (markdown-mode "2.3") (request "0.3.2"))

;;; Commentary:
;;
;; Improved Quarto Mode with dynamic language detection for `polymode`.
;; Supports multiple languages in Quarto (`R`, `Python`, `Julia`, `Bash`).
;; Provides functionality to send code blocks to the appropriate REPL.
;;
;;; Code:
(require 'polymode)
(require 'markdown-mode)
(require 'request)
(require 'comint)
(require 'ess-r-mode)
(require 'python)
;;(require 'julia-repl)
(require 'subr-x) ;; For string-trim

;; --- Language Detection ---
(defun quarto-detect-languages ()
  "Detect code blocks in the current Quarto document and return a list of required innermodes."
  (let (modes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^```{\\([^}]+\\)}" nil t)
        (let ((lang (match-string 1)))
          (cond
           ((string= lang "r") (push 'poly-r-markdown-inline-code-innermode modes))
           ((string= lang "python") (push 'poly-python-markdown-inline-code-innermode modes))
           ((string= lang "julia") (push 'poly-julia-markdown-inline-code-innermode modes))
           ((string= lang "bash") (push 'poly-bash-markdown-inline-code-innermode modes))
           (t (message "Unsupported language detected: %s" lang))))))
    (reverse (delete-dups modes))))

;; --- Define Quarto Polymode ---
(define-polymode poly-quarto-mode poly-markdown-mode
  "Polymode for Quarto files with dynamically detected inner modes."
  :lighter " Quarto"
  :innermodes (quarto-detect-languages))

;; --- Sending Code Blocks to REPL ---
(defun quarto-send-code-block-to-repl ()
  "Send the current Quarto code block to the appropriate REPL."
  (interactive)
  (let (lang start end code)
    (save-excursion
      (when (re-search-backward "^```{\\([^}]+\\)}" nil t)
        (setq lang (match-string 1))
        (forward-line)
        (setq start (point))
        (when (re-search-forward "^```" nil t)
          (setq end (match-beginning 0))
          (setq code (string-trim (buffer-substring-no-properties start end))))))
    (if (not (and lang code))
        (message "No valid code block found.")
      (cond
       ((string= lang "r")
        (ess-eval-linewise code))
       ((string= lang "python")
        (python-shell-send-string code))
       ;; ((string= lang "julia")
       ;;  (julia-repl-send-string code))
       ((string= lang "bash")
        (shell-command code))
       (t (message "Unsupported language: %s" lang))))))

(define-key poly-quarto-mode-map (kbd "C-c C-c") 'quarto-send-code-block-to-repl)

;; --- Auto-Mode Association ---
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(provide 'quarto-mode)
;;; quarto-mode.el ends here

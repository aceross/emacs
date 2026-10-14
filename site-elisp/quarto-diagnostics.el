;;; quarto-diagnostics.el --- One checker for Quarto source diagnostics -*- lexical-binding: t; -*-
(require 'quarto-document)
(require 'flycheck)
(defun quarto-diagnostics-start (checker callback)
  (let* ((source (quarto-source-buffer))
         (diagnostics (append (quarto-document-diagnostics (quarto-document-model))
                              (buffer-local-value 'quarto-yaml-diagnostics source)
                              (apply #'append (mapcar #'cdr (buffer-local-value 'quarto-lsp-diagnostics source)))))
         (target (current-buffer)))
    (funcall callback 'finished
             (with-current-buffer source
               (save-excursion (save-restriction
                 (widen)
                 (mapcar (lambda (item)
                           (goto-char (min (point-max) (car item)))
                           (flycheck-error-new-at (line-number-at-pos) (1+ (- (point) (line-beginning-position)))
                                                  (nth 1 item) (nth 2 item) :checker checker :buffer target
                                                  :filename buffer-file-name)) diagnostics)))))))
(flycheck-define-generic-checker 'quarto "Quarto structure, schema and mapped language diagnostics."
  :start #'quarto-diagnostics-start :modes '(markdown-mode ess-r-mode python-mode yaml-mode yaml-ts-mode)
  :predicate (lambda () (or (quarto-source-p) (quarto-yaml-position-p))))
(provide 'quarto-diagnostics)

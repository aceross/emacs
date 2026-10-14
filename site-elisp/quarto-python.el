;;; quarto-python.el --- Project Python for Quarto previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Choose an interpreter per document without activating a global pyvenv.
;; Pass it to both Quarto's Jupyter engine and knitr's reticulate bridge.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)

(require 'awc-project)
(require 'quarto-document)

(defalias 'quarto-preview--project-root #'awc-project-root)

(defun quarto-preview--python-chunks-p ()
  "Whether the current source contains executable Python chunks."
  (cl-some (lambda (chunk) (and (equal (quarto-chunk-language chunk) "python")
                              (quarto-chunk-executable chunk)))
           (quarto-document-chunks (quarto-document-model))))

(defun quarto-preview--python (file)
  "Select project Python for FILE when the source has Python chunks."
  (when (or quarto-preview-python (quarto-preview--python-chunks-p))
    (awc-project-python (file-name-directory (expand-file-name file)))))

(defun quarto-preview--python-environment (python)
  "Return shared subprocess overrides for PYTHON, or nil for R-only files."
  (when python (awc-project-environment python)))

(provide 'quarto-python)
;;; quarto-python.el ends here

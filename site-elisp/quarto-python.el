;;; quarto-python.el --- Project Python for Quarto previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Choose an interpreter per document without activating a global pyvenv.
;; Pass it to both Quarto's Jupyter engine and knitr's reticulate bridge.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)

(require 'awc-project)

(defalias 'quarto-preview--project-root #'awc-project-root)

(defun quarto-preview--python-chunks-p ()
  "Whether the current source contains executable Python chunks."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (re-search-forward "^[ \t]*\\(?:```+\\|~~~+\\){python\\(?:[ ,}]\\)" nil t)))))

(defun quarto-preview--python (file)
  "Select project Python for FILE when the source has Python chunks."
  (when (or quarto-preview-python (quarto-preview--python-chunks-p))
    (awc-project-python (file-name-directory (expand-file-name file)))))

(defun quarto-preview--python-environment (python)
  "Return shared subprocess overrides for PYTHON, or nil for R-only files."
  (when python (awc-project-environment python)))

(provide 'quarto-python)
;;; quarto-python.el ends here

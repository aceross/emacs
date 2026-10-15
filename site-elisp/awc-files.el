;;; awc-files.el --- File-saving conveniences -*- lexical-binding: t; -*-

(defun awc-files-create-parent-directories ()
  "Create missing parent directories before writing the current file.
Return nil so Emacs continues saving the buffer normally."
  (when buffer-file-name
    (let ((directory (file-name-directory buffer-file-name)))
      (unless (file-directory-p directory)
        (make-directory directory t))))
  nil)

;; This hook runs after a filename has been chosen, including on the first
;; save of an unnamed buffer.  Merely visiting a new path creates nothing.
(add-hook 'write-file-functions #'awc-files-create-parent-directories)

(provide 'awc-files)
;;; awc-files.el ends here

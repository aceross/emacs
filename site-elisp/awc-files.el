;;; awc-files.el --- File safety and recoverable editing -*- lexical-binding: t; -*-

(defgroup awc-files nil "File safety and recovery." :group 'files)

(defcustom awc-files-recovery-directory
  (expand-file-name "var/recovery/" user-emacs-directory)
  "Private recovery storage, outside manuscript and analysis repositories."
  :type 'directory :group 'awc-files)

(defun awc-files-private-directory (directory)
  "Create DIRECTORY for recovery data and restrict access to its owner."
  (make-directory directory t)
  (set-file-modes directory #o700)
  directory)

(defun awc-files-initialize ()
  "Set file-local safety checks and centralized recovery defaults.
Use native Emacs confirmation, backup, auto-save and locking mechanisms.
Calling this again does not discard existing recovery files."
  (let* ((root (awc-files-private-directory awc-files-recovery-directory))
         (backups (awc-files-private-directory (expand-file-name "backups/" root)))
         (autosaves (awc-files-private-directory (expand-file-name "autosave/" root)))
         (sessions (awc-files-private-directory (expand-file-name "sessions/" root))))
    (setq enable-local-variables t
          enable-local-eval 'maybe
          make-backup-files t
          backup-by-copying t
          backup-directory-alist (list (cons "." backups))
          vc-make-backup-files t
          version-control t
          kept-new-versions 6
          kept-old-versions 2
          delete-old-versions t
          auto-save-default t
          ;; Recovery copies must never replace the visited file on a timer.
          auto-save-visited-file-name nil
          auto-save-file-name-transforms (list (list ".*" autosaves 'sha256))
          auto-save-list-file-prefix (expand-file-name ".saves-" sessions)
          ;; Leave locks beside their files so other Emacs instances agree.
          create-lockfiles t)))

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

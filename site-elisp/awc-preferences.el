;;; awc-preferences.el --- Private persistent Customize settings -*- lexical-binding: t; -*-

(require 'awc-files)
(require 'lisp-mode)
(require 'subr-x)
(defvar org-confirm-babel-evaluate)

(defun awc-preferences-file ()
  "Return this configuration's private Customize filename."
  (expand-file-name "var/preferences/custom.el" user-emacs-directory))

(defun awc-preferences--disable-saving (error)
  "Report ERROR and prevent native Custom from falling back to init.el."
  ;; An existing directory cannot be overwritten as a Custom file.  In
  ;; particular, do not set nil: native Custom would then edit user-init-file.
  (setq custom-file (file-name-as-directory user-emacs-directory))
  (display-warning 'awc-preferences
                   (format "Preferences unavailable: %s. Repair %s and restart; Customize saving is disabled."
                           (error-message-string error) (awc-preferences-file))))

(defun awc-preferences-initialize ()
  "Prepare a stable owner-only Customize file without loading its contents.
Preserve unreadable, read-only, redirected or otherwise invalid existing files."
  (condition-case err
      (let* ((file (awc-preferences-file))
             (directory (file-name-directory file)))
        (dolist (path (list (expand-file-name "var" user-emacs-directory)
                            (directory-file-name directory) file))
          (when (file-symlink-p path) (error "Refusing redirected preferences: %s" path))
          (when (and (file-exists-p path)
                     (not (equal (file-attribute-user-id (file-attributes path 'integer)) (user-uid))))
            (error "Preferences path is not owned by this user: %s" path)))
        (awc-files-private-directory directory)
        (unless (file-exists-p file)
          (write-region ";;; Private Customize settings -*- lexical-binding: t; -*-\n"
                        nil file nil 'silent nil 'excl))
        (unless (and (file-regular-p file) (file-readable-p file) (file-writable-p file))
          (error "Preferences must be a readable, writable regular file: %s" file))
        (set-file-modes file #o600)
        (setq custom-file file))
    (error (awc-preferences--disable-saving err) nil)))

(defun awc-preferences--check-syntax (file)
  "Read all of FILE before allowing native load to evaluate any form."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let ((read-circle nil))
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (read (current-buffer))))))

(defun awc-preferences-load ()
  "Load prepared preferences after repository defaults, then retain safeguards.
This user-owned Lisp file is trusted configuration, not a document sandbox.
On a failed load, restore prior local-value approvals and preserve the file."
  (let ((file (awc-preferences-file))
        (trust (delq nil
                     (mapcar (lambda (variable)
                               (when (boundp variable)
                                 (list variable (copy-tree (symbol-value variable))
                                       (copy-tree (symbol-plist variable)))))
                             '(safe-local-variable-values safe-local-eval-forms
                               safe-local-variable-directories ignored-local-variable-values))))
        (user-settings (copy-tree (get 'user 'theme-settings))))
    (unwind-protect
        (when (equal custom-file file)
          (condition-case err
              (progn
                (awc-preferences--check-syntax file)
                (load file nil 'nomessage 'nosuffix)
                ;; Custom may include a stale custom-file value.  Keep one owner.
                (setq custom-file file)
                t)
            ((error quit)
             (dolist (entry trust)
               (set (car entry) (nth 1 entry))
               (setplist (car entry) (nth 2 entry)))
             ;; Prevent a later theme recalculation/native save from reviving
             ;; approvals recorded before a subsequent form failed or quit.
             (put 'user 'theme-settings user-settings)
             (awc-preferences--disable-saving err)
             (when (eq (car err) 'quit) (signal 'quit nil))
             nil)))
      ;; Private preferences may contain obsolete permissive settings.  Their
      ;; saved forms remain inspectable, but these startup policies stay active.
      (awc-files-initialize)
      (setq org-confirm-babel-evaluate t)
      (with-eval-after-load 'org
        (setq org-confirm-babel-evaluate t)))))

(provide 'awc-preferences)
;;; awc-preferences.el ends here

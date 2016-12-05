;;; init-emacs.el --- Modifications to Emacs configuations ---------------------
;;
;;; Commentary:
;;

;;; Code:

;; Globally set a directory variable.
(defconst awc/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun awc/emacs-subdirectory (d) (expand-file-name d awc/emacs-directory))

;; Ensure that the backup and elisp directories are generated.
(let* ((subdirs '("elisp" "backups"))
       (fulldirs (mapcar (lambda (d) (awc/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;; Put the Emacs customisations from menu into their own file.
(setq custom-file (expand-file-name "custom.el" awc/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Increase garbase collection
(setq gc-cons-threshold 50000000)

;; Remove the warnings from the GnuTLS library when using HTTPS by increasing
;; the minimum prime bits size.
(setq gnutls-min-prime-bits 4096)

;; auto-save list
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

;; update packages automatically
(use-package auto-package-update
  :ensure t
  :bind ("C-x P" . auto-package-update-now)
  :config
  (setq auto-package-update-delete-old-versions t))

(provide 'init-emacs)

;;; init-emacs.el ends here

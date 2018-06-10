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

;; restart Emacs from within Emacs
(use-package restart-emacs
  :ensure t
  :commands
  (restart-emacs))

;; for when I can't remember *all* Emacs' keybindings!
(use-package which-key
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-compute-remaps t))

;; two functions from Steve Purcell,
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(provide 'init-emacs)

;;; init-emacs.el ends here

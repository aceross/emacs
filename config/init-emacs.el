;;; init-emacs.el --- Modifications to Emacs configuations ---------------------
;;
;;; Commentary:
;;

;;; Code:

(require 'cl)

(use-package auto-compile
  :ensure t)

;; Globally set a directory variable.
(defconst awc/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun awc/emacs-subdirectory (d)
  "D is the sub-directory."
  (expand-file-name d awc/emacs-directory))

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

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
  :ensure t
  :diminish which-key-mode
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-compute-remaps t))

(use-package shell-pop
  :ensure t
  :bind (("s-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package exec-path-from-shell
   :ensure t
   :config
   (exec-path-from-shell-initialize))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(provide 'init-emacs)

;;; init-emacs.el ends here

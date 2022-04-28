;;; init-emacs.el --- Summary
;;
;;; Commentary:
;;     This are some core customisations to Emacs.
;;
;;; Code:

;; Globally set a directory variable.
(defconst awc/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

;; Put the Emacs customisations from menu into their own file.
(setq custom-file (expand-file-name "custom.el" awc/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Increase garbase collection
(setq gc-cons-threshold 50000000)

;; Remove the warnings from the GnuTLS library when using HTTPS by increasing
;; the minimum prime bits size.
(setq gnutls-min-prime-bits 4096)

;; auto-save list
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package exec-path-from-shell
   :ensure t
   :config
   (exec-path-from-shell-initialize))


(use-package dired
  :ensure nil
  :config
  (setq ls-lisp-dirs-first t)
  (setq delete-by-moving-to-trash t)
  (setq dired-ls-F-marks-symlinks t)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))
  )

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package quick-preview
  :init
  (global-set-key (kbd "C-c q") 'quick-preview-at-point)
  (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point))

(provide 'init-emacs)
;;; init-emacs ends here

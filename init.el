;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar my--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun my-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'my--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist my--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'my-reset-file-handler-alist-h)
  (add-hook 'after-init-hook '(lambda ()
                                 ;; restore after startup
                                 (setq gc-cons-threshold 16777216
                                       gc-cons-percentage 0.1)))
  )

;; Ensure Emacs is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

;; Add my customisations files path
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))

;; load the customisation modules
(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))
  (require 'init-core)
  (require 'init-ui)
  (require 'init-org)
  (require 'init-python)
  (require 'init-ess)
  (require 'init-lisp)
 ;; (require 'init-julia)
  (require 'init-text)
  (require 'init-lsp)
  ;;  (require 'init-quarto)
  (require 'quarto-mode)
  (require 'init-gptel)
  )

;;; init.el ends here

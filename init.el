;;;; init.el

(package-initialize)

(setq user-full-name "Aaron Ceross")
(require 'cl)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package dash)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
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

;; add core customisations for Emacs.
(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))
(require 'ui)
(require 'navigation)
(require 'editing)

;; add language and mode specific customisations.
(require 'init-lisp)  ; customisations for Common Lisp
(require 'init-cc)    ; customisations for C/C++
(require 'init-tex)   ; customisations for Latex/AucTex
(require 'init-org)   ; customisations for org-mode


;;; init.el ends here.

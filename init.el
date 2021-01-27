;;; init.el --- Emacs init file
;;
;;; Commentary:
;;
;;; Code:

(package-initialize)

(setq user-full-name "Aaron Ceross")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))


(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package dash)

;; Core customisations for Emacs.
(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))
(require 'init-emacs)
(require 'ui)
(require 'navigation)
(require 'editing)

;; Language and mode specific customisations.
(require 'init-lisp)        ; customisations for Common Lisp
(require 'init-scheme)      ; customisations for Scheme/Guile/Chicken
(require 'init-cc)          ; customisations for C/C++
(require 'init-ess)         ; customisations for ESS package
(require 'init-tex)         ; customisations for Latex/AucTex
(require 'init-org)         ; customisations for org-mode
(require 'init-python)      ; customisations for Python
(require 'init-markdown)    ; customisations for Markdown
(require 'init-julia)
(require 'init-prolog)
(require 'init-jupyter)

;;; init.el ends here

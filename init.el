;;; init.el --- Emacs init file
;;
;;; Commentary:
;;
;;; Code:

(require 'package)

(setq user-full-name "Aaron Ceross")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; customisations for Emacs
(add-to-list 'load-path (expand-file-name "elisp" "~/.emacs.d"))

(require 'init-emacs)
(require 'init-ui)
(require 'init-navigation)
(require 'init-editing)
(require 'init-text)
(require 'init-ess)
(require 'init-org)
(require 'init-git)
(require 'init-lisp)
(require 'init-python)

;;; init.el ends here

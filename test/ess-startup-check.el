;;; ess-startup-check.el --- Check real ESS activation order -*- lexical-binding: t; -*-
;; Run separately, without preloading straight/build directories:
;; emacs-xwidgets -Q --batch -l ~/.emacs.d/test/ess-startup-check.el
(setq user-emacs-directory
      (file-name-directory
       (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
      straight-use-package-by-default t
      straight-check-for-modifications nil)
(unless (file-exists-p (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
  (error "This startup check requires the existing straight installation"))
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory) nil t)
;; Use actual package activation, but refuse downloads or rebuilds in a check.
(setq straight-safe-mode t)
(straight-use-package 'use-package)
(require 'use-package)
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))
(when (locate-library "ess-site")
  (error "ESS must not already be on load-path; run this check in a fresh -Q Emacs"))
(require 'init-ess)
(unless (and (featurep 'ess-site) (featurep 'awc-r) (featurep 'awc-r-httpgd)
             (featurep 'awc-r-lsp))
  (error "ESS and its project helpers did not finish loading"))
(with-temp-buffer
  (ess-r-mode)
  (unless (and (eq (key-binding (kbd "C-c C-z")) #'awc-r-run)
               (eq (key-binding (kbd "C-c v")) #'awc/r-httpgd-init-and-open))
    (error "Project R bindings were not installed")))
(princ "PASS: straight activates ESS before its project helpers; R bindings work\n")

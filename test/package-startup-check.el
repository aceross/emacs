;;; package-startup-check.el --- Full startup with restored sources -*- lexical-binding: t; -*-
;; Explicit optional external-tool check, after package-restore-check.el.
;; AWC_PACKAGE_CONFIG must identify that run's disposable config directory.
(require 'cl-lib)
(require 'server)
(defvar awc-package-startup-config (getenv "AWC_PACKAGE_CONFIG"))
(unless (and awc-package-startup-config
             (file-in-directory-p awc-package-startup-config temporary-file-directory)
             (file-directory-p awc-package-startup-config))
  (error "Set AWC_PACKAGE_CONFIG to a disposable restored config under /tmp"))
(setq org-roam-db-location (expand-file-name "notes.db" awc-package-startup-config)
      server-name "package-startup"
      server-socket-dir (expand-file-name "server/" awc-package-startup-config)
      straight-disable-native-compile t)
(with-eval-after-load 'org-roam
  (setq org-roam-directory (expand-file-name "notes/" awc-package-startup-config))
  (make-directory org-roam-directory t))
(load (expand-file-name "init.el" awc-package-startup-config) nil t)
(require 'ert)
(when (with-current-buffer "*Messages*" (string-match-p "Error (use-package)" (buffer-string)))
  (error "Restored configuration has use-package errors"))
(should (awc-packages-audit))
(should-not (featurep 'vterm))
(should (and make-backup-files auto-save-default create-lockfiles))
(should (eq enable-local-variables t))
(should (eq org-confirm-babel-evaluate t))
(princ "PASS: full configuration builds and starts from the restored source snapshot\n")

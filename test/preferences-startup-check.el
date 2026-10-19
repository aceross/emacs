;;; preferences-startup-check.el --- Native Custom across full startups -*- lexical-binding: t; -*-
;; The core runner provides one disposable root and starts a fresh Emacs for
;; each stage.  The ordinary startup checks also exercise the real menus.
(load (expand-file-name "safety-startup-check.el" (file-name-directory load-file-name)) nil t)
;; -Q --batch loads init manually, after Emacs's own initial hook run.  Exercise
;; the configuration's installed hooks too, as a normal restart would do.
(run-hooks 'after-init-hook)
(pcase (getenv "AWC_PREFERENCES_CHECK_STAGE")
  ("save"
   (customize-save-variable 'sentence-end-double-space t)
   (customize-save-variable 'gptel-default-mode 'text-mode)
   (customize-save-variable 'org-todo-keyword-faces '(("TODO" . "#123456")))
   (require 'nov)
   (customize-save-variable 'nov-text-width 77)
   (should (= (file-modes custom-file) #o600))
   (princ "PASS: native Customize saved ordinary, GPT buffer and EPUB preferences privately\n"))
  ("load"
   (should sentence-end-double-space)
   (should (eq gptel-default-mode 'text-mode))
   (should (equal org-todo-keyword-faces '(("TODO" . "#123456"))))
   (set-org-todo-faces (selected-frame))
   (should (equal org-todo-keyword-faces '(("TODO" . "#123456"))))
   (require 'nov)
   (should (= nov-text-width 77))
   (should (equal custom-file (awc-preferences-file)))
   (should (= (file-modes custom-file) #o600))
   (princ "PASS: saved preferences survive full module defaults and deferred package loading\n"))
  ("corrupt"
   (should-not sentence-end-double-space)
   (should (file-directory-p custom-file))
   (should-not (file-exists-p (expand-file-name "preferences-ran" awc-safety-test-root)))
   (princ "PASS: malformed preferences preserve safe full startup without partial evaluation\n"))
  (_ (error "Select save, load or corrupt through AWC_PREFERENCES_CHECK_STAGE")))

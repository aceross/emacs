;;; package-restore-check.el --- Offline native Straight restore -*- lexical-binding: t; -*-
;; Explicit external integration test; copies sources into /tmp, never updates
;; the installed tree.  No alternate object database or existing builds used.
(require 'cl-lib)
(defvar awc-package-test-repo
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "site-elisp" awc-package-test-repo))
(require 'awc-packages)
(require 'ert)

(defvar awc-package-test-root (make-temp-file "/tmp/awc-package-restore-" t))
(setq user-emacs-directory (expand-file-name "config with spaces/" awc-package-test-root))
(make-directory (expand-file-name "straight/versions/" user-emacs-directory) t)
(copy-file (expand-file-name "straight/versions/default.el" awc-package-test-repo)
           (expand-file-name "straight/versions/default.el" user-emacs-directory))
(dolist (name '("init.el" "early-init.el"))
  (copy-file (expand-file-name name awc-package-test-repo) (expand-file-name name user-emacs-directory)))
(copy-directory (expand-file-name "site-elisp" awc-package-test-repo)
                (expand-file-name "site-elisp" user-emacs-directory))
(setq straight-initial-recipe-repositories nil
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications nil
      straight-disable-native-compile t
      straight-allow-recipe-inheritance nil)
(setenv "GIT_CONFIG_NOSYSTEM" "1")
(setenv "GIT_CONFIG_GLOBAL" "/dev/null")
(setenv "GIT_ALLOW_PROTOCOL" (if (getenv "AWC_PACKAGE_BOOTSTRAP_NETWORK") "file:https" "file"))
(let ((awc-packages-bootstrap-url
       (if (getenv "AWC_PACKAGE_BOOTSTRAP_NETWORK") awc-packages-bootstrap-url
         (concat "file://" (expand-file-name "straight/repos/straight.el" awc-package-test-repo)))))
  (awc-packages-bootstrap))
(setenv "GIT_ALLOW_PROTOCOL" "file")
;; Keep Straight's own recipe and origin consistent for native thaw.  The
;; commit is already local; protocol restrictions still prohibit network use.
(awc-packages--git (expand-file-name "straight/repos/straight.el" user-emacs-directory)
                   "remote" "set-url" "origin" awc-packages-bootstrap-url)
;; Drive the installed manager's actual warning-and-fallback path.  The hook
;; must quarantine the wrong checkout, including when Straight clears COMMIT.
(let* ((versions (awc-packages-read-snapshot))
       (snapshot (expand-file-name "straight/versions/default.el" user-emacs-directory))
       (directory (expand-file-name "straight/repos/dash.el" user-emacs-directory)))
  (setcdr (assoc "dash.el" versions) (make-string 40 ?0))
  (with-temp-file snapshot (prin1 versions (current-buffer)) (insert "\n:epsilon\n"))
  (should-error
   (straight-vc-clone (list :package "dash" :type 'git :local-repo "dash.el"
                            :repo (concat "file://" (expand-file-name "straight/repos/dash.el" awc-package-test-repo))
                            :nonrecursive t)))
  (should-not (file-exists-p directory))
  (should (directory-files (file-name-directory directory) nil "^\\.straight-failed-"))
  (copy-file (expand-file-name "straight/versions/default.el" awc-package-test-repo) snapshot t))
(dolist (entry (awc-packages-read-snapshot))
  (unless (equal (car entry) "straight.el")
    (straight-vc-clone
     (list :package (car entry) :type 'git :local-repo (car entry)
           :repo (concat "file://" (expand-file-name (car entry)
                                                    (expand-file-name "straight/repos/" awc-package-test-repo)))
           :nonrecursive t))))
(should (awc-packages-audit))
(princ (format "PASS: native Straight restored all %d pinned source repositories\n"
               (length (awc-packages-read-snapshot))))

;; Build and activate real packages in a fresh build tree, then exercise the
;; installed Straight thaw/rebuild APIs on a disposable changed dash checkout.
(dolist (spec '((bind-key "bind-key") (use-package "use-package") (dash "dash.el")))
  (straight-use-package
   (list (car spec) :type 'git :local-repo (cadr spec)
         :repo (concat "file://" (expand-file-name (cadr spec)
                                                  (expand-file-name "straight/repos/" awc-package-test-repo))))))
(require 'use-package)
(require 'dash)
(should (equal (-map #'1+ '(1 2 3)) '(2 3 4)))
(let* ((directory (expand-file-name "straight/repos/dash.el/" user-emacs-directory))
       (pin (cdr (assoc "dash.el" (awc-packages-read-snapshot))))
       (file (expand-file-name "awc-fixture-update.txt" directory)))
  (with-temp-file file (insert "Disposable update; never written to installed packages.\n"))
  (awc-packages--git directory "add" "awc-fixture-update.txt")
  (awc-packages--git directory "-c" "user.name=Fixture" "-c" "user.email=fixture@example.invalid"
                     "-c" "commit.gpgsign=false" "commit" "--quiet" "-m" "Disposable package update")
  (should-error (awc-packages-audit))
  ;; Snapshot a changed but clean package set, without fetching or pushing.
  (awc-packages-write-snapshot (expand-file-name "candidate.el" awc-package-test-root))
  (should-not (equal pin (awc-packages--head directory)))
  (straight-thaw-versions)
  (should (equal pin (awc-packages--head directory)))
  (should-not (file-exists-p file))
  (straight-rebuild-package "dash")
  (load (expand-file-name "straight/build/dash/dash.el" user-emacs-directory) nil t)
  (should (equal (-map #'1+ '(1 2 3)) '(2 3 4))))
(should (awc-packages-audit))
(princ (format "PASS: clean builds, drift detection, candidate snapshot, native thaw/rebuild\nFixtures: %s\n"
               awc-package-test-root))

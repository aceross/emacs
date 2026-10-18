;;; awc-packages-test.el --- Real Git bootstrap/recovery fixtures -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-packages)

(defvar straight-safe-mode nil)
(defvar awc-packages-test-loaded nil)

(defun awc-packages-test--commit (directory contents)
  (with-temp-file (expand-file-name "bootstrap.el" directory)
    (insert (format ";;; bootstrap.el --- Fixture -*- lexical-binding: t; -*-\n(setq awc-packages-test-loaded %S)\n" contents)))
  (awc-packages--git directory "add" "bootstrap.el")
  (awc-packages--git directory "-c" "user.name=Fixture" "-c" "user.email=fixture@example.invalid"
                     "-c" "commit.gpgsign=false" "commit" "--quiet" "-m" contents)
  (awc-packages--head directory))

(defun awc-packages-test--snapshot (directory versions)
  (let ((file (expand-file-name "straight/versions/default.el" directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (prin1 versions (current-buffer)) (insert "\n:epsilon\n"))
    file))

(defmacro awc-packages-test--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (make-temp-file "awc-packages-test-" t))
          (user-emacs-directory (expand-file-name "config é ; $(touch INJECTED)/" root))
          (remote (expand-file-name "upstream source" root))
          (awc-packages-bootstrap-url (concat "file://" remote))
          (straight-safe-mode nil)
          (awc-packages-test-loaded nil)
          (straight-vc-git-post-clone-hook nil)
          (process-environment (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null"
                                         "GIT_TERMINAL_PROMPT=0") process-environment)))
     (unwind-protect
         (progn
           (make-directory user-emacs-directory t)
           (make-directory remote)
           (awc-packages--git remote "init" "--quiet")
           (let* ((first (awc-packages-test--commit remote "pinned"))
                  (second (awc-packages-test--commit remote "newer upstream"))
                  (repo (expand-file-name "straight/repos/straight.el" user-emacs-directory))
                  (snapshot (awc-packages-test--snapshot
                             user-emacs-directory (list (cons "straight.el" first)))))
             ,@body))
       (delete-directory root t))))

(ert-deftest awc-packages-pinned-bootstrap-and-repeated-start ()
  (awc-packages-test--fixture
    (awc-packages-bootstrap)
    (should (equal awc-packages-test-loaded "pinned"))
    (should (equal (awc-packages--head repo) first))
    (should (awc-packages-audit))
    ;; Existing startup is offline and never rewrites an installed source.
    (let ((awc-packages-bootstrap-url "file:///missing-upstream"))
      (awc-packages-bootstrap))
    (should-not (file-exists-p (expand-file-name "INJECTED" root)))
    (should (equal first (awc-packages--head repo)))))

(ert-deftest awc-packages-bootstrap-failure-cleans-staging-and-retries ()
  (awc-packages-test--fixture
    (let ((awc-packages-bootstrap-url "file:///missing-upstream"))
      (should-error (awc-packages-bootstrap)))
    (should-not awc-packages-test-loaded)
    (should-not (file-exists-p repo))
    (should-not (directory-files (file-name-directory repo) nil "^\\.straight-bootstrap-"))
    (awc-packages-test--snapshot user-emacs-directory
                                 (list (cons "straight.el" (make-string 40 ?0))))
    (should-error (awc-packages-bootstrap))
    (should-not (file-exists-p repo))
    (should-not (directory-files (file-name-directory repo) nil "^\\.straight-bootstrap-"))
    ;; A valid commit with no bootstrap file must also remain inactive.
    (awc-packages--git remote "rm" "bootstrap.el")
    (awc-packages--git remote "-c" "user.name=Fixture" "-c" "user.email=fixture@example.invalid"
                       "-c" "commit.gpgsign=false" "commit" "--quiet" "-m" "Incomplete distribution")
    (awc-packages-test--snapshot user-emacs-directory
                                 (list (cons "straight.el" (awc-packages--head remote))))
    (should-error (awc-packages-bootstrap))
    (should-not (file-exists-p repo))
    (should-not awc-packages-test-loaded)
    (awc-packages-test--snapshot user-emacs-directory (list (cons "straight.el" first)))
    (awc-packages-bootstrap)
    (should (equal awc-packages-test-loaded "pinned"))))

(ert-deftest awc-packages-bootstrap-missing-git-or-safe-mode ()
  (awc-packages-test--fixture
    (let ((exec-path nil)) (should-error (awc-packages-bootstrap)))
    (let ((straight-safe-mode t)) (should-error (awc-packages-bootstrap)))
    (should-not (file-exists-p repo))
    (should-not awc-packages-test-loaded)))

(ert-deftest awc-packages-bootstrap-preserves-partial-checkout ()
  (awc-packages-test--fixture
    (make-directory repo t)
    (with-temp-file (expand-file-name "keep.txt" repo) (insert "local work"))
    (should-error (awc-packages-bootstrap))
    (should (equal "local work" (with-temp-buffer
                                 (insert-file-contents (expand-file-name "keep.txt" repo))
                                 (buffer-string))))
    (should-not awc-packages-test-loaded)))

(ert-deftest awc-packages-existing-drift-and-dirty-sources-refused ()
  (awc-packages-test--fixture
    (awc-packages-bootstrap)
    (setq awc-packages-test-loaded nil)
    (with-temp-file (expand-file-name "untracked.el" repo) (insert "local patch"))
    (should-error (awc-packages-bootstrap))
    (should-error (awc-packages-write-snapshot (expand-file-name "candidate.el" root)))
    (should-not (file-exists-p (expand-file-name "candidate.el" root)))
    (delete-file (expand-file-name "untracked.el" repo))
    (awc-packages--git repo "fetch" "origin" second)
    (awc-packages--git repo "checkout" "--detach" "--quiet" second)
    (should-error (awc-packages-bootstrap))
    (should-error (awc-packages-audit))
    (should-not awc-packages-test-loaded)
    ;; Roll back a real changed checkout, then start at the original pin.
    (awc-packages--git repo "checkout" "--detach" "--quiet" first)
    (awc-packages-bootstrap)
    (should (awc-packages-audit))))

(ert-deftest awc-packages-snapshot-validation-and-exclusive-write ()
  (awc-packages-test--fixture
    (awc-packages-bootstrap)
    (let ((candidate (expand-file-name "candidate.el" root)))
      (awc-packages-write-snapshot candidate)
      (should (equal (awc-packages-read-snapshot snapshot)
                     (awc-packages-read-snapshot candidate)))
      (should-error (awc-packages-write-snapshot candidate) :type 'file-already-exists)
      (let ((link (expand-file-name "candidate-link.el" root)))
        (make-symbolic-link candidate link)
        (should-error (awc-packages-write-snapshot link) :type 'file-already-exists)
        (should (equal (awc-packages-read-snapshot snapshot)
                       (awc-packages-read-snapshot candidate)))))
    (dolist (data (list "nil\n:epsilon\n" "#1=(#1#)\n:epsilon\n"
                       (format "((\"../escape\" . %S))\n:epsilon\n" first)
                       (format "((\"straight.el\" . %S) (\"straight.el\" . %S))\n:epsilon\n" first first)
                       "((\"straight.el\" . \"short\"))\n:epsilon\n"
                       (format "((\"straight.el\" . %S))\n:epsilon\n(setq awc-packages-test-loaded t)" first)))
      (with-temp-file snapshot (insert data))
      (should-error (awc-packages-read-snapshot snapshot)))))

(ert-deftest awc-packages-audit-requires-complete-clean-snapshot ()
  (awc-packages-test--fixture
    (awc-packages-bootstrap)
    (awc-packages-test--snapshot user-emacs-directory
                                 (list (cons "straight.el" first) (cons "missing" first)))
    (should-error (awc-packages-audit))
    (awc-packages-test--snapshot user-emacs-directory (list (cons "straight.el" first)))
    (let ((extra (expand-file-name "straight/repos/extra" user-emacs-directory)))
      (awc-packages--git root "clone" "--quiet" "--" remote extra)
      (should-error (awc-packages-audit))
      (awc-packages--git extra "checkout" "--detach" "--quiet" first)
      (awc-packages-test--snapshot user-emacs-directory
                                   (list (cons "straight.el" first) (cons "extra" first)))
      (should (awc-packages-audit))
      (with-temp-file (expand-file-name "bootstrap.el" extra) (insert "modified source"))
      (should-error (awc-packages-audit)))))

(ert-deftest awc-packages-post-clone-rejects-fallback-revision ()
  (awc-packages-test--fixture
    ;; A real checkout at the wrong revision represents Straight's documented
    ;; warning-and-continue fallback, including its cleared COMMIT argument.
    (let ((clone (expand-file-name "straight/repos/package" user-emacs-directory)))
      (make-directory (file-name-directory clone) t)
      (awc-packages-test--snapshot user-emacs-directory
                                   (list (cons "straight.el" first) (cons "package" first)))
      (dotimes (_ 2)
        (awc-packages--git root "clone" "--quiet" "--" remote clone)
        (should-error (awc-packages-check-clone :repo-dir clone :commit nil))
        (should-not (file-exists-p clone)))
      (let ((saved (directory-files (file-name-directory clone) t "^\\.straight-failed-")))
        (should (= (length saved) 2))
        (dolist (directory saved)
          (should (equal second (awc-packages--head (expand-file-name "package" directory)))))))))

(provide 'awc-packages-test)

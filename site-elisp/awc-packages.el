;;; awc-packages.el --- Reviewed Straight sources and bootstrap -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst awc-packages-bootstrap-url "https://github.com/radian-software/straight.el.git")

(defun awc-packages--git (directory &rest arguments)
  "Run checked Git ARGUMENTS in DIRECTORY, without a shell."
  (unless (executable-find "git")
    (error "Package recovery requires Git; no package files were loaded"))
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil (list t t) nil arguments)))
        (unless (eq status 0)
          (error "Package Git %s failed (%s): %s"
                 (car arguments) status (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun awc-packages-read-snapshot (&optional file)
  "Read and validate the native Straight snapshot FILE as data."
  (let ((file (or file (expand-file-name "straight/versions/default.el" user-emacs-directory)))
        (read-circle nil) names)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((versions (read (current-buffer))))
        (unless (and versions (proper-list-p versions))
          (error "Package snapshot must be a nonempty alist"))
        (dolist (entry versions)
          (unless (and (consp entry) (stringp (car entry))
                       (string-match-p "\\`[[:alnum:]_.-]+\\'" (car entry))
                       (not (member (car entry) '("." "..")))
                       (stringp (cdr entry))
                       (string-match-p "\\`[0-9a-f]\\{40\\}\\'" (cdr entry))
                       (not (member (car entry) names)))
            (error "Invalid or duplicate package snapshot entry: %S" entry))
          (push (car entry) names))
        (unless (and (assoc "straight.el" versions)
                     (eq (read (current-buffer)) :epsilon)
                     (progn (skip-chars-forward " \t\r\n") (eobp)))
          (error "Package snapshot needs straight.el and the :epsilon format marker"))
        versions))))

(defun awc-packages--head (directory)
  "Return DIRECTORY's commit, refusing accidental parent repositories."
  (unless (equal (file-truename (awc-packages--git directory "rev-parse" "--show-toplevel"))
                 (directory-file-name (file-truename directory)))
    (error "Not an independent package repository: %s" directory))
  (awc-packages--git directory "rev-parse" "HEAD"))

(defun awc-packages--clean (directory)
  "Refuse a snapshot or bootstrap that would omit local source changes."
  (unless (string-empty-p (awc-packages--git directory "status" "--porcelain=v1" "--untracked-files=all"))
    (error "Package repository has local changes; preserve them before proceeding: %s" directory)))

(cl-defun awc-packages-check-clone (&key repo-dir commit &allow-other-keys)
  "Reject Straight's fallback to an unpinned revision after a failed checkout.
Use its documented post-clone hook, before package build/activation.
Preserve a failed checkout outside its active path so restarting retries."
  (when repo-dir
    ;; Straight may clear its COMMIT argument when falling back.  Read our
    ;; snapshot too, rather than accepting that nil as an unpinned request.
    (let* ((name (file-name-nondirectory (directory-file-name repo-dir)))
           (expected (or (cdr (assoc name (awc-packages-read-snapshot))) commit)))
      (when (and expected (not (equal expected (awc-packages--head repo-dir))))
        (let* ((parent (file-name-directory (directory-file-name repo-dir)))
               (quarantine (make-temp-file (expand-file-name ".straight-failed-" parent) t)))
          (rename-file repo-dir (expand-file-name name quarantine))
          (error "Package clone missed its snapshot revision; preserved for inspection: %s" quarantine))))))

(defun awc-packages-bootstrap ()
  "Load the snapshotted Straight revision; never alter an existing checkout.
Install a missing checkout through a temporary sibling and atomic rename.
On failure, preserve existing installations and remove only our staging copy."
  (let* ((versions (awc-packages-read-snapshot))
         (commit (cdr (assoc "straight.el" versions)))
         (parent (expand-file-name "straight/repos/" user-emacs-directory))
         (repo (expand-file-name "straight.el" parent))
         (bootstrap (expand-file-name "bootstrap.el" repo)))
    (unless (or (file-exists-p repo) (file-symlink-p repo))
      (when (bound-and-true-p straight-safe-mode)
        (error "Straight is missing; safe mode cannot bootstrap packages"))
      (make-directory parent t)
      (let ((staging (make-temp-file (expand-file-name ".straight-bootstrap-" parent) t)))
        (unwind-protect
            (progn
              (awc-packages--git staging "init" "--quiet")
              (awc-packages--git staging "remote" "add" "origin" awc-packages-bootstrap-url)
              (awc-packages--git staging "fetch" "--no-tags" "--depth" "1" "origin" commit)
              ;; Straight's thaw/update APIs expect its configured main branch
              ;; and tracking refs, even when the initial fetch names a commit.
              (awc-packages--git staging "update-ref" "refs/remotes/origin/main" commit)
              (awc-packages--git staging "symbolic-ref" "refs/remotes/origin/HEAD" "refs/remotes/origin/main")
              (awc-packages--git staging "checkout" "--quiet" "-b" "main" "origin/main")
              (unless (and (equal commit (awc-packages--head staging))
                           (file-regular-p (expand-file-name "bootstrap.el" staging)))
                (error "Pinned Straight checkout has no usable bootstrap.el"))
              (rename-file staging repo))
          (when (file-directory-p staging) (delete-directory staging t)))))
    (unless (file-regular-p bootstrap)
      (error "Incomplete Straight checkout; preserve/rename it and retry: %s" repo))
    (unless (equal commit (awc-packages--head repo))
      (error "Straight differs from the snapshot; review/restore the checkout before restarting"))
    (awc-packages--clean repo)
    (add-hook 'straight-vc-git-post-clone-hook #'awc-packages-check-clone)
    (load bootstrap nil 'nomessage)))

(defun awc-packages--installed-versions ()
  "Inspect all installed source repositories, including recipe providers.
Do not fetch, normalize, build, push, or load third-party code."
  (let ((parent (expand-file-name "straight/repos/" user-emacs-directory)) versions)
    (dolist (directory (directory-files parent t "^[^.]"))
      (unless (file-directory-p directory)
        (error "Unexpected file in package repositories: %s" directory))
      (let ((commit (awc-packages--head directory)))
        (awc-packages--clean directory)
        (push (cons (file-name-nondirectory directory) commit) versions)))
    (sort versions (lambda (a b) (string-lessp (car a) (car b))))))

(defun awc-packages-audit ()
  "Check all installed sources against the snapshot without modifying them.
Report missing, extra, dirty, and changed repositories as errors."
  (interactive)
  (let ((expected (awc-packages-read-snapshot))
        (actual (awc-packages--installed-versions)))
    (dolist (entry expected)
      (unless (equal (cdr entry) (cdr (assoc (car entry) actual)))
        (error "Package missing or differs from snapshot: %s" (car entry))))
    (dolist (entry actual)
      (unless (assoc (car entry) expected)
        (error "Installed package is absent from snapshot: %s" (car entry))))
    (message "Package snapshot matches %d clean source repositories" (length expected))
    t))

(defun awc-packages-write-snapshot (file)
  "Capture clean installed sources in native Straight FILE, without updating.
Refuse an existing destination.  Review a new candidate before replacing the
committed snapshot.  Unlike `straight-freeze-versions', never push packages."
  (interactive "FNew package snapshot candidate (must not exist): ")
  (let ((versions (awc-packages--installed-versions)))
    (unless (assoc "straight.el" versions) (error "Straight checkout is missing"))
    (with-temp-buffer
      (insert "(" (mapconcat (lambda (entry) (format "%S" entry)) versions "\n ") ")\n:epsilon\n")
      ;; Check our data before touching the destination, then create exclusively.
      (let ((candidate (make-temp-file "awc-package-snapshot-")))
        (unwind-protect
            (progn
              (write-region (point-min) (point-max) candidate nil 'silent)
              (awc-packages-read-snapshot candidate))
          (delete-file candidate)))
      (write-region (point-min) (point-max) file nil 'silent nil 'excl))))

(provide 'awc-packages)
;;; awc-packages.el ends here

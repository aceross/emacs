;;; safety-startup-check.el --- Configured safety, isolated state -*- lexical-binding: t; -*-
;; Use installed package builds read-only, with disposable config/state paths.
(require 'cl-lib)
(defvar awc-safety-test-repo
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar awc-safety-test-root
  (or (getenv "AWC_SAFETY_TEST_ROOT")
      (make-temp-file (expand-file-name "safety-startup-" (or (getenv "QUARTO_TEST_RUN_DIR") "/tmp/")) t)))
(unless (file-in-directory-p awc-safety-test-root "/tmp/")
  (error "Configured startup fixtures must be under /tmp"))
(defvar awc-safety-test-config (expand-file-name "config/" awc-safety-test-root))
(make-directory awc-safety-test-config t)
(dolist (name '("init.el" "early-init.el"))
  (copy-file (expand-file-name name awc-safety-test-repo) (expand-file-name name awc-safety-test-config) t))
(dolist (name '("site-elisp" "straight"))
  (unless (file-exists-p (expand-file-name name awc-safety-test-config))
    (make-symbolic-link (expand-file-name name awc-safety-test-repo) (expand-file-name name awc-safety-test-config))))
(setq straight-safe-mode t
      org-roam-db-location (expand-file-name "notes.db" awc-safety-test-root))
;; Give actual org-roam setup an empty fixture vault before its :config runs.
(with-eval-after-load 'org-roam
  (setq org-roam-directory (expand-file-name "notes/" awc-safety-test-root))
  (make-directory org-roam-directory t))
(require 'server)
(setq server-name "safety-startup"
      server-socket-dir (expand-file-name "server/" awc-safety-test-root))
(setq user-init-file (expand-file-name "init.el" awc-safety-test-config))
(load user-init-file nil t)
(require 'org)
(require 'ert)
(should (eq enable-local-variables t))
(should (eq enable-local-eval 'maybe))
(should (eq org-confirm-babel-evaluate t))
(should (and make-backup-files auto-save-default create-lockfiles vc-make-backup-files))
(should (file-in-directory-p auto-save-list-file-prefix awc-files-recovery-directory))
(should (file-in-directory-p awc-files-recovery-directory awc-safety-test-root))
(when (with-current-buffer "*Messages*" (string-match-p "Error (use-package)" (buffer-string)))
  (error "Full configuration has a use-package error"))
;; Back up a genuinely Git-tracked source, not only an unversioned fixture.
(let* ((default-directory (file-name-as-directory (expand-file-name "project" awc-safety-test-root)))
       (file (expand-file-name "tracked.txt" default-directory))
       ;; The production predicate deliberately excludes /tmp.
       (backup-enable-predicate (lambda (name) (file-in-directory-p name default-directory))))
  (make-directory default-directory t)
  (should (zerop (call-process "git" nil nil nil "init" "--quiet")))
  (with-temp-file file (insert "Tracked original\n"))
  (should (zerop (call-process "git" nil nil nil "add" "tracked.txt")))
  (with-current-buffer (let ((noninteractive nil)) (find-file-noselect file))
    (should (eq (vc-backend file) 'Git))
    (goto-char (point-max)) (insert "Saved revision\n")
    (save-buffer)
    (let ((backup (car (file-backup-file-names file))))
      (should (file-in-directory-p backup awc-files-recovery-directory))
      (should (equal "Tracked original\n"
                     (with-temp-buffer (insert-file-contents backup) (buffer-string))))))
  (princ "PASS: a Git-tracked file retains its original contents in a central backup\n"))
;; Assert the intentional mode differences and exercise real Transient key
;; dispatch without contacting an LLM, starting servers, or visiting notes.
(dolist (mode '(org-mode markdown-mode python-mode ess-r-mode poly-quarto-mode))
  (save-window-excursion
   (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (delay-mode-hooks (funcall mode))
    (insert "This research examines the evidence. The method uses a small sample.\n")
    (should (eq (key-binding (kbd "C-c w")) #'awc/workflow))
    (should-not (key-binding (kbd "C-c g")))
    (should (eq (key-binding (kbd "C-q")) #'quoted-insert))
    (should (eq (key-binding (kbd "C-c p"))
                (if (eq mode 'org-mode) #'org-pomodoro #'completion-at-point)))
    (should (eq (key-binding (kbd "M-p"))
                (if (memq mode '(markdown-mode poly-quarto-mode))
                    #'markdown-previous-link #'completion-at-point)))
    (execute-kbd-macro (kbd "C-c w"))
    (should (eq (key-binding (kbd "a")) #'gptel))
    (should (eq (key-binding (kbd "W")) #'awc/writing))
    (execute-kbd-macro (kbd "W"))
    (should (eq (key-binding (kbd "g")) #'writegood-grade-level))
    (should (eq (key-binding (kbd "e")) #'writegood-reading-ease))
    ;; This pinned Transient keeps inactive suffixes in its raw keymap and
    ;; rejects them through its predicate map.  Check state AND real dispatch.
    (should (eq (not (oref (transient-suffix-object 'org-skeleton-R-src) inactive))
                (eq mode 'org-mode)))
    (unless (eq mode 'org-mode)
      (let ((before (buffer-string)))
        (execute-kbd-macro (kbd "r"))
        (should (equal before (buffer-string)))))
    ;; Calculate a real grade score through the menu, then reopen it to
    ;; insert a real contextual template.  Neither operation executes code.
    (execute-kbd-macro (kbd "g"))
    (should (with-current-buffer "*Messages*"
              (string-match-p "Flesch-Kincaid grade level score" (buffer-string))))
    (when (eq mode 'org-mode)
      (should (eq (key-binding (kbd "C-c o s r")) #'org-skeleton-R-src))
      (execute-kbd-macro (kbd "C-c w W r"))
      (should (string-match-p "#\\+begin_src R" (buffer-string)))
      (execute-kbd-macro (kbd "C-q C-a"))
      (should (eq (char-before) 1)))
    (princ (format "%s: C-c g=%S; C-c p=%S; M-p=%S; C-q=%S\n"
                   mode (key-binding (kbd "C-c g")) (key-binding (kbd "C-c p"))
                   (key-binding (kbd "M-p")) (key-binding (kbd "C-q")))))))
(princ (format "PASS: actual full startup retains safety/recovery policy; state: %s\n" awc-safety-test-root))

;;; awc-preferences-test.el --- Native Custom restart and failure checks -*- lexical-binding: t; -*-
(require 'awc-files-test)
(require 'awc-preferences)

(defconst awc-preferences-test--source (file-name-directory (locate-library "awc-preferences")))

(defmacro awc-preferences-test--fixture (&rest body)
  (declare (indent 0))
  `(awc-files-test--fixture
     (let ((user-emacs-directory (file-name-as-directory root))
           (user-init-file (expand-file-name "init.el" root))
           (custom-file nil))
       (awc-files-test--write user-init-file ";;; Original init, never a Custom destination.\n")
       ,@body)))

(defun awc-preferences-test--child (root &rest forms)
  "Run real native Custom/file-local handling in a fresh Emacs with FORMS."
  (let ((file (make-temp-file (expand-file-name "child-" root) nil ".el")))
    (with-temp-file file
      (insert ";;; Restart fixture -*- lexical-binding: t; -*-\n")
      (prin1 `(progn
                (setq user-emacs-directory ,(file-name-as-directory root)
                      user-init-file ,(expand-file-name "init.el" root)
                      sentence-end-double-space nil)
                (require 'ert)
                (require 'awc-preferences)
                (awc-files-initialize)
                (awc-preferences-initialize)
                (awc-preferences-load)
                ,@forms) (current-buffer)))
    (with-temp-buffer
      (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                  nil (list t t) nil "-Q" "--batch"
                                  "-L" awc-preferences-test--source "-l" file)))
        (unless (eq status 0)
          (let ((retained (make-temp-file "/tmp/awc-preferences-failed-" t)))
            (copy-directory root retained nil t t)
            (ert-fail (format "Child Emacs failed (%S); fixture retained at %s:\n%s"
                              status retained (substring (buffer-string) 0 (min 3000 (buffer-size)))))))))))

(ert-deftest awc-preferences-private-stable-initialization ()
  (awc-preferences-test--fixture
    (should (equal (awc-preferences-initialize) (awc-preferences-file)))
    (should (= (file-modes (file-name-directory custom-file)) #o700))
    (should (= (file-modes custom-file) #o600))
    (awc-files-test--write custom-file ";;; Preserved settings -*- lexical-binding: t; -*-\n")
    (should (awc-preferences-initialize))
    (should (string-prefix-p ";;; Preserved" (awc-files-test--read custom-file)))
    (should (awc-preferences-load))
    (should (awc-preferences-load))))

(ert-deftest awc-preferences-malformed-file-evaluates-nothing ()
  (awc-preferences-test--fixture
    (awc-preferences-initialize)
    (let* ((marker (expand-file-name "executed" root))
           (file custom-file)
           (text (format ";;; -*- lexical-binding: t; -*-\n(write-region \"ran\" nil %S)\n(custom-set-variables\n" marker)))
      (awc-files-test--write file text)
      (should-not (awc-preferences-load))
      (should-not (file-exists-p marker))
      (should (equal text (awc-files-test--read file)))
      (should (file-directory-p custom-file))
      (should (eq enable-local-variables t))
      (should org-confirm-babel-evaluate))))

(ert-deftest awc-preferences-runtime-failure-restores-approvals ()
  (awc-preferences-test--fixture
    (awc-preferences-initialize)
    (setq safe-local-variable-values '((fill-column . 81)))
    (let ((properties (copy-tree (symbol-plist 'safe-local-variable-values)))
          (settings (copy-tree (get 'user 'theme-settings))))
      (awc-files-test--write custom-file
                             ";;; -*- lexical-binding: t; -*-\n(custom-set-variables '(safe-local-variable-values '((awc-project-python . \"unreviewed\"))))\n(error \"Fixture runtime failure\")\n")
      (should-not (awc-preferences-load))
      (should (equal safe-local-variable-values '((fill-column . 81))))
      (should (equal (symbol-plist 'safe-local-variable-values) properties))
      (should (equal (get 'user 'theme-settings) settings))
      (should (file-directory-p custom-file)))))

(ert-deftest awc-preferences-invalid-storage-preserved-and-saving-disabled ()
  (awc-preferences-test--fixture
    (let* ((file (awc-preferences-file))
           (external (expand-file-name "external.el" root))
           (original (awc-files-test--read user-init-file)))
      (awc-files-test--write external ";;; Keep this file intact.\n")
      (make-directory (file-name-directory file) t)
      (make-symbolic-link external file)
      (should-not (awc-preferences-initialize))
      (should (file-directory-p custom-file))
      (awc-preferences-test--child root
        '(should-error (customize-save-variable 'sentence-end-double-space t)))
      (should (equal original (awc-files-test--read user-init-file)))
      (should (equal ";;; Keep this file intact.\n" (awc-files-test--read external)))
      (delete-file file)
      (awc-files-test--write file ";;; Read-only settings.\n")
      (set-file-modes file #o400)
      (should-not (awc-preferences-initialize))
      (should (= (file-modes file) #o400))
      (should (equal ";;; Read-only settings.\n" (awc-files-test--read file)))
      (set-file-modes file #o200)
      (should-not (awc-preferences-initialize))
      (should (= (file-modes file) #o200))
      (set-file-modes file #o600)
      (should (awc-preferences-initialize)))))

(ert-deftest awc-preferences-cancelled-load-restores-approvals-and-safety ()
  (awc-preferences-test--fixture
    (awc-preferences-initialize)
    (setq safe-local-variable-values '((fill-column . 81)))
    (awc-files-test--write custom-file
                           ";;; -*- lexical-binding: t; -*-\n(setq safe-local-variable-values '((awc-project-python . \"unreviewed\")) enable-local-variables :all)\n(signal 'quit nil)\n")
    (let (cancelled)
      (condition-case nil (awc-preferences-load) (quit (setq cancelled t)))
      (should cancelled))
    (should (equal safe-local-variable-values '((fill-column . 81))))
    (should (eq enable-local-variables t))
    (should (file-directory-p custom-file))))

(ert-deftest awc-preferences-native-custom-survives-restart ()
  (awc-preferences-test--fixture
    (awc-preferences-test--child root
      '(customize-save-variable 'sentence-end-double-space t)
      '(require 'calendar)
      '(customize-save-variable 'calendar-week-start-day 1)
      '(should (= (file-modes custom-file) #o600)))
    (awc-preferences-test--child root
      '(should sentence-end-double-space)
      '(require 'calendar)
      '(should (= calendar-week-start-day 1))
      '(should (equal custom-file (awc-preferences-file)))
      '(should (= (file-modes custom-file) #o600)))
    (should (equal ";;; Original init, never a Custom destination.\n"
                   (awc-files-test--read user-init-file)))))

(ert-deftest awc-preferences-stale-safety-settings-do-not-weaken-startup ()
  (awc-preferences-test--fixture
    ;; Use native saving in a separate process, including a delayed Org load.
    (awc-preferences-test--child root
      '(require 'org)
      '(customize-save-variable 'enable-local-variables :all)
      '(customize-save-variable 'enable-local-eval t)
      '(customize-save-variable 'org-confirm-babel-evaluate nil)
      '(customize-save-variable 'make-backup-files nil)
      '(customize-save-variable 'auto-save-default nil)
      '(customize-save-variable 'create-lockfiles nil))
    (awc-preferences-test--child root
      '(should (eq enable-local-variables t))
      '(should (eq enable-local-eval 'maybe))
      '(should (and make-backup-files auto-save-default create-lockfiles))
      '(require 'org)
      '(should org-confirm-babel-evaluate))))

(ert-deftest awc-preferences-native-approval-persists-without-trusting-new-values ()
  (awc-preferences-test--fixture
    (let* ((project (expand-file-name "project" root))
           (locals (expand-file-name ".dir-locals.el" project))
           (python (expand-file-name "env with spaces/bin/python" project))
           (document (expand-file-name "document.txt" project))
           (eval-file (expand-file-name "untrusted.txt" root))
           (marker (expand-file-name "eval-ran" root)))
      (awc-files-test--write python "Synthetic executable; never invoked.\n")
      (set-file-modes python #o700)
      (awc-files-test--write (expand-file-name "pyproject.toml" project) "")
      (awc-files-test--write document "Project document\n")
      (awc-files-test--write locals
                             "((nil . ((awc-project-python . \"env with spaces/bin/python\") (inferior-ess-r-program . \"/reviewed/R\"))))")
      (awc-preferences-test--child root
        '(require 'awc-project)
        '(defvar inferior-ess-r-program "R")
        `(let ((prompts 0) (noninteractive nil) (find-file-hook nil) (auto-mode-alist nil))
           (cl-letf (((symbol-function 'read-char-choice)
                      (lambda (&rest _) (cl-incf prompts) ?!)))
             (with-current-buffer (find-file-noselect ,document)
               (should (equal (awc-project-python) ,python))
               (should (equal inferior-ess-r-program "/reviewed/R"))))
           (should (= prompts 1)))
        '(should (safe-local-variable-p 'awc-project-python "env with spaces/bin/python"))
        '(should (string-match-p "safe-local-variable-values" (with-temp-buffer
                                                                (insert-file-contents custom-file) (buffer-string)))))
      (awc-preferences-test--child root
        '(require 'awc-project)
        '(defvar inferior-ess-r-program "R")
        '(should (safe-local-variable-p 'awc-project-python "env with spaces/bin/python"))
        ;; Native ! deliberately excludes risky names such as *-program.
        ;; Python's exact approved value persists; R still needs session review.
        `(let ((prompts 0) (noninteractive nil) (find-file-hook nil) (auto-mode-alist nil))
           (cl-letf (((symbol-function 'read-char-choice)
                      (lambda (&rest _) (cl-incf prompts) ?y)))
             (with-current-buffer (find-file-noselect ,document)
               (should (equal (awc-project-python) ,python))
               (should (equal inferior-ess-r-program "/reviewed/R"))))
           (should (= prompts 1))))
      (awc-files-test--write locals
                             "((nil . ((awc-project-python . \"/unreviewed/python\") (inferior-ess-r-program . \"/unreviewed/R\"))))")
      (awc-files-test--write eval-file
                             (format "Text\n\nLocal Variables:\neval: (write-region \"ran\" nil %S)\nEnd:\n" marker))
      (awc-preferences-test--child root
        '(require 'awc-project)
        '(defvar inferior-ess-r-program "R")
        `(let ((prompts 0) (noninteractive nil) (find-file-hook nil) (auto-mode-alist nil))
           (cl-letf (((symbol-function 'read-char-choice)
                      (lambda (&rest _) (cl-incf prompts) ?n)))
             (with-current-buffer (find-file-noselect ,document)
               (should-not awc-project-python)
               (should (equal inferior-ess-r-program "R")))
             (find-file-noselect ,eval-file))
           (should (= prompts 2))
           (should-not (file-exists-p ,marker)))
        '(should-not (safe-local-variable-p 'awc-project-python "/unreviewed/python"))))))

(provide 'awc-preferences-test)

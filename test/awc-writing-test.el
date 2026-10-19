;;; awc-writing-test.el --- Prose adapter boundaries -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-writing)

(defmacro awc-writing-test--fixture (&rest body)
  "Run BODY with disposable source, config and fake executable."
  (declare (indent 0))
  `(let* ((root (make-temp-file "/tmp/awc-writing-test-" t))
          (default-directory (file-name-as-directory root))
          (file (expand-file-name "draft.md" root))
          (program (expand-file-name "vale-fixture" root))
          (awc-writing-program program)
          (awc-writing-timeout 2)
          (source (generate-new-buffer "writing-test-source"))
          (initial-buffers (buffer-list)))
     (unwind-protect
         (with-current-buffer source
           (setq buffer-file-name file)
           (insert "Élise 😀\tTODO\n")
           (with-temp-file (expand-file-name ".vale.ini" root) (insert "[*]\n"))
           ;; A single owned Python process; cancellation never orphans a shell's sleep.
           (with-temp-file program
             (insert "#!/usr/bin/python3\nimport json,sys,time\ntext=sys.stdin.read()\ntime.sleep(0.08)\n"
                     "path=next(x[7:] for x in sys.argv if x.startswith('--path='))\n"
                     "print(json.dumps({path:[dict(Line=1,Span=[9,12],Check='Test.Placeholder',Severity='error',Message='Resolve TODO',Match='TODO')]}))\n"))
           (set-file-modes program #o700)
           ,@body)
       (when (buffer-live-p source)
         (with-current-buffer source
           (set-buffer-modified-p nil)
           (awc-writing-cancel))
         (kill-buffer source))
       (dolist (buffer (buffer-list))
         (unless (memq buffer initial-buffers)
           (when (string-match-p "writing\\|Writing QA" (buffer-name buffer)) (kill-buffer buffer))))
       (delete-directory root t))))

(defun awc-writing-test--wait ()
  (let ((deadline (+ (float-time) 4)))
    (while (and awc-writing--process (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should-not awc-writing--process)))

(defun awc-writing-test--report ()
  (with-current-buffer awc-writing--results (buffer-string)))

(ert-deftest awc-writing-missing-tool-and-rules-preserve-editing ()
  (awc-writing-test--fixture
    (let ((awc-writing-program "/missing/vale"))
      (should-error (awc-writing-check) :type 'user-error))
    (delete-file (expand-file-name ".vale.ini" root))
    (should-error (awc-writing-check) :type 'user-error)
    (should-not awc-writing--results)
    (insert "Still editable")
    (save-buffer)
    (should (file-exists-p file))))

(ert-deftest awc-writing-nearest-config-and-path-order ()
  (awc-writing-test--fixture
    (let* ((subdir (expand-file-name "chapter/" root))
           (nested (expand-file-name "_vale.ini" subdir)))
      (make-directory subdir)
      (with-temp-file nested (insert "[*]\n"))
      (should (equal (awc-writing--config (expand-file-name "draft.md" subdir)) nested)))))

(ert-deftest awc-writing-unsaved-wide-indirect-and-character-ranges ()
  (awc-writing-test--fixture
    (let ((indirect (make-indirect-buffer source "writing-indirect")))
      (unwind-protect
          (with-current-buffer indirect
            (narrow-to-region 3 7)
            (awc-writing-check))
        (kill-buffer indirect)))
    (awc-writing-test--wait)
    (should-not (file-exists-p file))
    (should (string-match-p "1 findings" (awc-writing-test--report)))
    (save-window-excursion
      (switch-to-buffer awc-writing--results)
      (goto-char (point-min))
      (search-forward "draft.md:1:9-12")
      (beginning-of-line)
      (awc-writing-goto-finding)
      (should (eq (current-buffer) source))
      (should (= (point) 9))
      (should (equal (buffer-substring-no-properties (point) (+ (point) 4)) "TODO")))))

(ert-deftest awc-writing-stale-completion-is-discarded ()
  (awc-writing-test--fixture
    (awc-writing-check)
    (insert "Changed")
    (awc-writing-test--wait)
    (should (string-match-p "discarded" (awc-writing-test--report)))))

(ert-deftest awc-writing-report-actions-resolve-the-unsaved-source-and-nearest-rules ()
  (awc-writing-test--fixture
    (let* ((chapter (expand-file-name "chapter/" root))
           (config (expand-file-name "_vale.ini" chapter))
           config-buffer)
      (make-directory chapter)
      (with-temp-file config (insert "[*]\n"))
      (setq buffer-file-name (expand-file-name "draft.qmd" chapter))
      (awc-writing-check)
      (awc-writing-test--wait)
      (let ((report awc-writing--results))
        (unwind-protect
            (save-window-excursion
              (switch-to-buffer report)
              (awc-writing-show-results)
              (should (eq (current-buffer) report))
              (awc-writing-open-config)
              (setq config-buffer (current-buffer))
              (should (equal buffer-file-name config))
              (with-current-buffer source (goto-char (point-max)) (insert "Changed unsaved prose\n"))
              (switch-to-buffer report)
              (awc-writing-check)
              (with-current-buffer source
                (should (eq awc-writing--results report))
                (should (equal (process-get awc-writing--process 'config) config))
                (awc-writing-test--wait)
                (should (buffer-modified-p))
                (should-not (file-exists-p buffer-file-name))
                (should (string-match-p "1 findings" (awc-writing-test--report)))))
          (when (buffer-live-p config-buffer) (kill-buffer config-buffer)))))))

(ert-deftest awc-writing-report-actions-refuse-a-closed-source-without-helpers ()
  (awc-writing-test--fixture
    (awc-writing-check)
    (awc-writing-test--wait)
    (let ((report awc-writing--results))
      (set-buffer-modified-p nil)
      (kill-buffer source)
      (with-current-buffer report
        (dolist (command '(awc-writing-check awc-writing-recheck awc-writing-show-results awc-writing-open-config))
          (should (equal (should-error (funcall command) :type 'user-error) '(user-error "Writing source is closed"))))
        (awc-writing-cancel))
      (should-not (get-buffer " *writing-stdout*"))
      (should-not (get-buffer " *writing-stderr*")))))

(ert-deftest awc-writing-stale-navigation-and-rename-are-refused ()
  (awc-writing-test--fixture
    (awc-writing-check)
    (awc-writing-test--wait)
    (insert "Changed")
    (with-current-buffer awc-writing--results
      (should-error (awc-writing--next-error 1 t) :type 'user-error))
    (setq buffer-file-name (expand-file-name "renamed.md" root))
    (with-current-buffer awc-writing--results
      (should-error (awc-writing--next-error 1 t) :type 'user-error)
      (should-error (awc-writing-goto-finding) :type 'user-error))))

(ert-deftest awc-writing-two-sources-remain-independent ()
  (awc-writing-test--fixture
    (let ((sibling (generate-new-buffer "writing-sibling")))
      (unwind-protect
          (progn
            (awc-writing-check)
            (with-current-buffer sibling
              (setq buffer-file-name (expand-file-name "sibling.md" root))
              (insert "Élise 😀\tTODO\n")
              (awc-writing-check))
            (awc-writing-cancel)
            (with-current-buffer sibling
              (awc-writing-test--wait)
              (should (string-match-p "1 findings" (awc-writing-test--report))))
            (should (string-match-p "cancelled" (awc-writing-test--report))))
        (with-current-buffer sibling (set-buffer-modified-p nil))
        (kill-buffer sibling)))))

(ert-deftest awc-writing-closed-source-cancels-its-check ()
  (awc-writing-test--fixture
    (awc-writing-check)
    (let ((process awc-writing--process) (results awc-writing--results))
      (set-buffer-modified-p nil)
      (kill-buffer source)
      (should-not (process-live-p process))
      (should-not (buffer-live-p (process-buffer process)))
      (with-current-buffer results (should-error (awc-writing--guard) :type 'user-error)))))

(ert-deftest awc-writing-timeout-cleans-process-and-buffers ()
  (awc-writing-test--fixture
    (let ((awc-writing-timeout 0.01)) (awc-writing-check))
    (let ((process awc-writing--process))
      (awc-writing-test--wait)
      (should-not (process-live-p process))
      (should-not (buffer-live-p (process-buffer process)))
      (should-not (buffer-live-p (process-get process 'stderr)))
      (should-not (memq (process-get process 'timer) timer-list))
      (should (string-match-p "timed out" (awc-writing-test--report))))))

(ert-deftest awc-writing-results-close-and-recheck-cancel-owned-processes ()
  (awc-writing-test--fixture
    (awc-writing-check)
    (let ((first awc-writing--process))
      (awc-writing-check)
      (should-not (process-live-p first)))
    (let ((second awc-writing--process))
      (kill-buffer awc-writing--results)
      (should-not (process-live-p second))
      (should-not awc-writing--process))))

(ert-deftest awc-writing-broken-rules-and-malformed-json-are-failures ()
  (awc-writing-test--fixture
    (dolist (script '("import sys; print('missing style',file=sys.stderr); sys.exit(2)"
                      "print('not JSON')"
                      "print('{}'); print('rule warning',file=__import__('sys').stderr)"))
      (with-temp-file program (insert "#!/usr/bin/python3\nimport sys; sys.stdin.read()\n" script "\n"))
      (awc-writing-check)
      (awc-writing-test--wait)
      (should (string-match-p "Writing QA failed:" (awc-writing-test--report)))
      (should-not (string-match-p "0 findings" (awc-writing-test--report))))))

(ert-deftest awc-writing-malformed-or-out-of-bounds-ranges-are-refused ()
  (with-temp-buffer
    (insert "TODO\n")
    (dolist (output '("[]" "{\"other.md\":[]}"
                      "{\"draft.md\":[{\"Line\":1,\"Span\":[1,99],\"Check\":\"X\",\"Message\":\"X\",\"Severity\":\"error\"}]}"
                      "{\"draft.md\":[{\"Line\":9,\"Span\":[1,4],\"Check\":\"X\",\"Message\":\"X\",\"Severity\":\"error\"}]}"))
      (should-error (awc-writing--parse output "draft.md" (current-buffer))))))

(ert-deftest awc-writing-unsupported-and-remote-sources-are-refused ()
  (with-temp-buffer
    (dolist (file '(nil "/tmp/source.py" "/ssh:host:/draft.md"))
      (setq buffer-file-name file)
      (should-error (awc-writing-check) :type 'user-error))))

(ert-deftest awc-writing-launch-failure-does-not-leak-helpers ()
  (awc-writing-test--fixture
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "fixture launch failure"))))
      (should-error (awc-writing-check)))
    (should (string-match-p "failed to start" (awc-writing-test--report)))
    (should-not awc-writing--process)
    (should-not (get-buffer " *writing-stdout*"))
    (should-not (get-buffer " *writing-stderr*"))))

(provide 'awc-writing-test)
;;; awc-writing-test.el ends here

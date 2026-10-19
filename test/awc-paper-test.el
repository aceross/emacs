;;; awc-paper-test.el --- Manuscript UI boundaries -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-paper)

(defmacro awc-paper-test--project (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "/tmp/awc-paper-ert-" t)))
          (default-directory root))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(ert-deftest awc-paper-no-config-or-remote-project-is-refused ()
  (awc-paper-test--project
    (should-error (awc-paper--project) :type 'user-error)
    (let ((default-directory "/ssh:example:/paper/"))
      (should-error (awc-paper--project) :type 'user-error))))

(ert-deftest awc-paper-unsaved-project-buffers-block-mutating-actions ()
  (awc-paper-test--project
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "paper.qmd" root))
      (insert "An unsaved manuscript")
      (should (member buffer-file-name (awc-paper--dirty root)))
      (should-error (awc-paper--saved root) :type 'user-error)
      (set-buffer-modified-p nil)
      (should-not (awc-paper--dirty root)))))

(ert-deftest awc-paper-declined-execution-only-reads-status ()
  (awc-paper-test--project
    (let ((awc-paper--root root) actions)
      (cl-letf (((symbol-function 'awc-paper--start)
                 (lambda (_root action &optional _arguments callback)
                   (push action actions)
                   (when callback (funcall callback (make-hash-table :test 'equal)))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (awc-paper-check))
      (should (equal actions '("status"))))))

(ert-deftest awc-paper-execution-passes-the-reviewed-token-and-explicit-flag ()
  (awc-paper-test--project
    (let ((awc-paper--root root) arguments)
      (cl-letf (((symbol-function 'awc-paper--start)
                 (lambda (_root action &optional args callback)
                   (if (equal action "check") (setq arguments args)
                     (let ((data (make-hash-table :test 'equal)))
                       (puthash "review_token" "reviewed-snapshot" data)
                       (funcall callback data)))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (awc-paper-check))
      (should (equal (plist-get arguments :expect) "reviewed-snapshot"))
      (should (equal (plist-get arguments :args) '("--execute"))))))

(ert-deftest awc-paper-status-has-no-project-execution-flag ()
  (awc-paper-test--project
    (let ((awc-paper--root root) seen)
      (cl-letf (((symbol-function 'awc-paper--start)
                 (lambda (directory action &rest args) (setq seen (list directory action args)))))
        (awc-paper-status))
      (should (equal seen (list root "status" nil))))))

(ert-deftest awc-paper-renders-runner-commands-without-expanding-the-launcher ()
  (awc-paper-test--project
    (with-temp-buffer
      (setq awc-paper--root root)
      (awc-paper--render
       (json-parse-string
        "{\"state\":\"incomplete\",\"run\":{},\"snapshot\":{\"id\":\"fixture\",\"git\":{},\"files\":{}},\"plan\":{\"checks\":[{\"id\":\"words\",\"type\":\"word-count\",\"argv\":[\"launcher-only\"],\"commands\":[[\"runner-command\",\"--to=json\",\"paper with spaces é.qmd\"]]},{\"id\":\"deferred\",\"commands\":[],\"skip\":\"Explicitly deferred\"}]}}"
        :array-type 'list))
      (should (string-match-p (regexp-quote "(\"runner-command\" \"--to=json\" \"paper with spaces é.qmd\")") (buffer-string)))
      (should (string-match-p "Skipped: Explicitly deferred" (buffer-string)))
      (should-not (string-match-p "launcher-only" (buffer-string))))))

(ert-deftest awc-paper-inapplicable-review-retains-the-current-report ()
  (awc-paper-test--project
    (with-temp-file (expand-file-name "paper.qmd" root) (insert "Synthetic manuscript\n"))
    (with-temp-file (expand-file-name ".awc-paper.json" root)
      (insert "{\"version\":1,\"inputs\":[\"paper.qmd\"]}"))
    (let ((report (get-buffer-create (format "*Paper: %s*" root))) messages)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (awc-paper-verify)
              (let ((deadline (+ (float-time) 5)))
                (while (and (buffer-local-value 'awc-paper--process report) (< (float-time) deadline))
                  (accept-process-output nil 0.02))))
            (with-current-buffer report
              (should-not awc-paper--process)
              (should-not (gethash "error" awc-paper--data))
              (should (gethash "snapshot" awc-paper--data))
              (should (string-match-p "Status: INCOMPLETE" (buffer-string))))
            (should (member "No applicable manual requirement" messages))
            (should-not (file-exists-p (expand-file-name ".awc-paper" root)))
            (should-not (get-buffer " *paper-stdout*"))
            (should-not (get-buffer " *paper-stderr*")))
        (kill-buffer report)))))

(ert-deftest awc-paper-every-review-rechecks-unsaved-buffers-after-prompts ()
  (awc-paper-test--project
    (dolist (command '(awc-paper-check awc-paper-verify awc-paper-freeze))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "paper.qmd" root))
        (let ((source (current-buffer)) (awc-paper--root root) actions)
          (cl-letf (((symbol-function 'awc-paper--start)
                     (lambda (_root action &optional _args callback)
                       (push action actions)
                       (when callback
                         (let ((data (json-parse-string "{\"manual\":[{\"id\":\"review\"}]}" :array-type 'list)))
                           (funcall callback data)))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (with-current-buffer source (insert "Unsaved")) t))
                    ((symbol-function 'completing-read) (lambda (&rest _) "review"))
                    ((symbol-function 'awc-paper--render) #'ignore)
                    ((symbol-function 'read-string)
                     (lambda (&rest _) (with-current-buffer source (insert "Unsaved")) "Synthetic review")))
            (funcall command))
          (should (equal actions '("status"))))))))

(ert-deftest awc-paper-aborted-or-unsaved-review-keeps-status-and-creates-no-state ()
  (awc-paper-test--project
    (with-temp-file (expand-file-name "paper.qmd" root) (insert "Synthetic manuscript\n"))
    (with-temp-file (expand-file-name ".awc-paper.json" root)
      (insert "{\"version\":1,\"inputs\":[\"paper.qmd\"]}"))
    (let ((source (generate-new-buffer "paper-review-source"))
          (report (get-buffer-create (format "*Paper: %s*" root))))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer source (setq buffer-file-name (expand-file-name "paper.qmd" root)))
            (dolist (outcome '(quit unsaved))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (&rest _)
                           (if (eq outcome 'quit) (signal 'quit nil)
                             (with-current-buffer source (insert "Unsaved")) t))))
                (with-current-buffer source (awc-paper-check))
                (let ((deadline (+ (float-time) 5)))
                  (while (and (buffer-local-value 'awc-paper--process report) (< (float-time) deadline))
                    (accept-process-output nil 0.02))))
              (with-current-buffer report
                (should-not awc-paper--process)
                (should-not (gethash "error" awc-paper--data))
                (should (string-match-p (if (eq outcome 'quit) "Status: INCOMPLETE" "STALE.*UNSAVED") (buffer-string))))
              (should-not (file-exists-p (expand-file-name ".awc-paper" root)))
              (should-not (get-buffer " *paper-stdout*"))
              (should-not (get-buffer " *paper-stderr*"))))
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source)
        (kill-buffer report)))))

(ert-deftest awc-paper-manual-and-freeze-forward-the-exact-reviewed-token ()
  (awc-paper-test--project
    (dolist (entry '((awc-paper-verify "verify" ("review" "--reviewer" "Fixture" "--note" "Fixture"))
                     (awc-paper-freeze "freeze" ("--label" "Fixture"))))
      (let ((awc-paper--root root) actions)
        (cl-letf (((symbol-function 'awc-paper--start)
                   (lambda (_root action &optional args callback)
                     (push (cons action args) actions)
                     (when callback
                       (funcall callback (json-parse-string "{\"review_token\":\"exact-token\",\"manual\":[{\"id\":\"review\"}]}" :array-type 'list)))))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'completing-read) (lambda (&rest _) "review"))
                  ((symbol-function 'read-string) (lambda (&rest _) "Fixture")))
          (funcall (car entry)))
        (should (equal (nreverse actions)
                       (list '("status") (list (cadr entry) :expect "exact-token" :args (caddr entry)))))))))

(ert-deftest awc-paper-missing-python-allocates-no-report-or-helper ()
  (awc-paper-test--project
    (let ((awc-paper-python "/missing/awc-python"))
      (should-error (awc-paper--start root "status") :type 'user-error))
    (should-not (get-buffer (format "*Paper: %s*" root)))
    (should-not (get-buffer " *paper-stdout*"))
    (should-not (get-buffer " *paper-stderr*"))))

(ert-deftest awc-paper-backend-failure-is-visible-and-cleans-helper-buffers ()
  (awc-paper-test--project
    (with-temp-file (expand-file-name ".awc-paper.json" root) (insert "malformed JSON"))
    (let ((report (get-buffer-create (format "*Paper: %s*" root))))
      (unwind-protect
          (progn
            (awc-paper--start root "status")
            (let ((deadline (+ (float-time) 5)))
              (while (and (buffer-local-value 'awc-paper--process report) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (with-current-buffer report
              (should-not awc-paper--process)
              (should (gethash "error" awc-paper--data))
              (should (string-match-p "Unavailable:" (buffer-string))))
            (should-not (get-buffer " *paper-stdout*"))
            (should-not (get-buffer " *paper-stderr*")))
        (kill-buffer report)))))

(ert-deftest awc-paper-first-line-diagnostic-navigates-without-changing-the-log ()
  (awc-paper-test--project
    (let* ((file (expand-file-name "paper.txt" root))
           (log (expand-file-name "check.log" root))
           (original "paper.txt:2:1: error: Synthetic fixture finding\n")
           (report (generate-new-buffer "paper-test-report"))
           view source)
      (with-temp-file file (insert "First line\nSecond line\n"))
      (with-temp-file log (insert original))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer report)
            (awc-paper--link "Log" "check.log" root t)
            (goto-char (point-min))
            (push-button)
            (setq view (current-buffer))
            (should (derived-mode-p 'compilation-mode))
            (setq next-error-last-buffer view)
            (execute-kbd-macro (kbd "M-g n"))
            (setq source (current-buffer))
            (should (equal buffer-file-name file))
            (should (= (line-number-at-pos) 2))
            (should (equal original (with-temp-buffer (insert-file-contents log) (buffer-string)))))
        (dolist (buffer (list report view source)) (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest awc-paper-cancelled-status-needs-no-json-to-explain-cancellation ()
  (awc-paper-test--project
    (let* ((awc-paper--runner (expand-file-name "slow.py" root))
           (report (get-buffer-create (format "*Paper: %s*" root))))
      (with-temp-file awc-paper--runner (insert "import time\ntime.sleep(20)\n"))
      (unwind-protect
          (save-window-excursion
            (awc-paper--start root "status")
            (awc-paper-cancel)
            (let ((deadline (+ (float-time) 3)))
              (while (and (buffer-local-value 'awc-paper--process report) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (with-current-buffer report
              (should-not awc-paper--process)
              (should (string-match-p "action cancelled" (buffer-string))))
            (should-not (get-buffer " *paper-stdout*"))
            (should-not (get-buffer " *paper-stderr*")))
        (kill-buffer report)))))

(provide 'awc-paper-test)
;;; awc-paper-test.el ends here

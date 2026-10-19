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

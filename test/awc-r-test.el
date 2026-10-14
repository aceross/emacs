;;; awc-r-test.el --- R process routing and plot lifecycle -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-r-httpgd)

(ert-deftest awc-r-project-markers-bound-nested-quarto ()
  (let ((root (make-temp-file "r project " t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "reports/chapter" root) t)
          (write-region "" nil (expand-file-name "study.Rproj" root) nil 'silent)
          (write-region "" nil (expand-file-name "reports/_quarto.yml" root) nil 'silent)
          (should (equal (awc-project-root (expand-file-name "reports/chapter" root))
                         (file-name-as-directory root)))
          (write-region "{}" nil (expand-file-name "reports/renv.lock" root) nil 'silent)
          (should (equal (awc-project-root (expand-file-name "reports/chapter" root))
                         (expand-file-name "reports/" root))))
      (delete-directory root t))))

(ert-deftest awc-r-native-commands-do-not-fall-back-to-another-project ()
  (let* ((awc-r--sessions (make-hash-table :test #'equal))
         (proc (make-pipe-process :name "r-routing-test" :buffer (generate-new-buffer " *r routing*")))
         (selection '(("/tmp/a/" "R" nil) "/tmp/a/" "R" nil)))
    (unwind-protect
        (with-temp-buffer
          (ess-r-mode)
          (setq buffer-file-name "/tmp/a/source.R")
          (cl-letf (((symbol-function 'awc-r--selection) (lambda () selection)))
            (setq-local ess-local-process-name (process-name proc))
            (should-not (ess-make-buffer-current))
            (should-not (ess-get-current-process))
            (should-error (ess-force-buffer-current nil nil t) :type 'user-error)
            (should-error (ess-get-process nil t) :type 'user-error)
            (puthash (car selection) proc awc-r--sessions)
            (should (eq (ess-get-process) proc))
            (should (eq (ess-force-buffer-current) (process-buffer proc)))
            (setq selection '(("/tmp/b/" "R" nil) "/tmp/b/" "R" nil))
            (should-not (ess-get-current-process))
            (should-not (ess-make-buffer-current))))
      (delete-process proc) (kill-buffer (process-buffer proc)))))

(ert-deftest awc-r-auxiliary-buffers-keep-their-owner ()
  (let ((proc (make-pipe-process :name "r-owner-test" :buffer nil)))
    (unwind-protect
        (with-temp-buffer
          (setq-local awc-r--owner proc)
          (cl-letf (((symbol-function 'awc-r--selection) (lambda () (error "Must use owner"))))
            (should (eq (awc-r-process) proc))
            (delete-process proc)
            (should-error (awc-r-process t) :type 'user-error)))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest awc-r-chunk-starts-project-session-and-calls-ess-with-visibility ()
  (let (started sent)
    (cl-letf (((symbol-function 'awc-r-run) (lambda () (setq started t)))
              ;; VIS is required by the installed ESS, even when nil.
              ((symbol-function 'ess-eval-region)
               (lambda (start end vis)
                 (should started) (setq sent (list start end vis)))))
      (awc-r-eval-region 10 20)
      (should (equal sent '(10 20 nil))))))

(ert-deftest awc-r-httpgd-complete-token-url-only ()
  (let ((url "http://127.0.0.1:39271/live?token=abc123&sidebar=0"))
    (should (equal (awc-r-httpgd--url (concat "noise\nAWC_HTTPGD_URL=" url "\n")) url))
    (should-not (awc-r-httpgd--url (concat "AWC_HTTPGD_URL=" url)))
    (should-not (awc-r-httpgd--url "AWC_HTTPGD_URL=http://example.com:123/live\n"))
    (should-not (awc-r-httpgd--url "AWC_HTTPGD_URL=http://localhost.evil:123/live\n"))))

(ert-deftest awc-r-httpgd-retries-and-ignores-retired-responses ()
  (let ((proc (make-pipe-process :name "r-viewer-test" :buffer nil))
        (request (list t)) callback scheduled shown)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url cb &rest _) (setq callback cb)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq scheduled args)))
                  ((symbol-function 'awc-r-httpgd--show)
                   (lambda (&rest _) (setq shown t))))
          (process-put proc 'awc-httpgd-request request)
          (process-put proc 'awc-httpgd-frame (selected-frame))
          (awc-r-httpgd--poll proc request "http://127.0.0.1:321/live?token=x")
          (with-temp-buffer
            (setq-local url-http-response-status 503)
            (funcall callback nil))
          (should scheduled) (should-not shown)
          (apply (nth 2 scheduled) (nthcdr 3 scheduled))
          (with-temp-buffer
            (setq-local url-http-response-status 200)
            (funcall callback nil))
          (should shown)
          (should-not (process-get proc 'awc-httpgd-request))
          (setq shown nil)
          (with-temp-buffer
            (setq-local url-http-response-status 200)
            (funcall callback nil))
          (should-not shown))
      (delete-process proc))))

(provide 'awc-r-test)

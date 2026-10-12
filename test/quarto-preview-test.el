;;; quarto-preview-test.el --- Preview lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'quarto-preview)

(ert-deftest quarto-preview-arguments-preserve-quoted-values ()
  (should (equal (quarto-preview--arguments "/tmp/a report.qmd"
                                           "--metadata title=\"A report\" --port 48733")
                 '("preview" "/tmp/a report.qmd" "--metadata" "title=A report"
                   "--port" "48733" "--no-browser" "--host" "127.0.0.1"))))

(ert-deftest quarto-preview-rejects-flags-that-disable-the-workflow ()
  (dolist (flags '("--quiet" "--no-serve" "--no-watch-inputs" "--no-watch"
                   "--no-navigate" "--no-render" "--host=0.0.0.0" "--browser"
                   "--log-level error" "--log-format=json-stream"))
    (should-error (quarto-preview--arguments "/tmp/report.qmd" flags)
                  :type 'user-error)))

(ert-deftest quarto-preview-accepts-the-previous-manual-preview-flags ()
  (should (equal (quarto-preview--arguments "/tmp/report.qmd"
                                           "--no-browser --host 127.0.0.1")
                 '("preview" "/tmp/report.qmd" "--no-browser" "--host" "127.0.0.1"))))

(defmacro quarto-preview-test--with-process (&rest body)
  "Run BODY with a temporary source, log and pipe process."
  (declare (indent 0))
  `(let* ((source (generate-new-buffer " *quarto source*"))
          (log (generate-new-buffer " *quarto log*"))
          (process (make-pipe-process :name "quarto-test" :buffer log :noquery t)))
     (unwind-protect
         (progn
           (with-current-buffer source (setq quarto-preview--process process))
           (with-current-buffer log (setq quarto-preview--source source))
           (process-put process 'quarto-source source)
           ,@body)
       (when (process-live-p process) (delete-process process))
       (kill-buffer log)
       (kill-buffer source))))

(ert-deftest quarto-preview-waits-for-complete-coloured-url-and-opens-once ()
  (quarto-preview-test--with-process
    (let ((scheduled 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat _callback received)
                   (should (eq received process))
                   (cl-incf scheduled))))
        (with-current-buffer log
          (insert "Render complete\n\e[32mBrowse at http://local")
          (quarto-preview--watch-output)
          (should (= scheduled 0))
          (insert "host:48733/report%20name.html")
          (quarto-preview--watch-output)
          (should (= scheduled 0))
          (insert "\e[0m\r\n")
          (quarto-preview--watch-output)
          (should (= scheduled 1))
          (insert "rebuilding...\nBrowse at http://localhost:48733/\n")
          (quarto-preview--watch-output)
          (should (= scheduled 1)))
        (should (equal (buffer-local-value 'quarto-preview--url source)
                       "http://127.0.0.1:48733/report%20name.html"))))))

(ert-deftest quarto-preview-ignores-unrelated-urls-and-retired-processes ()
  (quarto-preview-test--with-process
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (ert-fail "Should not open a browser"))))
      (with-current-buffer log
        (insert "Error; documentation at https://quarto.org/\n"
                "Browse at http://example.org:48733/\n")
        (quarto-preview--watch-output)
        (with-current-buffer source (setq quarto-preview--process nil))
        (insert "Browse at http://localhost:48733/\n")
        (quarto-preview--watch-output)))))

(ert-deftest quarto-preview-stop-from-indirect-chunk-stops-base-session ()
  (quarto-preview-test--with-process
    (let ((chunk (make-indirect-buffer source " *quarto chunk*")))
      (unwind-protect
          (progn
            (with-current-buffer source (setq quarto-preview--url "http://127.0.0.1:48733/"))
            (with-current-buffer chunk
              (should (eq (quarto-preview--source-buffer) source))
              (quarto-stop-preview))
            (should-not (process-live-p process))
            (should (process-get process 'quarto-stopping))
            (should-not (buffer-local-value 'quarto-preview--url source)))
        (kill-buffer chunk)))))

(ert-deftest quarto-preview-retired-process-does-not-display-a-failure ()
  (quarto-preview-test--with-process
    (delete-process process)
    (with-current-buffer source (setq quarto-preview--process nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (ert-fail "Old process must not interrupt a new preview"))))
      (quarto-preview--finished process "killed\n"))))

(provide 'quarto-preview-test)
;;; quarto-preview-test.el ends here

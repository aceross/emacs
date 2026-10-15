;;; awc-lsp-test.el --- New-file language server regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-files)
(require 'awc-lsp)
(require 'lsp-mode)
(require 'quarto-python-test)

(ert-deftest awc-lsp-new-file-timer-waits-for-save-and-retries-once ()
  (quarto-python-test--project
    (let* ((file (expand-file-name "new/nested/test.R" root))
           (directory (file-name-directory file))
           (create-lockfiles nil)
           (auto-save-default nil)
           (ess-r-mode-hook nil)
           (lsp-clients (make-hash-table))
           (lsp-warn-no-matched-clients nil)
           (starts 0)
           buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'lsp--require-packages)
                     (lambda ()
                       ;; A real subprocess would fail here with the original
                       ;; missing-directory error if startup ran too soon.
                       (should (zerop (process-file "true" nil nil nil)))
                       (cl-incf starts)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt &rest _)
                       (ert-fail (concat "Saving should not prompt: " prompt)))))
            (let ((noninteractive nil))
              (setq buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (insert "answer <- 42\n")
              ;; This is the callback used by lsp-deferred's idle timer.
              (lsp--init-if-visible)
              (lsp--init-if-visible)
              (should (zerop starts))
              (should-not (file-exists-p directory))
              (should (= 1 (cl-count #'awc-lsp--after-save after-save-hook)))
              (save-buffer)
              (should (file-exists-p file))
              (should (= starts 1))
              (should-not (memq #'awc-lsp--after-save after-save-hook))
              (insert "answer\n")
              (save-buffer)
              (should (= starts 1))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest awc-lsp-unsaved-file-in-existing-directory-starts-normally ()
  (quarto-python-test--project
    (let ((lsp-clients (make-hash-table))
          (lsp-warn-no-matched-clients nil)
          started)
      (setq buffer-file-name (expand-file-name "new.R" root))
      (cl-letf (((symbol-function 'lsp--require-packages)
                 (lambda () (setq started t))))
        (lsp--init-if-visible)
        (should started)
        (should-not (memq #'awc-lsp--after-save after-save-hook))))))

(provide 'awc-lsp-test)
;;; awc-lsp-test.el ends here

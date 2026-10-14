;;; awc-r-lsp-test.el --- Project R LSP regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-r-lsp)
(require 'lsp-mode)
(require 'quarto-python-test)

(ert-deftest awc-r-lsp-first-file-honors-directory-locals ()
  (quarto-python-test--project
    (let ((file (expand-file-name "first.R" root))
          (ess-r-mode-hook '(awc-r-lsp-prepare-buffer))
          (enable-local-variables :all)
          selected buffer)
      (with-temp-file file (insert "answer <- 42\n"))
      (with-temp-file (expand-file-name ".dir-locals.el" root)
        (insert "((ess-r-mode . ((inferior-ess-r-program . \"/bin/true\"))))"))
      (unwind-protect
          (cl-letf (((symbol-function 'awc-r-start-lsp)
                     (lambda () (setq selected (nth 2 (awc-r--selection))))))
            (setq buffer (find-file-noselect file))
            (should (equal selected "/bin/true")))
        (when buffer (kill-buffer buffer))))))

(ert-deftest awc-r-lsp-first-file-without-local-settings ()
  (quarto-python-test--project
    (let ((file (expand-file-name "first.R" root))
          (ess-r-mode-hook '(awc-r-lsp-prepare-buffer))
          started buffer)
      (with-temp-file file (insert "answer <- 42\n"))
      (unwind-protect
          (cl-letf (((symbol-function 'awc-r-start-lsp) (lambda () (setq started t))))
            (setq buffer (find-file-noselect file))
            (should started))
        (when buffer (kill-buffer buffer))))))

(ert-deftest awc-r-lsp-never-starts-in-chunks-or-disabled-projects ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/source.R")
    (let ((chunk (make-indirect-buffer (current-buffer) " *R LSP chunk*")))
      (unwind-protect
          (cl-letf (((symbol-function 'awc-r-start-lsp)
                     (lambda () (ert-fail "Unexpected server startup"))))
            (with-current-buffer chunk
              (ess-r-mode)
              (awc-r-lsp-prepare-buffer)
              (awc-r-lsp-after-locals)
              (should-not (memq #'awc-r-lsp-after-locals find-file-hook)))
            (ess-r-mode)
            (setq-local awc-r-lsp-auto-start nil)
            (awc-r-lsp-after-locals))
        (kill-buffer chunk)))))

(ert-deftest awc-r-lsp-launch-has-project-cwd-and-private-environment ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "renv.lock" root) (insert "{}"))
    (make-directory (expand-file-name "reports/chapters" root) t)
    (with-temp-file (expand-file-name "reports/_quarto.yml" root))
    (let* ((python (quarto-python-test--environment (expand-file-name ".venv" root)))
           (default-directory (expand-file-name "reports/chapters/" root))
           (process-environment (append '("RETICULATE_PYTHON=/old/python" "R_LIBS_USER=/custom/library")
                                        process-environment))
           (before (copy-sequence process-environment))
           (workspace (make-lsp--workspace :root root)))
      (setq buffer-file-name (expand-file-name "analysis.R" default-directory))
      (cl-letf (((symbol-function 'lsp-stdio-connection)
                 (lambda (command &rest _)
                   (should (equal (car command) (executable-find inferior-ess-r-program)))
                   (should (member "--no-restore" command))
                   (should-not (member "--vanilla" command))
                   (list :connect
                         (lambda (&rest _)
                           (should (equal default-directory root))
                           (should (equal (getenv "RETICULATE_PYTHON") python))
                           (should (equal (getenv "R_LIBS_USER") "/custom/library"))
                           'connected)))))
        (should (eq 'connected (awc-r-lsp--connect nil nil "test" nil workspace))))
      (should (equal before process-environment))
      (awc-r-lsp--check-workspace workspace)
      (setq-local inferior-ess-r-program "/bin/true")
      (should-error (awc-r-lsp--check-workspace workspace) :type 'user-error))))

(ert-deftest awc-r-lsp-rejects-a-stale-project-root ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "renv.lock" root) (insert "{}"))
    (should-error (awc-r-lsp--connect nil nil "test" nil
                                   (make-lsp--workspace :root "/tmp/elsewhere"))
                  :type 'user-error)))

(provide 'awc-r-lsp-test)

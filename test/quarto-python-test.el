;;; quarto-python-test.el --- Project environment tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'quarto-preview)

(defmacro quarto-python-test--project (&rest body)
  "Run BODY with a temporary ROOT and a Python source buffer."
  (declare (indent 0))
  `(let ((root (file-name-as-directory (make-temp-file "quarto project " t))))
     (unwind-protect
         (with-temp-buffer
           (setq default-directory root)
           (insert "```{python}\nprint(42)\n```\n")
           ,@body)
       (delete-directory root t))))

(defun quarto-python-test--environment (directory)
  "Create a minimal environment fixture at DIRECTORY; return its Python."
  (make-directory (expand-file-name "bin" directory) t)
  (with-temp-file (expand-file-name "pyvenv.cfg" directory) (insert "home = /usr/bin\n"))
  (let ((python (expand-file-name "bin/python" directory)))
    (make-symbolic-link (executable-find "sh") python)
    python))

(ert-deftest quarto-python-finds-repository-venv-above-nested-quarto-project ()
  (quarto-python-test--project
    (make-directory (expand-file-name ".git" root))
    (make-directory (expand-file-name "reports/chapters" root) t)
    (with-temp-file (expand-file-name "reports/_quarto.yml" root))
    (let ((python (quarto-python-test--environment (expand-file-name ".venv" root))))
      (should (equal (quarto-preview--python (expand-file-name "reports/chapters/a.qmd" root))
                     python))
      ;; Resolving the symlink would lose the virtual environment's identity.
      (should-not (equal python (file-truename python))))))

(ert-deftest quarto-python-prefers-nearest-environment-and-dot-venv ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (quarto-python-test--environment (expand-file-name ".venv" root))
    (quarto-python-test--environment (expand-file-name "reports/venv" root))
    (let ((python (quarto-python-test--environment (expand-file-name "reports/.venv" root))))
      (should (equal (quarto-preview--python (expand-file-name "reports/a.qmd" root)) python)))))

(ert-deftest quarto-python-does-not-inherit-another-projects-environment ()
  (quarto-python-test--project
    (quarto-python-test--environment (expand-file-name ".venv" root))
    (make-directory (expand-file-name "separate/.git" root) t)
    (should-error (quarto-preview--python (expand-file-name "separate/a.qmd" root))
                  :type 'user-error)))

(ert-deftest quarto-python-rejects-broken-venv-instead-of-falling-back ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (quarto-python-test--environment (expand-file-name ".venv" root))
    (make-directory (expand-file-name "reports/.venv" root) t)
    (should-error (quarto-preview--python (expand-file-name "reports/a.qmd" root))
                  :type 'user-error)))

(ert-deftest quarto-python-explicit-path-is-relative-to-project-root ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (make-directory (expand-file-name "reports" root))
    (let ((python (quarto-python-test--environment (expand-file-name "custom-env" root)))
          (quarto-preview-python "custom-env/bin/python"))
      (should (equal (quarto-preview--python (expand-file-name "reports/a.qmd" root)) python)))))

(ert-deftest quarto-python-r-only-document-keeps-existing-rendering ()
  (quarto-python-test--project
    (erase-buffer)
    (insert "```{r}\n1 + 1\n```\n")
    (should-not (quarto-preview--python (expand-file-name "a.qmd" root)))
    (should-not (quarto-preview--python-environment nil))))

(ert-deftest quarto-python-overrides-are-local-and-cover-both-engines ()
  (quarto-python-test--project
    (let* ((prefix (expand-file-name ".venv" root))
           (awc-project-tool-directories nil)
           (python (quarto-python-test--environment prefix))
           (compilation-environment '("QUARTO_PYTHON=/wrong/python" "PATH=/compile/bin"))
           (process-environment '("PATH=/base/bin" "VIRTUAL_ENV=/other" "PYTHONHOME=/wrong" "KEEP=value"))
           (original (copy-sequence process-environment))
           (overrides (quarto-preview--python-environment python)))
      (should (equal process-environment original))
      (should (equal compilation-environment '("QUARTO_PYTHON=/wrong/python" "PATH=/compile/bin")))
      (let ((process-environment (append overrides compilation-environment process-environment)))
        (should (equal (getenv "QUARTO_PYTHON") python))
        (should (equal (getenv "RETICULATE_PYTHON") python))
        (should (equal (getenv "VIRTUAL_ENV") prefix))
        (should (equal (getenv "PATH") (concat prefix "/bin:/compile/bin")))
        (should-not (getenv "PYTHONHOME"))
        (should (equal (getenv "KEEP") "value")))
      (should (equal (getenv "VIRTUAL_ENV") "/other")))))

(provide 'quarto-python-test)
;;; quarto-python-test.el ends here

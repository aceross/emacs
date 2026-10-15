;;; awc-project-test.el --- Environment and execution regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'awc-files)
(require 'awc-python)
(require 'quarto-render)
(require 'quarto-python-test)

(ert-deftest awc-project-old-quarto-setting-applies-to-python ()
  (quarto-python-test--project
    (let ((python (quarto-python-test--environment (expand-file-name "outside env" root))))
      (setq-local quarto-preview-python python)
      (should (equal (awc-project-python) python))
      (should (equal (awc-python-configure) python)))))

(ert-deftest awc-project-bridge-keeps-python-boundary ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (make-directory (expand-file-name "reports" root))
    (let ((default-directory (expand-file-name "reports/" root)))
      (should (equal (project-root (project-current)) root))
      (should (equal (awc-project-find-root default-directory) root)))))

(ert-deftest awc-project-new-directories-inherit-parent-project ()
  (dolist (marker '("pyproject.toml" "analysis.Rproj" "_quarto.yml"))
    (quarto-python-test--project
      (with-temp-file (expand-file-name marker root))
      (let ((default-directory (expand-file-name "new/nested/" root))
            ;; Ignore project markers belonging to the test host's /tmp.
            (locate-dominating-stop-dir-regexp
             (concat "\\`" (regexp-quote (file-name-directory (directory-file-name root))) "\\'")))
        (should (equal (awc-project-find-root default-directory) root))
        (should (equal (project-root (project-current)) root))
        (should-not (file-exists-p default-directory))))))

(ert-deftest awc-project-new-directories-without-project ()
  (quarto-python-test--project
    (let ((default-directory (expand-file-name "new/nested/" root))
          (locate-dominating-stop-dir-regexp
           (concat "\\`" (regexp-quote (file-name-directory (directory-file-name root))) "\\'")))
      (should-not (awc-project-find-root default-directory))
      (should (equal (awc-project-root) default-directory))
      (should-not (awc-project-python nil t))
      (should-not (file-exists-p default-directory)))))

(ert-deftest awc-python-new-file-in-missing-directories-can-be-saved ()
  (quarto-python-test--project
    (let* ((python (quarto-python-test--environment (expand-file-name ".venv" root)))
           (file (expand-file-name "new/nested/first.py" root))
           (directory (file-name-directory file))
           (python-mode-hook '(awc-python-prepare-buffer))
           (create-lockfiles nil)
           (auto-save-default nil)
           selected buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'awc-python-start-lsp)
                     (lambda () (setq selected python-shell-interpreter)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt &rest _)
                       (ert-fail (concat "Saving should not prompt: " prompt)))))
            ;; Batch Emacs leaves missing-directory buffers read-only; exercise
            ;; the interactive file-visiting path used by C-x C-f instead.
            (let ((noninteractive nil))
              (setq buffer (find-file-noselect file)))
            (should (equal selected python))
            (should-not (file-exists-p directory))
            (with-current-buffer buffer
              (insert "answer = 42\n")
              (save-buffer)
              (should-not (buffer-modified-p)))
            (should (file-directory-p directory))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "answer = 42\n")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest awc-project-environment-clears-inherited-activation ()
  (let* ((process-environment '("PATH=/old/bin:/conda/bin:/usr/bin" "VIRTUAL_ENV=/old"
                                "CONDA_PREFIX=/conda" "PYTHONHOME=/old" "KEEP=yes"))
         (compilation-environment nil)
         (awc-project-tool-directories nil)
         (overrides (awc-project-environment nil)))
    (let ((process-environment (append overrides process-environment)))
      (should (equal (getenv "PATH") "/usr/bin"))
      (should-not (getenv "VIRTUAL_ENV"))
      (should-not (getenv "CONDA_PREFIX"))
      (should-not (getenv "PYTHONHOME"))
      (should (equal (getenv "KEEP") "yes")))
    (should (equal (getenv "VIRTUAL_ENV") "/old"))))

(ert-deftest awc-project-identity-includes-root-and-interpreter ()
  (quarto-python-test--project
    (should-not (equal (awc-project-process-name "Python" "/one/bin/python")
                       (awc-project-process-name "Python" "/two/bin/python")))
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (let ((name (awc-project-process-name "Python" "/one/bin/python")))
      (make-directory (expand-file-name "another" root))
      (with-temp-file (expand-file-name "another/pyproject.toml" root))
      (let ((default-directory (expand-file-name "another/" root)))
        (should-not (equal name (awc-project-process-name "Python" "/one/bin/python")))))))

(ert-deftest awc-python-configuration-is-buffer-local-and-missing-env-blocks-native-commands ()
  (quarto-python-test--project
    (let ((original python-shell-interpreter)
          (environment (copy-sequence (default-value 'process-environment)))
          (python (quarto-python-test--environment (expand-file-name ".venv" root))))
      (setq-local buffer-file-name (expand-file-name "a.py" root))
      (awc-python-prepare-buffer)
      (should (equal python-shell-interpreter python))
      (should (equal (default-value 'python-shell-interpreter) original))
      (should (equal (default-value 'process-environment) environment))
      (delete-directory (expand-file-name ".venv" root) t)
      (should-error (python-shell-calculate-command) :type 'user-error)
      (should-not (python-shell-get-process)))))

(ert-deftest awc-python-indirect-buffer-uses-source-directory-locals ()
  (quarto-python-test--project
    (let* ((python (quarto-python-test--environment (expand-file-name "external" root)))
           (chunk (make-indirect-buffer (current-buffer) " *awc chunk*")))
      (unwind-protect
          (progn
            (setq-local awc-project-python python)
            (with-current-buffer chunk (should (equal (awc-project-python) python))))
        (kill-buffer chunk)))))

(ert-deftest awc-project-broken-environment-is-not-optional ()
  (quarto-python-test--project
    (make-directory (expand-file-name ".venv" root))
    (should-error (awc-project-python nil t) :type 'user-error)))

(ert-deftest quarto-render-arguments-are-not-shell-code ()
  (should (equal (quarto-render--arguments "/tmp/a report.qmd"
                                          "--metadata title=\"two words\" ';touch /tmp/never'")
                 '("render" "/tmp/a report.qmd" "--metadata" "title=two words" ";touch /tmp/never"))))

(ert-deftest quarto-render-from-indirect-buffer-saves-and-reuses-process ()
  (quarto-python-test--project
    (make-directory (expand-file-name "reports/chapter" root) t)
    (with-temp-file (expand-file-name "renv.lock" root) (insert "{}"))
    (with-temp-file (expand-file-name "reports/_quarto.yml" root))
    (let* ((file (expand-file-name "reports/chapter/a report.qmd" root))
           (log (generate-new-buffer " *render test log*"))
           (process (make-pipe-process :name "render-test" :buffer log :noquery t))
           (source (current-buffer))
           (chunk (make-indirect-buffer source " *render chunk*"))
           (starts 0))
      (unwind-protect
          (progn
            (setq buffer-file-name file)
            (erase-buffer) (insert "---\nengine: markdown\n---\nInitial\n")
            (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/true"))
                      ((symbol-function 'quarto-context-resolve)
                       (lambda (callback &rest _) (funcall callback (list :root root) nil)))
                      ((symbol-function 'compilation-start)
                       (lambda (command &rest _)
                         (should (string-match-p "a\\\\ report.qmd" command))
                         (should (equal default-directory root))
                         (cl-incf starts) log)))
              (with-current-buffer chunk (quarto-build-document))
              (should (file-exists-p file))
              (should (eq quarto-render--process process))
              (with-current-buffer chunk (quarto-build-document))
              (should (= starts 1))
              (with-current-buffer log (quarto-stop-render))
              (should-not (process-live-p process))))
        (when (process-live-p process) (delete-process process))
        (kill-buffer chunk) (kill-buffer log)))))

(ert-deftest awc-babel-respects-explicit-python-and-named-sessions ()
  (dolist (params '(((:python . "/explicit/python")) ((:session . "existing"))))
    (cl-letf (((symbol-function 'awc-project-python)
               (lambda (&rest _) (ert-fail "Explicit Babel settings must win"))))
      (should (equal (awc-python-babel (lambda (body _) body) "42" params) "42")))))

(provide 'awc-project-test)

(ert-deftest awc-python-first-chunk-waits-for-shell-bootstrap ()
  (let* ((buffer (generate-new-buffer " *python startup*"))
         (proc (make-pipe-process :name "python-start-test" :buffer buffer :noquery t))
         (python-shell-buffer-name "python-start-test")
         waited)
    (unwind-protect
        (cl-letf (((symbol-function 'awc-python-configure) (lambda () "/usr/bin/python3"))
                  ((symbol-function 'awc-project-root) (lambda (&rest _) "/tmp/"))
                  ((symbol-function 'run-python) (lambda (&rest _) proc))
                  ((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'accept-process-output)
                   (lambda (process &rest _)
                     (should (eq process proc))
                     (setq waited t)
                     (with-current-buffer buffer
                       (setq-local python-shell--first-prompt-received t)))))
          (should (eq (awc-python-run) proc))
          (should waited))
      (delete-process proc) (kill-buffer buffer))))

(ert-deftest awc-python-first-file-without-locals-starts-lsp-after-selection ()
  (quarto-python-test--project
    (let ((python (quarto-python-test--environment (expand-file-name ".venv" root)))
          (file (expand-file-name "first.py" root))
          (python-mode-hook '(awc-python-prepare-buffer))
          selected buffer)
      (with-temp-file file (insert "answer = 42\n"))
      (unwind-protect
          (cl-letf (((symbol-function 'awc-python-start-lsp)
                     (lambda () (setq selected python-shell-interpreter))))
            (setq buffer (find-file-noselect file))
            (should (equal selected python)))
        (when buffer (kill-buffer buffer))))))

(ert-deftest awc-project-argument-quoting-and-errors ()
  (should (equal (awc-project-split-arguments "--title 'one two' \"\" literal\\ space '$HOME;echo' title=\"two words\"")
                 '("--title" "one two" "" "literal space" "$HOME;echo" "title=two words")))
  (should-error (awc-project-split-arguments "'broken") :type 'user-error)
  (should-error (awc-project-split-arguments "broken\\") :type 'user-error))

(ert-deftest awc-project-command-only-offers-to-save-its-own-project ()
  (quarto-python-test--project
    (with-temp-file (expand-file-name "pyproject.toml" root))
    (let ((other (generate-new-buffer " *unrelated source*")))
      (unwind-protect
          (cl-letf (((symbol-function 'save-some-buffers)
                     (lambda (_ predicate)
                       (with-current-buffer other
                         (setq buffer-file-name "/tmp/unrelated.py" default-directory "/tmp/")
                         (should-not (funcall predicate)))))
                    ((symbol-function 'compilation-start) (lambda (&rest _) nil)))
            (awc-project-compile "true"))
        (kill-buffer other)))))

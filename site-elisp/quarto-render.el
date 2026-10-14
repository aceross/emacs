;;; quarto-render.el --- Save and render a Quarto source -*- lexical-binding: t; -*-
(require 'quarto-preview)

(defcustom quarto-build-flags ""
  "Extra render arguments, using shell-style quoting without shell expansion."
  :group 'quarto :type 'string)
(make-variable-buffer-local 'quarto-build-flags)
(defvar-local quarto-render--process nil)

(defun quarto-render--arguments (file flags)
  "Build a render argument vector for FILE and FLAGS."
  (append (list "render" file) (awc-project-split-arguments flags)))

(defun quarto-stop-render ()
  "Stop this source's render, including when called from its log or chunk."
  (interactive)
  (let ((source (quarto-preview--source-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (process-live-p quarto-render--process)
          (delete-process quarto-render--process))))))

(defun quarto-build-document (&optional restart)
  "Save and render this Quarto document in its project Python environment.
Reuse an active render log.  With RESTART stop it and render again.
Works from prose, an indirect chunk, or this document's render log."
  (interactive "P")
  (let ((source (quarto-preview--source-buffer)))
    (unless (buffer-live-p source) (user-error "The Quarto source buffer was closed"))
    (with-current-buffer source
      (unless (and buffer-file-name
                   (equal (downcase (or (file-name-extension buffer-file-name) "")) "qmd"))
        (user-error "Open a saved .qmd file to render"))
      (when (file-remote-p buffer-file-name) (user-error "Quarto rendering requires a local file"))
      (let* ((program (or (executable-find "quarto") (user-error "Cannot find quarto on exec-path")))
             (args (quarto-render--arguments buffer-file-name quarto-build-flags))
             (python (quarto-preview--python buffer-file-name)))
        (save-buffer)
        (when restart (quarto-stop-render))
        (if (process-live-p quarto-render--process)
            (display-buffer (process-buffer quarto-render--process))
          (let* ((default-directory (awc-project-root))
                 (compilation-environment
                  (append (quarto-preview--python-environment python) compilation-environment))
                 (name (format "*Quarto render: %s*" (abbreviate-file-name buffer-file-name)))
                 (log (compilation-start
                       (mapconcat #'shell-quote-argument (cons program args) " ")
                       'compilation-mode (lambda (_) name))))
            (setq quarto-render--process (get-buffer-process log))
            (process-put quarto-render--process 'quarto-python python)
            (with-current-buffer log
              (setq-local quarto-preview--source source)
              (setq-local header-line-format (concat " Quarto Python: " (or python "not requested (R/Markdown)"))))
            (add-hook 'kill-buffer-hook #'quarto-stop-render nil t)))))))

(defun quarto-check-python ()
  "Show the selected interpreter's dependencies and available kernelspecs.
A named kernel can override QUARTO_PYTHON.  Compare its argv[0] with the
selected path; relative argv uses PATH.  This command does not repair kernels."
  (interactive)
  (with-current-buffer (quarto-preview--source-buffer)
    (let* ((python (awc-project-python))
           (process-environment (append (awc-project-environment python) process-environment))
           (buffer (get-buffer-create "*Quarto Python check*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Selected Python: %s\n\n" python))
          (call-process python nil t nil "-c"
                        "import sys, importlib.util; print('Executable:', sys.executable); print('Prefix:', sys.prefix); [print(m, bool(importlib.util.find_spec(m))) for m in ('ipykernel', 'nbclient', 'yaml')]")
          (insert "\nKernelspecs (check argv[0] for a named kernel):\n")
          (call-process python nil t nil "-m" "jupyter" "kernelspec" "list" "--json")
          (goto-char (point-min))
          (special-mode)))
      (display-buffer buffer))))

(provide 'quarto-render)
;;; quarto-render.el ends here

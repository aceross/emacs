;;; quarto-context.el --- Resolved document environments -*- lexical-binding: t; -*-
(require 'quarto-yaml)
(defvar-local quarto-format nil)
(defvar-local quarto-profile nil)
(defvar-local quarto-context--value nil)
(defvar-local quarto-context--process nil)
(defvar-local quarto-context-error nil)

(defun quarto-context-flags ()
  (with-current-buffer (quarto-source-buffer)
    (append (when quarto-format (list "--to" quarto-format))
            (when quarto-profile (list "--profile" quarto-profile)))))

(defun quarto-context-metadata ()
  "Return the current YAML front matter without its delimiters."
  (with-current-buffer (quarto-source-buffer)
    (let ((front (quarto-document-front-matter (quarto-document-model))))
      (if (not front) "{}"
        (save-excursion (save-restriction
          (widen) (goto-char (car front)) (forward-line 1)
          (let ((start (point)))
            (goto-char (cdr front)) (forward-line -1)
            (buffer-substring-no-properties start (point)))))))))

(defun quarto-context--merge (base overlay)
  "Recursively merge metadata without modifying BASE."
  (let ((result (if (hash-table-p base) (copy-hash-table base) (make-hash-table :test #'equal))))
    (when (hash-table-p overlay)
      (maphash (lambda (key value)
                 (puthash key (if (hash-table-p value)
                                  (quarto-context--merge (gethash key result) value) value) result)) overlay))
    result))

(defun quarto-context-resolve (callback &optional refresh)
  "Resolve this document asynchronously; CALLBACK receives (context error).
The saved project's effective configuration comes from quarto inspect.
Unsaved front matter is parsed independently and overlays that metadata."
  (with-current-buffer (quarto-source-buffer)
    (let* ((source (current-buffer)) (revision (buffer-chars-modified-tick))
           (file buffer-file-name) (root (awc-project-root))
           (python (awc-project-python nil t))
           (key (list revision quarto-format quarto-profile root python)))
      (if (and (not refresh) (equal key (plist-get quarto-context--value :key)))
          (funcall callback quarto-context--value nil)
        (unless (and file (file-readable-p file)) (user-error "Save the document once before resolving its environment"))
        (when (file-remote-p file) (user-error "Quarto currently requires a local document"))
        (let* ((default-directory root)
               (process-environment (append (awc-project-environment python) process-environment))
               (log (generate-new-buffer " *Quarto inspect*"))
               (errors (get-buffer-create (format "*Quarto inspect: %s*" (buffer-name))))
               (format-choice quarto-format) (profile quarto-profile)
               (metadata (quarto-context-metadata)))
          (setq quarto-context--process
                (make-process
                 :name "quarto-inspect" :buffer log :stderr errors :coding 'utf-8-unix :noquery t
                 :command (append (list (or (executable-find "quarto") (user-error "Cannot find quarto")) "inspect" file)
                                  (when profile (list "--profile" profile)))
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (unwind-protect
                         (when (buffer-live-p source)
                           (if (/= 0 (process-exit-status process))
                               (with-current-buffer source
                                 (setq quarto-context-error (format "quarto inspect failed; see %s" (buffer-name errors)))
                                 (funcall callback nil quarto-context-error))
                             (condition-case err
                                 (let ((inspection (with-current-buffer log
                                                     (json-parse-string (buffer-string) :object-type 'hash-table :array-type 'list :null-object nil))))
                                   (quarto-yaml-request
                                    "parse" (list :code metadata)
                                    (lambda (overlay error)
                                      (when (buffer-live-p source)
                                        (with-current-buffer source
                                          (cond
                                           (error (setq quarto-context-error error) (funcall callback nil error))
                                           ((not (= revision (buffer-chars-modified-tick)))
                                            (funcall callback nil "Document changed while resolving its environment; retry"))
                                           (t
                                            (let* ((formats (quarto-get inspection "formats"))
                                                   (format-name (or format-choice (car (and formats (hash-table-keys formats)))))
                                                   (format-data (quarto-get formats format-name))
                                                   (effective (quarto-context--merge (quarto-get format-data "metadata") overlay))
                                                   (project (quarto-get inspection "project"))
                                                   (project-dir (or (quarto-get project "dir") root))
                                                   (config (quarto-get project "config"))
                                                   (exec-dir (or (quarto-get effective "execute-dir")
                                                                 (quarto-get (quarto-get config "project") "execute-dir")))
                                                   (jupyter (quarto-get effective "jupyter"))
                                                   (engine (cond ((quarto-get effective "engine") (quarto-get effective "engine"))
                                                                 (jupyter "jupyter")
                                                                 ((quarto-get effective "knitr") "knitr")
                                                                 (t (car (quarto-get inspection "engines"))))))
                                              (setq quarto-context-error nil
                                                    quarto-context--value
                                                    (list :key key :source source :file file :root root :python python
                                                          :project-directory project-dir :engine engine :metadata effective
                                                          :inspection inspection :format format-name :profile profile
                                                          :kernel (if (stringp jupyter) jupyter (quarto-get jupyter "kernel"))
                                                          :directory (file-name-as-directory
                                                                      (cond ((equal exec-dir "project") project-dir)
                                                                            ((and (stringp exec-dir) (not (equal exec-dir "file")))
                                                                             (expand-file-name exec-dir project-dir))
                                                                            (t (file-name-directory file))))))
                                              (funcall callback quarto-context--value nil)))))))))
                               (error (funcall callback nil (error-message-string err))))))
                       (kill-buffer log)))))))))))

(provide 'quarto-context)

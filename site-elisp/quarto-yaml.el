;;; quarto-yaml.el --- Quarto's own YAML schemas over JSON lines -*- lexical-binding: t; -*-
(require 'quarto-document)
(defconst quarto-tools-directory (expand-file-name "quarto-tools" (file-name-directory (or load-file-name buffer-file-name))))
(defcustom quarto-editor-bundle nil
  "Override the selected Quarto's editor/tools/vs-code.mjs path."
  :type '(choice (const nil) file) :group 'quarto)
(defvar quarto-yaml--process nil)
(defvar quarto-yaml--pending (make-hash-table :test #'eql))
(defvar quarto-yaml--serial 0)
(defvar-local quarto-yaml-error nil)
(defvar-local quarto-yaml-diagnostics nil)
(defvar-local quarto-yaml--timer nil)
(defvar-local quarto-yaml--completion nil)

(defun quarto-get (object key &optional default)
  "Read KEY from a JSON OBJECT, tolerating absent fields."
  (if (hash-table-p object) (gethash key object default) default))

(defun quarto-yaml--bundle ()
  (or quarto-editor-bundle
      (when-let* ((quarto (executable-find "quarto")))
        (expand-file-name "../share/editor/tools/vs-code.mjs" (file-name-directory (file-truename quarto))))
      (user-error "Quarto executable is missing; YAML assistance unavailable")))

(defun quarto-yaml--filter (process output)
  (let* ((text (concat (process-get process 'partial) output)) (start 0))
    (while (string-match "\n" text start)
      (let* ((line (substring text start (match-beginning 0)))
             (next (match-end 0)))
        (condition-case err
            (let* ((reply (json-parse-string line :object-type 'hash-table :array-type 'list :null-object nil))
                   (id (quarto-get reply "id")) (callback (gethash id quarto-yaml--pending)))
              (remhash id quarto-yaml--pending)
              (when callback (funcall callback (quarto-get reply "value") (quarto-get reply "error"))))
          (error (message "Quarto YAML response: %s" (error-message-string err))))
        (setq start next)))
    (process-put process 'partial (substring text start))))

(defun quarto-yaml--start ()
  (unless (process-live-p quarto-yaml--process)
    (let ((bundle (quarto-yaml--bundle)))
      (setq quarto-yaml--process
            (make-process
             :name "quarto-editor" :command (list (or (executable-find "node") (user-error "YAML assistance needs Node"))
                                                  (expand-file-name "editor.mjs" quarto-tools-directory) bundle)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :stderr (get-buffer-create "*Quarto editor log*") :filter #'quarto-yaml--filter
             :sentinel (lambda (process event)
                         (unless (process-live-p process)
                           (let ((callbacks (hash-table-values quarto-yaml--pending)))
                             (clrhash quarto-yaml--pending)
                             (dolist (callback callbacks)
                               (funcall callback nil (concat "Quarto YAML helper ended: " event "; see *Quarto editor log*"))))))))))
  quarto-yaml--process)

(defun quarto-yaml-request (method payload callback)
  "Call editor METHOD with PAYLOAD asynchronously, invoking CALLBACK(value,error)."
  (condition-case err
      (let* ((process (quarto-yaml--start)) (id (cl-incf quarto-yaml--serial)))
        (puthash id callback quarto-yaml--pending)
        (process-send-string process (concat (json-serialize (append (list :id id :method method) payload)) "\n")))
    (error (funcall callback nil (error-message-string err)))))

(defun quarto-yaml-context (&optional position)
  (let ((pos (or position (point))))
    (with-current-buffer (quarto-source-buffer)
      (save-excursion (save-restriction
        (widen) (goto-char pos)
        (list :code (buffer-substring-no-properties (point-min) (point-max))
              :line (buffer-substring-no-properties (line-beginning-position) (line-end-position))
              :position (list :row (1- (line-number-at-pos)) :column (- (point) (line-beginning-position)))
              :filetype (if (and buffer-file-name (string-match-p "\\.ya?ml\\'" buffer-file-name)) "yaml" "markdown")
              :path (or buffer-file-name "document.qmd") :client "lsp"
              :formats ["html" "pdf"] :project_formats []))))))

(defun quarto-yaml-position-p ()
  (or (and buffer-file-name (string-match-p "_quarto\\(?:-[^.]+\\)?\\.ya?ml\\'" buffer-file-name))
      (let ((front (quarto-document-front-matter (quarto-document-model))))
        (and front (<= (car front) (point)) (< (point) (cdr front))))
      (and (quarto-chunk-at) (save-excursion (beginning-of-line) (looking-at-p "[ >\t]*#|")))))

(defun quarto-yaml-refresh (source)
  (when (buffer-live-p source)
    (with-current-buffer source
      (let ((revision (buffer-chars-modified-tick)))
        (quarto-yaml-request
         "getLint" (list :context (quarto-yaml-context))
         (lambda (value error)
           (when (and (buffer-live-p source) (= revision (buffer-chars-modified-tick source)))
             (with-current-buffer source
               (setq quarto-yaml-error error quarto-yaml-diagnostics
                     (mapcar (lambda (item)
                               (save-excursion
                                 (goto-char (point-min)) (forward-line (quarto-get item "start.row" 0))
                                 (list (+ (point) (quarto-get item "start.column" 0))
                                       (intern (quarto-get item "type" "error")) (quarto-get item "text")))) value))
               (when (bound-and-true-p flycheck-mode) (flycheck-buffer))))))))))

(defun quarto-yaml-changed (source _begin)
  (with-current-buffer source
    (when (timerp quarto-yaml--timer) (cancel-timer quarto-yaml--timer))
    (setq quarto-yaml-diagnostics nil
          quarto-yaml--timer (run-with-idle-timer 0.6 nil #'quarto-yaml-refresh source))))

(defun quarto-yaml-completion-at-point ()
  "Complete using the installed Quarto editor schema, with a bounded wait."
  (when (quarto-yaml-position-p)
    (let ((end (point)) (start (save-excursion (skip-chars-backward "[:alnum:]_./-") (point)))
          value done error (revision (buffer-chars-modified-tick (quarto-source-buffer))))
      (quarto-yaml-request "getCompletions" (list :context (quarto-yaml-context))
                           (lambda (result err) (setq value result error err done t)))
      (let ((deadline (+ (float-time) 1.5)))
        (while (and (not done) (< (float-time) deadline)) (accept-process-output quarto-yaml--process 0.02)))
      (setq quarto-yaml-error error)
      (when (= revision (buffer-chars-modified-tick (quarto-source-buffer)))
        (let ((items (quarto-get value "completions")))
          (list start end (mapcar (lambda (item) (quarto-get item "value")) items)
                :exclusive 'no
                :annotation-function
                (lambda (candidate)
                  (let ((item (cl-find candidate items :key (lambda (x) (quarto-get x "value")) :test #'equal)))
                    (concat " " (quarto-get item "description" ""))))))))))

(defun quarto-yaml-eldoc (callback &rest _)
  (when (quarto-yaml-position-p)
    (let ((source (quarto-source-buffer)) (revision (buffer-chars-modified-tick (quarto-source-buffer))))
      (quarto-yaml-request
       "getHover" (list :context (quarto-yaml-context))
       (lambda (value _error)
         (when (and value (buffer-live-p source) (= revision (buffer-chars-modified-tick source)))
           (funcall callback (if (stringp value) value (or (quarto-get value "content")
                                                          (quarto-get value "contents") (format "%s" value)))))))
      t)))

(add-hook 'quarto-document-change-hook #'quarto-yaml-changed)
(provide 'quarto-yaml)

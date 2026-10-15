;;; quarto-yaml.el --- Quarto's own YAML schemas over JSON lines -*- lexical-binding: t; -*-
(require 'quarto-document)
(eval-when-compile (require 'corfu) (require 'corfu-popupinfo))
(defvar quarto-context--value)
(defvar quarto-format)
(declare-function markdown-mode "markdown-mode" ())
(defconst quarto-tools-directory (expand-file-name "quarto-tools" (file-name-directory (or load-file-name buffer-file-name))))
(defcustom quarto-editor-bundle nil
  "Override the selected Quarto's editor/tools/vs-code.mjs path."
  :type '(choice (const nil) file) :group 'quarto)
(defvar quarto-yaml--process nil)
(defvar quarto-yaml--pending (make-hash-table :test #'eql))
(defvar quarto-yaml--timeouts (make-hash-table :test #'eql))
(defvar quarto-yaml--serial 0)
(defvar-local quarto-yaml-error nil)
(defvar-local quarto-yaml-diagnostics nil)
(defvar-local quarto-yaml--timer nil)
(defvar-local quarto-yaml--completion nil)
(defvar-local quarto-yaml--completion-request nil)
(defvar-local quarto-yaml--completion-generation 0)
(defvar-local quarto-yaml--ui-defaults nil)
(defvar-local quarto-yaml--ui-position nil)
(defvar quarto-yaml--redisplaying nil)

(defun quarto-yaml--finish-request (id value error)
  "Complete request ID exactly once, including timeout and process failures."
  (when-let* ((timer (gethash id quarto-yaml--timeouts))) (cancel-timer timer))
  (remhash id quarto-yaml--timeouts)
  (when-let* ((callback (gethash id quarto-yaml--pending)))
    (remhash id quarto-yaml--pending)
    (funcall callback value error)))

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
                   (id (quarto-get reply "id")))
              (quarto-yaml--finish-request id (quarto-get reply "value") (quarto-get reply "error")))
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
                         (when (and (not (process-live-p process)) (eq process quarto-yaml--process))
                           (dolist (id (hash-table-keys quarto-yaml--pending))
                             (quarto-yaml--finish-request
                              id nil (concat "Quarto YAML helper ended: " event "; see *Quarto editor log*")))))))))
  quarto-yaml--process)

(defun quarto-yaml-request (method payload callback)
  "Call editor METHOD with PAYLOAD asynchronously, invoking CALLBACK(value,error)."
  (let (id)
   (condition-case err
      (let ((process (quarto-yaml--start)))
        (setq id (cl-incf quarto-yaml--serial))
        (puthash id callback quarto-yaml--pending)
        (puthash id (run-at-time 5 nil #'quarto-yaml--finish-request id nil "Quarto YAML helper timed out")
                 quarto-yaml--timeouts)
        (process-send-string process (concat (json-serialize (append (list :id id :method method) payload)) "\n"))
        id)
    (error (if id (quarto-yaml--finish-request id nil (error-message-string err))
             (funcall callback nil (error-message-string err)))))))

(defun quarto-yaml-context (&optional position assistance)
  "Build schema context at POSITION; ASSISTANCE normalises the current chunk.
Use cached project context and unsaved metadata, never inspect on a keystroke."
  (let ((pos (or position (point))))
    (with-current-buffer (quarto-source-buffer)
      (save-excursion (save-restriction
        (widen) (goto-char pos)
        (let* ((model (quarto-document-model)) (front (quarto-document-front-matter model))
               (project-yaml (and buffer-file-name (string-match-p "\\.ya?ml\\'" buffer-file-name)))
               (context (bound-and-true-p quarto-context--value))
               (format (bound-and-true-p quarto-format))
               (chunk (and assistance (quarto-chunk-at pos)))
               (code (buffer-substring-no-properties (point-min) (point-max)))
               (row (1- (line-number-at-pos)))
               (column (- (point) (line-beginning-position))) metadata open)
          (if project-yaml (setq metadata code)
            (when front
              (save-excursion
                (goto-char (car front)) (forward-line 1)
                (let* ((start (point))
                       (closed (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" (cdr front) t)))
                  (setq open (not closed)
                        metadata (buffer-substring-no-properties start (if closed (match-beginning 0) (cdr front))))))))
          (when (and chunk (member (quarto-chunk-language chunk) '("r" "python")))
            (setq code (concat "```{" (quarto-chunk-language chunk) "}\n" (quarto-chunk-code chunk) "\n```\n")
                  row (1+ (- row (1- (line-number-at-pos (quarto-chunk-head-end chunk)))))
                  column (max 0 (- column (length (quarto-chunk-prefix chunk))))))
          (let ((line (or (nth row (split-string code "\n")) "")))
            (list :code code :line line
                  :position (list :row row :column (/ (length (encode-coding-string
                                                              (substring line 0 (min column (length line))) 'utf-16le t)) 2))
                  :filetype (if project-yaml "yaml" "markdown")
                  :path (or buffer-file-name "document.qmd") :client "lsp"
                  :metadata (or metadata "") :front_matter_open (if (and open (not chunk)) t :false)
                  :engine (or (plist-get context :engine)
                              (if (cl-find "r" (quarto-document-chunks model) :key #'quarto-chunk-language :test #'equal)
                                  "knitr" "jupyter"))
                  :selected_format (or format :false)
                  :formats (vconcat (delq nil (list (or format (plist-get context :format)))))
                  :project_formats (vconcat (when-let* ((formats (quarto-get (plist-get context :inspection) "formats")))
                                             (hash-table-keys formats)))))))))))

(defun quarto-yaml-position-p ()
  (or (and buffer-file-name (string-match-p "_quarto\\(?:-[^.]+\\)?\\.ya?ml\\'" buffer-file-name))
      (let ((front (quarto-document-front-matter (quarto-document-model))))
        (and front (< (car front) (point)) (<= (point) (cdr front))
             (not (save-excursion (beginning-of-line) (looking-at-p "\\(?:---\\|\\.\\.\\.\\)[ \t]*$")))
             (or (< (point) (cdr front))
                 (save-excursion
                   (goto-char (car front)) (forward-line 1)
                   (not (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" (cdr front) t))))))
      (when-let* ((chunk (quarto-chunk-at)))
        (and (member (quarto-chunk-language chunk) '("r" "python"))
             (save-excursion (beginning-of-line) (looking-at-p "[ >\t]*#|"))))))

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

(defun quarto-yaml--key ()
  "Identify a request by source revision, cursor and selected context."
  (let ((position (point)) (generation quarto-yaml--completion-generation))
    (with-current-buffer (quarto-source-buffer)
      (list (buffer-chars-modified-tick) position generation (bound-and-true-p quarto-format)
            (bound-and-true-p quarto-context--value)))))

(defun quarto-yaml--target ()
  "Locate the editable token, its suffix and any existing colon."
  (let* ((start (save-excursion (skip-chars-backward "[:alnum:]_./-") (point)))
         (end (save-excursion (skip-chars-forward "[:alnum:]_./-") (point)))
         (colon (save-excursion (beginning-of-line) (search-forward ":" (line-end-position) t)))
         (kind (if (and colon (>= (point) colon)) 'value 'key)))
    (list :start start :end end :kind kind
          :colon (and (eq kind 'key) (save-excursion (goto-char end) (looking-at-p "[ \t]*:")))
          :prefix (buffer-substring-no-properties (line-beginning-position) start))))

(defun quarto-yaml--show-completions (buffer key manual)
  "Show a reply only while BUFFER still owns the cursor for KEY."
  (when (and (buffer-live-p buffer) (not (active-minibuffer-window))
             (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (when (and (equal key (quarto-yaml--key)) (quarto-yaml-position-p)
                 (not buffer-read-only))
        (let ((quarto-yaml--redisplaying t))
          (if (and (bound-and-true-p corfu-mode) (display-graphic-p))
              (progn
                (when completion-in-region-mode (corfu-quit))
                (let ((corfu-auto-prefix (if manual 0 2)))
                  (corfu--auto-complete-deferred)))
            (when manual (completion-at-point))))))))

(defun quarto-yaml--fetch-completions (key)
  "Request KEY without waiting in the editor's command loop."
  (unless (or quarto-yaml--redisplaying (equal key quarto-yaml--completion-request))
    (let* ((buffer (current-buffer))
           (manual (memq this-command '(completion-at-point corfu-complete)))
           (target (quarto-yaml--target))
           (display (or manual (and (bound-and-true-p corfu-auto) (>= (- (point) (plist-get target :start)) 2)))))
      (setq quarto-yaml--completion-request key)
      (quarto-yaml-request
       "getCompletions" (list :context (quarto-yaml-context nil t))
       (lambda (value error)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (equal key quarto-yaml--completion-request)
               (setq quarto-yaml--completion-request nil)
               (when (equal key (quarto-yaml--key))
                 (setq quarto-yaml--completion (list :key key :value value :error error)
                       quarto-yaml-error error)
                 (when (and display value)
                   (run-at-time 0 nil #'quarto-yaml--show-completions buffer key manual)))))))))))

(defun quarto-yaml--documentation (candidate items)
  "Return a reusable documentation buffer for CANDIDATE in ITEMS."
  (when-let* ((item (assoc candidate items)) (description (quarto-get (cdr item) "description"))
              (_ (not (string-empty-p description))))
    (with-current-buffer (get-buffer-create " *Quarto YAML documentation*")
      (let ((inhibit-read-only t))
        (erase-buffer) (insert "## " (string-trim candidate) "\n\n" description)
        (when (fboundp 'markdown-mode) (delay-mode-hooks (markdown-mode)) (font-lock-ensure))
        (setq buffer-read-only t))
      (current-buffer))))

(defun quarto-yaml--follow-up (buffer position)
  "Offer schema-supplied values after accepting an option key."
  (when (and (buffer-live-p buffer) (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (when (and (= (point) position) (quarto-yaml-position-p))
        (let ((this-command 'completion-at-point)) (quarto-yaml-completion-at-point))))))

(defun quarto-yaml-completion-at-point ()
  "Offer cached schema completion; fetch a missing response asynchronously."
  (when (quarto-yaml-position-p)
    (let* ((key (quarto-yaml--key)) (target (quarto-yaml--target))
           (start (plist-get target :start)) (end (plist-get target :end))
           (cached (and (equal key (plist-get quarto-yaml--completion :key))
                        (not (and (memq this-command '(completion-at-point corfu-complete))
                                  (plist-get quarto-yaml--completion :error))))))
      (unless cached (quarto-yaml--fetch-completions key))
      (let* ((value (and cached (plist-get quarto-yaml--completion :value)))
             (prefix (replace-regexp-in-string "[-+*] \\'" "  " (plist-get target :prefix)))
             (items (mapcar (lambda (item)
                              (let ((text (quarto-get item "value")))
                                (when (and (plist-get target :colon) (equal (quarto-get item "type") "key"))
                                  (setq text (car (split-string text ":"))))
                                (cons (replace-regexp-in-string "\n" (concat "\n" prefix) text t t) item)))
                            (quarto-get value "completions")))
             (suffix (buffer-substring-no-properties (point) end))
             (suffix-end (copy-marker end t)) (buffer (current-buffer)))
        (list start (point) (mapcar #'car items)
              :exclusive t
              :annotation-function
              (lambda (candidate)
                (concat " " (truncate-string-to-width
                             (replace-regexp-in-string "[\n\t ]+" " " (quarto-get (cdr (assoc candidate items)) "description" ""))
                             65 nil nil "…")))
              :company-doc-buffer (lambda (candidate) (quarto-yaml--documentation candidate items))
              :exit-function
              (lambda (candidate status)
                (when (and (eq status 'finished) (buffer-live-p buffer))
                  (with-current-buffer buffer
                    ;; Corfu inserts the candidate before this callback. Only
                    ;; remove the original suffix; never touch adjacent YAML.
                    (when (and (<= (point) suffix-end)
                               (equal suffix (buffer-substring-no-properties (point) suffix-end)))
                      (delete-region (point) suffix-end))
                    (set-marker suffix-end nil)
                    (when (or (equal "key" (quarto-get (cdr (assoc candidate items)) "type"))
                              (eq t (quarto-get (cdr (assoc candidate items)) "suggest_on_accept")))
                      (when (looking-at "[ \t]*:[ \t]*") (goto-char (match-end 0)))
                      (run-at-time 0 nil #'quarto-yaml--follow-up buffer (point)))))))))))

(defun quarto-yaml--post-command ()
  "Apply YAML-only Corfu settings and close menus after leaving YAML."
  (when (memq this-command '(keyboard-quit keyboard-escape-quit corfu-quit corfu-reset))
    (cl-incf quarto-yaml--completion-generation))
  (let ((yaml (quarto-yaml-position-p)))
    (when (and quarto-yaml--ui-position (not yaml) (bound-and-true-p completion-in-region-mode))
      (corfu-quit))
    (setq quarto-yaml--ui-position yaml)
    (dolist (setting quarto-yaml--ui-defaults)
      (set (make-local-variable (car setting))
           (if yaml (pcase (car setting)
                      ('corfu-auto-prefix 2) ('corfu-auto-delay 0.25)
                      ('corfu-popupinfo-mode t) ('corfu-popupinfo-delay '(0.5 . 0.2)))
             (cdr setting))))))

(defun quarto-yaml-setup ()
  "Install the YAML UI locally, preserving ordinary code completion settings."
  (require 'corfu)
  (require 'corfu-popupinfo)
  (unless quarto-yaml--ui-defaults
    (setq quarto-yaml--ui-defaults
          (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                  '(corfu-auto-prefix corfu-auto-delay corfu-popupinfo-mode corfu-popupinfo-delay))))
  (add-hook 'post-command-hook #'quarto-yaml--post-command -90 t)
  (quarto-yaml--post-command))

(defun quarto-yaml-restart ()
  "Restart schema assistance after correcting a missing helper or bundle."
  (interactive)
  (when (process-live-p quarto-yaml--process) (delete-process quarto-yaml--process))
  (setq quarto-yaml--process nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (local-variable-p 'quarto-yaml--completion)
        (setq quarto-yaml--completion nil quarto-yaml--completion-request nil quarto-yaml-error nil))))
  (when (quarto-yaml-position-p)
    (let ((this-command 'completion-at-point)) (quarto-yaml-completion-at-point))))

(defun quarto-yaml-eldoc (callback &rest _)
  (when (quarto-yaml-position-p)
    (let ((source (quarto-source-buffer)) (buffer (current-buffer)) (key (quarto-yaml--key)))
      (quarto-yaml-request
       "getHover" (list :context (quarto-yaml-context nil t))
       (lambda (value _error)
         (when (and value (buffer-live-p source) (buffer-live-p buffer))
           (with-current-buffer buffer
             (when (and (equal key (quarto-yaml--key)) (quarto-yaml-position-p))
               (funcall callback (if (stringp value) value (or (quarto-get value "content")
                                                              (quarto-get value "contents") (format "%s" value)))))))))
      t)))

(add-hook 'quarto-document-change-hook #'quarto-yaml-changed)
(provide 'quarto-yaml)

;;; quarto-runtime.el --- Serial document-owned execution -*- lexical-binding: t; -*-
(require 'quarto-context)
(require 'awc-r)
(require 'ansi-color)
(require 'button)
(require 'tabulated-list)

(cl-defstruct quarto-request id document chunk revision marker code-marker code language state events method name offset (text-count 0))
(defvar-local quarto-runtime--process nil)
(defvar-local quarto-runtime--context nil)
(defvar-local quarto-runtime--queue nil)
(defvar-local quarto-runtime--current nil)
(defvar-local quarto-runtime--results nil)
(defvar-local quarto-runtime--pane nil)
(defvar-local quarto-runtime--directory nil)
(defvar-local quarto-runtime--serial 0)
(defvar-local quarto-runtime--generation 0)
(defvar-local quarto-runtime--state 'stopped)
(defvar-local quarto-runtime--identity nil)
(defvar-local quarto-runtime--timer nil)
(defvar-local quarto-table--request nil)
(defvar quarto-execution-event-hook nil
  "Functions called with source buffer, request and engine event.")
(defcustom quarto-results-text-limit 24000 "Maximum text characters displayed per request. Full output remains in its log."
  :type 'integer :group 'quarto)

(defun quarto-runtime--document-id ()
  (secure-hash 'sha1 (or buffer-file-name (buffer-name))))

(defun quarto-runtime--title ()
  (if buffer-file-name (file-name-nondirectory buffer-file-name) (buffer-name)))

(defun quarto-runtime--log () (expand-file-name "runtime.log" quarto-runtime--directory))

(defun quarto-runtime--event (source event)
  "Accept an engine EVENT only for the active request of SOURCE."
  (when (buffer-live-p source)
    (with-current-buffer source
      (let ((type (quarto-get event "type")) (request quarto-runtime--current))
        (cond
         ((equal type "ready")
          (setq quarto-runtime--state 'ready quarto-runtime--identity event)
          (quarto-runtime--dispatch))
         ((equal type "fatal") (quarto-runtime--fail (quarto-get event "text")))
         ((and request (equal (quarto-get event "id") (quarto-request-id request))
               (equal (quarto-get event "document") (quarto-request-document request))
               (equal (quarto-get event "revision") (quarto-request-revision request)))
          (pcase type
            ("running" (unless (eq (quarto-request-state request) 'stale) (setf (quarto-request-state request) 'running)))
            ("done"
             (unless (memq (quarto-request-state request) '(stale interrupted))
               (setf (quarto-request-state request) (intern (quarto-get event "status" "failed"))))
             (when (equal (quarto-get event "status") "failed")
               (dolist (queued quarto-runtime--queue) (setf (quarto-request-state queued) 'interrupted))
               (setq quarto-runtime--queue nil))
             (setq quarto-runtime--current nil quarto-runtime--state 'ready)
             (quarto-runtime--dispatch))
            (_
             (when-let* ((text (quarto-get event "text")))
               (let ((used (quarto-request-text-count request)))
                 (puthash "text" (if (< used quarto-results-text-limit)
                                     (truncate-string-to-width (ansi-color-filter-apply text)
                                                               (- quarto-results-text-limit used) nil nil "… [full output in log]")
                                   "") event)
                 (cl-incf (quarto-request-text-count request) (length text))))
             (unless (equal (quarto-get event "text") "") (push event (quarto-request-events request)))
             (when (equal type "table") (quarto-table--show request event))))
          (run-hook-with-args 'quarto-execution-event-hook source request event)))
        (quarto-results--schedule source)))))

(defun quarto-runtime--filter (process output)
  (let ((source (process-get process 'source)))
    (when (and (buffer-live-p source)
               (eq process (buffer-local-value 'quarto-runtime--process source)))
      (with-current-buffer source
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region output nil (quarto-runtime--log) t 'silent))
        (let ((text (concat (process-get process 'partial) output)) (start 0))
          (while (string-match "\n" text start)
            (let ((line (substring text start (match-beginning 0))) (next (match-end 0)))
              (when (string-match "\036QUARTO\\(.*\\)" line)
                (condition-case err
                    (quarto-runtime--event source (json-parse-string (match-string 1 line) :object-type 'hash-table :array-type 'list :null-object nil))
                  (error (message "Quarto runtime event: %s" (error-message-string err)))))
              (setq start next)))
          (process-put process 'partial (substring text start)))))))

(defun quarto-runtime--fail (message)
  (dolist (request (append (when quarto-runtime--current (list quarto-runtime--current)) quarto-runtime--queue))
    (setf (quarto-request-state request) 'failed)
    (push (let ((event (make-hash-table :test #'equal)))
            (puthash "type" "error" event) (puthash "text" message event) event)
          (quarto-request-events request)))
  (setq quarto-runtime--queue nil quarto-runtime--current nil quarto-runtime--state 'failed)
  (message "Quarto: %s" message)
  (quarto-results--schedule (current-buffer)))

(defun quarto-runtime--start (context)
  "Start the selected engine, with normal R profiles read at project root."
  (let* ((source (current-buffer)) (root (plist-get context :root))
         (python (plist-get context :python)) (engine (plist-get context :engine))
         (default-directory root)
         (process-environment (append (awc-project-environment python) process-environment))
         (exec-path (append (split-string (getenv "PATH") path-separator t) (list exec-directory))))
    (unless quarto-runtime--directory (setq quarto-runtime--directory (make-temp-file "quarto-results-" t)))
    (let* ((artefacts (make-temp-file (expand-file-name "session-" quarto-runtime--directory) t))
           (command
           (pcase engine
             ("knitr" (append (list (nth 2 (awc-r--selection)) "--slave" "--no-save" "--no-restore"
                                     "-f" (expand-file-name "runtime.R" quarto-tools-directory)
                                     "--args" artefacts)))
             ("jupyter" (unless python (user-error "Jupyter needs a selected project Python environment"))
              (list python "-u" (expand-file-name "jupyter.py" quarto-tools-directory)
                    artefacts (or (plist-get context :kernel) "python3") (plist-get context :directory)))
             (_ (user-error "Interactive execution is unavailable for engine %s" engine)))))
      (setq quarto-runtime--context context quarto-runtime--state 'starting
            quarto-runtime--process
            (make-process
             :name (format "quarto-runtime-%s" (substring (quarto-runtime--document-id) 0 10))
             :command command :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :stderr (get-buffer-create (format "*Quarto runtime log: %s*" (buffer-name)))
             :filter #'quarto-runtime--filter
             :sentinel (lambda (process event)
                         (when (and (buffer-live-p source)
                                    (eq process (buffer-local-value 'quarto-runtime--process source))
                                    (not (process-live-p process)))
                           (with-current-buffer source
                             (unless (eq quarto-runtime--state 'stopped)
                               (quarto-runtime--fail (format "Runtime %s; see %s and C-c q l"
                                                            (string-trim event) (process-get process 'stderr-name)))))))))
      (process-put quarto-runtime--process 'source source)
      (process-put quarto-runtime--process 'stderr-name (format "*Quarto runtime log: %s*" (buffer-name))))))

(defun quarto-runtime--dispatch ()
  "Send one request, preserving document-local ordering."
  (when (and (eq quarto-runtime--state 'ready) (not quarto-runtime--current) quarto-runtime--queue)
    (setq quarto-runtime--current (pop quarto-runtime--queue) quarto-runtime--state 'running)
    (let ((request quarto-runtime--current))
      (process-send-string
       quarto-runtime--process
       (concat (json-serialize
                (list :method (or (quarto-request-method request) "execute")
                      :id (quarto-request-id request) :document (quarto-request-document request)
                      :chunk (quarto-request-chunk request) :revision (quarto-request-revision request)
                      :language (quarto-request-language request) :code (or (quarto-request-code request) "")
                      :file buffer-file-name :directory (plist-get quarto-runtime--context :directory)
                      :name (or (quarto-request-name request) "") :offset (or (quarto-request-offset request) 0) :limit 100)) "\n")))))

(defun quarto-execution-request (chunks &optional start end)
  "Queue CHUNKS, optionally restricting the first chunk to START and END.
This is the common execution interface; requests snapshot code and revision."
  (with-current-buffer (quarto-source-buffer)
    (let ((source (current-buffer)) (generation quarto-runtime--generation) requests)
      (dolist (chunk chunks)
        (unless (and (quarto-chunk-executable chunk) (quarto-chunk-closed chunk))
          (user-error "This block is not a complete executable R/Python chunk"))
        (when (and start (not (<= (quarto-chunk-head-end chunk) start end (quarto-chunk-body-end chunk))))
          (user-error "Selection must stay within one chunk"))
        (push (make-quarto-request :id (cl-incf quarto-runtime--serial) :document (quarto-runtime--document-id)
                                   :chunk (quarto-chunk-id chunk) :revision (buffer-chars-modified-tick)
                                   :marker (copy-marker (quarto-chunk-start chunk) t)
                                   :code-marker (copy-marker (or start (quarto-chunk-head-end chunk)))
                                   :code (quarto-chunk-code chunk start end) :language (quarto-chunk-language chunk)
                                   :state 'queued) requests))
      (setq requests (nreverse requests))
      (unless requests (user-error "No executable chunks in this range"))
      (setq quarto-runtime--results (append quarto-runtime--results requests))
      (setq quarto-runtime--queue (append quarto-runtime--queue requests))
      (quarto-results-show)
      (unless (eq quarto-runtime--state 'resolving)
      (unless (process-live-p quarto-runtime--process) (setq quarto-runtime--state 'resolving))
      (quarto-context-resolve
       (lambda (context error)
         (when (and (buffer-live-p source) (= generation (buffer-local-value 'quarto-runtime--generation source)))
           (with-current-buffer source
             (condition-case err
                 (progn
                   (when error (error "%s" error))
                   (when (and quarto-runtime--context (process-live-p quarto-runtime--process)
                              (cl-some (lambda (key) (not (equal (plist-get context key) (plist-get quarto-runtime--context key))))
                                       '(:root :python :engine :kernel :directory :profile)))
                     (user-error "Document environment changed; restart its runtime first"))
                   (unless (process-live-p quarto-runtime--process) (quarto-runtime--start context))
                   (quarto-runtime--dispatch))
               (error
                (dolist (request quarto-runtime--queue)
                  (setf (quarto-request-state request) 'failed)
                  (push (let ((event (make-hash-table :test #'equal)))
                          (puthash "type" "error" event) (puthash "text" (error-message-string err) event) event)
                        (quarto-request-events request)))
                (message "Quarto execution: %s" (error-message-string err))
                (setq quarto-runtime--queue nil)
                (unless (process-live-p quarto-runtime--process) (setq quarto-runtime--state 'failed))
                (quarto-results--schedule source)))))))))))

(defun quarto-run-chunk ()
  (interactive)
  (quarto-execution-request (list (or (quarto-chunk-at) (user-error "No chunk here")))))
(defun quarto-run-and-advance () (interactive) (quarto-run-chunk) (ignore-errors (quarto-next-chunk)))
(defun quarto-run-all (&optional preceding)
  (interactive)
  (let ((position (point)))
    (quarto-execution-request
     (cl-remove-if-not (lambda (chunk)
                         (and (quarto-chunk-executable chunk) (quarto-chunk-closed chunk)
                              (not (eq :false (cdr (assoc "eval" (quarto-chunk-options chunk)))))
                              (or (not preceding) (< (quarto-chunk-start chunk) position))))
                       (quarto-document-chunks (quarto-document-model))))))
(defun quarto-run-preceding () (interactive) (quarto-run-all t))
(defun quarto-run-statement (start end)
  "Execute a selection or the current language statement."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (quarto-statement-bounds)))
  (quarto-execution-request (list (or (quarto-chunk-at start) (user-error "No chunk here"))) start end))

(defun quarto-statement-bounds ()
  "Find the language statement without crossing this chunk's boundaries."
  (let* ((chunk (or (quarto-chunk-at) (user-error "No chunk here")))
         (position (point)) (span (pm-innermost-span position)))
    (unless (eq (car span) 'body) (user-error "Place point inside a code statement"))
    (with-current-buffer (pm-span-buffer span)
      (save-excursion (save-restriction
        (widen) (narrow-to-region (quarto-chunk-head-end chunk) (quarto-chunk-body-end chunk))
        (goto-char position)
        (if (equal (quarto-chunk-language chunk) "python")
            (progn (python-nav-beginning-of-statement)
                   (let ((start (point)))
                     (if (python-info-beginning-of-block-p) (python-nav-end-of-block) (python-nav-end-of-statement))
                     (list start (point))))
          (or (ess-continuations-bounds) (user-error "Cannot find a complete R statement"))))))))

(defun quarto-interrupt ()
  "Interrupt this document and cancel its pending requests."
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (cl-incf quarto-runtime--generation)
    (dolist (request quarto-runtime--results)
      (when (eq (quarto-request-state request) 'queued) (setf (quarto-request-state request) 'interrupted)))
    (setq quarto-runtime--queue nil)
    (unless (process-live-p quarto-runtime--process) (setq quarto-runtime--state 'stopped))
    (when quarto-runtime--current (setf (quarto-request-state quarto-runtime--current) 'interrupted))
    (when (process-live-p quarto-runtime--process)
      (if (equal (plist-get quarto-runtime--context :engine) "jupyter")
          (process-send-string quarto-runtime--process "{\"method\":\"interrupt\"}\n")
        (interrupt-process quarto-runtime--process)))
    (quarto-results--schedule (current-buffer))))

(defun quarto-runtime-stop ()
  "Stop only this document's owned processes."
  (with-current-buffer (quarto-source-buffer)
    (cl-incf quarto-runtime--generation)
    (setq quarto-runtime--state 'stopped quarto-runtime--queue nil quarto-runtime--current nil)
    (when (process-live-p quarto-runtime--process)
      (let ((process quarto-runtime--process))
        (if (equal (plist-get quarto-runtime--context :engine) "jupyter")
            (progn (process-send-string process "{\"method\":\"shutdown\"}\n")
                   (run-at-time 4 nil (lambda () (when (process-live-p process) (delete-process process)))))
          (delete-process process))))
    (setq quarto-runtime--process nil quarto-runtime--context nil quarto-runtime--identity nil)))
(defun quarto-restart ()
  "Discard this document's runtime state; the next run starts afresh."
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (quarto-runtime-stop)
    (dolist (request quarto-runtime--results) (setf (quarto-request-state request) 'stale))
    (quarto-results--schedule (current-buffer))))

(defun quarto-runtime-changed (source begin)
  (with-current-buffer source
    (dolist (request quarto-runtime--results)
      (let ((chunk (and quarto-document--model
                        (cl-find (quarto-request-chunk request) (quarto-document-chunks quarto-document--model)
                                 :key #'quarto-chunk-id))))
        (when (or (>= (marker-position (quarto-request-marker request)) begin)
                  (and chunk (<= (marker-position (quarto-request-marker request)) begin (quarto-chunk-body-end chunk))))
          (setf (quarto-request-state request) 'stale))))
    (when quarto-runtime--results (quarto-results--schedule source))))

(define-derived-mode quarto-results-mode special-mode "Quarto results"
  "Chunk-linked interactive results; RET follows a source or artefact link.")

(defun quarto-results-show ()
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (unless (buffer-live-p quarto-runtime--pane)
      (setq quarto-runtime--pane (generate-new-buffer (format "*Quarto results: %s*" (quarto-runtime--title))))
      (let ((source (current-buffer)))
        (with-current-buffer quarto-runtime--pane (quarto-results-mode) (setq-local quarto-document--owner source))))
    (quarto-results--refresh (current-buffer))
    (display-buffer quarto-runtime--pane '((display-buffer-reuse-window display-buffer-in-side-window)
                                          (side . right) (slot . 1) (window-width . 0.42)
                                          (inhibit-same-window . t)))))

(defun quarto-results--schedule (source)
  (with-current-buffer source
    (unless (timerp quarto-runtime--timer)
      (setq quarto-runtime--timer (run-at-time 0.08 nil #'quarto-results--refresh source)))))

(defun quarto-results--refresh (source)
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (timerp quarto-runtime--timer) (cancel-timer quarto-runtime--timer))
      (setq quarto-runtime--timer nil)
      (when (buffer-live-p quarto-runtime--pane)
        (let ((results quarto-runtime--results) (state quarto-runtime--state) (title (quarto-runtime--title)))
          (with-current-buffer quarto-runtime--pane
            (let ((inhibit-read-only t) (position (point)))
              (erase-buffer)
              (insert (format "%s — %s\n\n" title state))
              (dolist (request results)
                (let ((marker (quarto-request-marker request)))
                  (insert-text-button (format "Chunk %s · request %s · %s" (quarto-request-chunk request)
                                              (quarto-request-id request) (quarto-request-state request))
                                      'follow-link t 'action (lambda (_) (when (marker-buffer marker)
                                                                           (pop-to-buffer (marker-buffer marker)) (goto-char marker)))))
                (insert "\n")
                (dolist (event (reverse (quarto-request-events request)))
                  (pcase (quarto-get event "type")
                    ("plot"
                     (let ((path (quarto-get event "path")))
                       (when (file-readable-p path)
                         (if (display-images-p) (insert-image (create-image path nil nil :max-width 600 :max-height 400))
                           (insert-text-button "Open plot" 'action (lambda (_) (find-file-other-window path))))
                         (insert "\n"))))
                    ("table" (insert (format "Table: %s rows; page starting at %s\n" (quarto-get event "total") (quarto-get event "offset"))))
                    (_ (let ((text (quarto-get event "text")))
                         (when (and text (not (string-empty-p text)))
                           (let ((start (point)))
                             (insert (propertize (concat text (unless (string-suffix-p "\n" text) "\n"))
                                                 'face (pcase (quarto-get event "type") ("error" 'error) ("warning" 'warning) (_ 'default))))
                             (when (equal (quarto-get event "type") "error")
                               (quarto-results--error-links start (point) request))))))))
                (insert "\n"))
              (goto-char (min position (point-max))))))))))

(defun quarto-results--error-links (start end request)
  "Link recognised error lines between START and END to REQUEST's source."
  (let ((marker (or (quarto-request-code-marker request) (quarto-request-marker request))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\(?:line \\|<text>:\\|<string>:\\|\\.qmd:\\)\\([0-9]+\\)" end t)
        (let ((line (string-to-number (match-string 1))))
          (make-text-button
           (match-beginning 0) (match-end 0) 'follow-link t
           'action (lambda (_)
                     (when (marker-buffer marker)
                       (pop-to-buffer (marker-buffer marker))
                       (goto-char marker) (forward-line (1- line))))))))))

(defun quarto-results-log ()
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (unless quarto-runtime--directory (user-error "No runtime log yet"))
    (find-file-read-only-other-window (quarto-runtime--log))))

(defun quarto-inspect-table (name &optional language offset)
  "Inspect a bounded page of NAME; never mutate the data frame."
  (interactive (list (read-string "Data frame variable: " (thing-at-point 'symbol t))
                     (or (and (quarto-chunk-at) (quarto-chunk-language (quarto-chunk-at)))
                         (completing-read "Language: " '("r" "python") nil t))))
  (with-current-buffer (quarto-source-buffer)
    (unless (process-live-p quarto-runtime--process) (user-error "Run this document before inspecting its variables"))
    (let ((request (make-quarto-request :id (cl-incf quarto-runtime--serial) :document (quarto-runtime--document-id)
                                        :chunk (or (and quarto-runtime--results (quarto-request-chunk (car (last quarto-runtime--results)))) 0)
                                        :revision (buffer-chars-modified-tick) :marker (copy-marker (point))
                                        :language language :state 'queued :method "table" :name name :offset (or offset 0))))
      (setq quarto-runtime--queue (append quarto-runtime--queue (list request))
            quarto-runtime--results (append quarto-runtime--results (list request)))
      (quarto-runtime--dispatch))))

(define-derived-mode quarto-table-mode tabulated-list-mode "Quarto table"
  "Read-only data frame pages. n/p move between pages; q closes the view.")
(defun quarto-table--show (request event)
  (let* ((source (quarto-source-buffer))
         (buffer (get-buffer-create (format "*Quarto table: %s · %s*" (buffer-name source) (quarto-request-name request))))
         (columns (quarto-get event "columns")) (rows (quarto-get event "rows")))
    (with-current-buffer buffer
      (quarto-table-mode)
      (setq-local quarto-document--owner source quarto-table--request request
                  tabulated-list-format (vconcat (mapcar (lambda (name) (list name 22 t)) columns))
                  tabulated-list-entries (cl-loop for row in rows for index from (1+ (quarto-get event "offset" 0))
                                                  collect (list index (vconcat (mapcar (lambda (x) (format "%s" x)) row))))
                  header-line-format (format " %s — rows %d–%d of %d (n/p pages)"
                                             (quarto-request-name request) (1+ (quarto-get event "offset" 0))
                                             (+ (length rows) (quarto-get event "offset" 0)) (quarto-get event "total" 0)))
      (tabulated-list-init-header) (tabulated-list-print))
    (display-buffer buffer '((display-buffer-reuse-window display-buffer-in-side-window) (side . right) (slot . 1)))))
(defun quarto-table-next-page (&optional backwards)
  (interactive)
  (let ((request quarto-table--request))
    (quarto-inspect-table (quarto-request-name request) (quarto-request-language request)
                          (max 0 (+ (quarto-request-offset request) (if backwards -100 100))))))
(define-key quarto-table-mode-map (kbd "n") #'quarto-table-next-page)
(define-key quarto-table-mode-map (kbd "p") (lambda () (interactive) (quarto-table-next-page t)))

(defun quarto-runtime-close ()
  (when (eq (current-buffer) (quarto-source-buffer))
    (quarto-runtime-stop)
    (when (timerp quarto-runtime--timer) (cancel-timer quarto-runtime--timer))
    (when (process-live-p quarto-context--process) (delete-process quarto-context--process))
    (when (timerp quarto-yaml--timer) (cancel-timer quarto-yaml--timer))
    (when (buffer-live-p quarto-runtime--pane) (kill-buffer quarto-runtime--pane))
    (let ((source (current-buffer)))
      (dolist (buffer (buffer-list))
        (when (and (eq (buffer-local-value 'quarto-document--owner buffer) source)
                   (with-current-buffer buffer (derived-mode-p 'quarto-table-mode)))
          (kill-buffer buffer))))
    (when quarto-runtime--directory
      (let ((directory quarto-runtime--directory))
        ;; Give the Jupyter bridge its shutdown interval before removing files.
        (run-at-time 5 nil (lambda () (when (file-directory-p directory) (delete-directory directory t))))))))

(add-hook 'quarto-document-change-hook #'quarto-runtime-changed)
(provide 'quarto-runtime)

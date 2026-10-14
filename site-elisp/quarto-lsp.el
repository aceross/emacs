;;; quarto-lsp.el --- Mapped R and Python language documents -*- lexical-binding: t; -*-
(require 'quarto-yaml)
(require 'xref)
(eval-when-compile (require 'lsp-mode))
(defvar lsp-pyright-python-executable-cmd)
(defvar lsp-pyright-python-search-functions)
(defvar-local quarto-lsp--documents nil)
(defvar-local quarto-lsp--map nil)
(defvar-local quarto-lsp--revision nil)
(defvar-local quarto-lsp--language nil)
(defvar-local quarto-lsp--environment nil)
(defvar-local quarto-lsp--timer nil)
(defvar-local quarto-lsp-error nil)
(defvar-local quarto-lsp-diagnostics nil)
(defvar quarto-lsp--buffers nil)
(defcustom quarto-lsp-auto-start t "Start code intelligence after document local settings."
  :type 'boolean :group 'quarto)

(defun quarto-lsp--position (position)
  "Encode a projection POSITION using LSP's UTF-16 columns."
  (save-excursion
    (goto-char position)
    (list :line (1- (line-number-at-pos))
          :character (/ (length (encode-coding-string
                                  (buffer-substring-no-properties (line-beginning-position) (point)) 'utf-16le t)) 2))))
(defun quarto-lsp--point (position)
  "Decode an LSP UTF-16 POSITION in the current projection."
  (save-excursion
    (goto-char (point-min)) (forward-line (quarto-get position "line" 0))
    (let ((remaining (quarto-get position "character" 0)))
      (while (and (> remaining 0) (< (point) (line-end-position)))
        (cl-decf remaining (if (> (char-after) #xffff) 2 1)) (forward-char 1)))
    (point)))

(defun quarto-lsp--register (language root python r-program)
  "Use a separate lsp-mode client identity for every resolved environment."
  (require 'lsp-mode)
  (require 'lsp-completion)
  (require 'lsp-lens)
  (require 'lsp-headerline)
  (let* ((id (intern (concat "quarto-" language "-" (substring (secure-hash 'sha1 (prin1-to-string (list root python r-program))) 0 16))))
         (command (if (equal language "r")
                      (list r-program "--slave" "--no-save" "--no-restore" "-e"
                            "if (!requireNamespace('languageserver', quietly=TRUE)) stop('Install languageserver explicitly in this project'); languageserver::run()")
                    (list (or (let ((path (expand-file-name "tools/pyright/node_modules/.bin/pyright-langserver" user-emacs-directory)))
                                (and (file-executable-p path) path))
                              (executable-find "pyright-langserver")
                              (user-error "Install Pyright explicitly before starting Quarto Python assistance")) "--stdio"))))
    (when (equal language "python") (require 'lsp-pyright))
    (unless (gethash id lsp-clients)
      (lsp-register-client
       (make-lsp-client
        :server-id id :major-modes (list (if (equal language "r") 'ess-r-mode 'python-mode)) :multi-root nil
        :initialized-fn (lambda (workspace)
                          (when (equal language "python")
                            (let ((lsp--cur-workspace workspace))
                              (lsp--set-configuration (make-hash-table :test #'equal)))))
        :request-handlers
        (let ((handlers (make-hash-table :test #'equal)))
          (puthash "workspace/applyEdit"
                   (lambda (_workspace _params)
                     (list :applied :json-false :failureReason "Use Quarto's mapped source-edit commands")) handlers)
          (puthash "workspace/configuration"
                   (lambda (workspace params)
                     (with-current-buffer (car (lsp--workspace-buffers workspace))
                       (let ((lsp-pyright-python-executable-cmd python)
                             (lsp-pyright-python-search-functions (list (lambda () python))))
                         (lsp--build-workspace-configuration-response params)))) handlers)
          handlers)
        :new-connection
        (list :test? (lambda () t)
              :connect (lambda (filter sentinel name environment-fn workspace)
                         (let* ((default-directory root)
                                (process-environment (append (awc-project-environment python) process-environment))
                                (exec-path (append (split-string (getenv "PATH") path-separator t) (list exec-directory))))
                           (funcall (plist-get (lsp-stdio-connection command) :connect)
                                    filter sentinel (concat name "-" (symbol-name id)) environment-fn workspace)))))))
    id))

(defun quarto-lsp--create (source language)
  (with-current-buffer source
    (require 'awc-r)
    (let* ((root (awc-project-root)) (python (awc-project-python nil (equal language "r")))
           (r-program (and (equal language "r") (nth 2 (awc-r--selection))))
           (id (quarto-lsp--register language root python r-program))
           (file (concat buffer-file-name ".__quarto__." (if (equal language "r") "R" "py")))
           (projection (quarto-document-projection language))
           (revision (buffer-chars-modified-tick))
           (buffer (generate-new-buffer (format " *Quarto %s: %s*" language (buffer-name)))))
      (with-current-buffer buffer
        ;; A real in-memory projection keeps lsp-mode's bookkeeping away from
        ;; polymode's shared text.  No generated file is written to the project.
        (delay-mode-hooks (if (equal language "r") (ess-r-mode) (python-mode)))
        (setq-local buffer-file-name file default-directory root
                    quarto-document--owner source quarto-lsp--language language
                    quarto-lsp--environment (list root python r-program)
                    quarto-lsp--map (cdr projection) quarto-lsp--revision revision
                    lsp-auto-guess-root t lsp-enable-suggest-server-download nil
                    lsp-enabled-clients (list id) lsp-diagnostics-provider :none
                    lsp-enable-on-type-formatting nil lsp-enable-indentation nil
                    lsp-enable-snippet nil lsp-lens-enable nil lsp-headerline-breadcrumb-enable nil
                    lsp-enable-symbol-highlighting nil lsp-enable-links nil
                    lsp-enable-file-watchers nil lsp-completion--cache nil lsp-completion--last-result nil
                    lsp-pyright-python-executable-cmd python
                    lsp-pyright-python-search-functions (list (lambda () python)))
        (insert (car projection)) (set-buffer-modified-p nil)
        (let ((lsp-client-packages nil)) (lsp))
        ;; Register the same virtual URI mapping used by lsp-mode's Org
        ;; support.  Our commands validate source maps before applying edits.
        (puthash file
                 (list :buffer source :buffer-file-name file :major-mode major-mode
                       :with-current-buffer (lambda (f) (with-current-buffer buffer (funcall f)))
                       :buffer-live? (lambda (_) (buffer-live-p source))
                       :buffer-name (lambda (_) (buffer-name source)))
                 lsp--virtual-buffer-mappings))
      (push buffer quarto-lsp--buffers)
      (push (cons language buffer) quarto-lsp--documents)
      buffer)))

(defun quarto-lsp-start ()
  "Connect every supported language in this document without starting a runtime."
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (when buffer-file-name
      (condition-case err
          (progn
            (dolist (language (delete-dups (mapcar #'quarto-chunk-language (quarto-document-chunks (quarto-document-model)))))
              (when-let* ((buffer (cdr (assoc language quarto-lsp--documents)))
                          (_ (buffer-live-p buffer)))
                (unless (equal (buffer-local-value 'quarto-lsp--environment buffer)
                               (list (awc-project-root) (awc-project-python nil (equal language "r"))
                                     (and (equal language "r") (nth 2 (awc-r--selection)))))
                  (with-current-buffer buffer
                    (remhash buffer-file-name lsp--virtual-buffer-mappings) (set-buffer-modified-p nil))
                  (kill-buffer buffer)
                  (setq quarto-lsp--documents (assoc-delete-all language quarto-lsp--documents))))
              (when (and (member language '("r" "python")) (not (buffer-live-p (cdr (assoc language quarto-lsp--documents)))))
                (quarto-lsp--create (current-buffer) language)))
            (setq quarto-lsp-error nil))
        (error (setq quarto-lsp-error (error-message-string err)) (message "Quarto language assistance: %s" quarto-lsp-error))))))

(defun quarto-lsp--sync (source)
  (when (buffer-live-p source)
    (with-current-buffer source
      (when quarto-lsp-auto-start (quarto-lsp-start))
      (dolist (entry quarto-lsp--documents)
        (when (buffer-live-p (cdr entry))
          (let ((projection (quarto-document-projection (car entry))) (revision (buffer-chars-modified-tick)))
            (with-current-buffer (cdr entry)
              (setq quarto-lsp--map (cdr projection) quarto-lsp--revision revision)
              (unless (equal (buffer-string) (car projection))
                (erase-buffer) (insert (car projection)) (set-buffer-modified-p nil)
                (when (bound-and-true-p lsp-mode) (lsp--flush-delayed-changes))))))))))

(defun quarto-lsp-changed (source _begin)
  (with-current-buffer source
    (setq quarto-lsp-diagnostics nil)
    (when (timerp quarto-lsp--timer) (cancel-timer quarto-lsp--timer))
    (setq quarto-lsp--timer (run-with-idle-timer 0.3 nil #'quarto-lsp--sync source))))

(defun quarto-lsp--at-point ()
  "Return (projection . position) for the current code position."
  (let* ((source (quarto-source-buffer)) (pos (point)) (chunk (quarto-chunk-at pos)))
    (when (and chunk (quarto-chunk-executable chunk) (quarto-chunk-closed chunk)
               (<= (quarto-chunk-head-end chunk) pos) (< pos (quarto-chunk-body-end chunk)))
      (quarto-lsp--sync source)
      (let ((buffer (cdr (assoc (quarto-chunk-language chunk) (buffer-local-value 'quarto-lsp--documents source)))))
        (when (and (buffer-live-p buffer) (buffer-local-value 'lsp--buffer-workspaces buffer))
          (with-current-buffer buffer
            (when-let* ((mapped (quarto-map-position pos quarto-lsp--map))) (cons buffer mapped))))))))

(defun quarto-lsp--params (position)
  (list :textDocument (list :uri (lsp--buffer-uri)) :position (quarto-lsp--position position)))

(defun quarto-lsp--request (method &optional extra)
  (let ((where (or (quarto-lsp--at-point) (user-error "Language server is not ready at this position"))))
    (with-current-buffer (car where)
      (let ((lsp-response-timeout 2))
        (cons (car where) (lsp-request method (append (quarto-lsp--params (cdr where)) extra)))))))

(defun quarto-lsp--safe-edit (buffer edit)
  "Map EDIT from BUFFER, rejecting scaffolding and cross-chunk changes."
  (with-current-buffer buffer
    (let* ((range (or (quarto-get edit "range") (quarto-get edit "replace")))
           (from (quarto-lsp--point (quarto-get range "start")))
           (to (quarto-lsp--point (quarto-get range "end")))
           (start (quarto-map-position from quarto-lsp--map t))
           (end (quarto-map-position to quarto-lsp--map t))
           (a (and start (with-current-buffer quarto-document--owner (quarto-chunk-at start))))
           (b (and end (with-current-buffer quarto-document--owner (quarto-chunk-at end))))
           (text (quarto-get edit "newText" "")))
      (unless (and start end (<= start end) a b (= (quarto-chunk-id a) (quarto-chunk-id b)))
        (user-error "Language-server edit touches prose, a fence or another chunk; no edits applied"))
      (when (string-match-p "\n" text)
        (setq text (replace-regexp-in-string "\n" (concat "\n" (quarto-chunk-prefix a)) text t t)))
      (list start end text))))

(defun quarto-lsp--apply (edits)
  "Apply already validated source EDITS as one undo operation."
  (setq edits (sort edits (lambda (a b) (> (car a) (car b)))))
  (cl-loop for (a b) on edits while b do
           (when (< (car a) (cadr b)) (user-error "Overlapping language-server edits; no changes made")))
  (with-current-buffer (quarto-source-buffer)
    (save-restriction (widen)
      (atomic-change-group
        (dolist (edit edits)
          (goto-char (car edit)) (delete-region (car edit) (cadr edit)) (insert (nth 2 edit)))))))

(defun quarto-lsp-completion-at-point ()
  "Provide Corfu with mapped completion, including safe additional edits."
  (when (and (quarto-chunk-at) (not (quarto-yaml-position-p)))
    (condition-case nil
        (let* ((source (quarto-source-buffer)) (end (point))
               (word-end (save-excursion
                      (skip-chars-forward (if (equal (quarto-chunk-language (quarto-chunk-at)) "python") "[:alnum:]_" "[:alnum:]_."))
                      (point)))
               (start (save-excursion
                        (skip-chars-backward (if (equal (quarto-chunk-language (quarto-chunk-at)) "python")
                                                "[:alnum:]_" "[:alnum:]_."))
                        (point)))
               (reply (quarto-lsp--request "textDocument/completion" '(:context (:triggerKind 1))))
               (buffer (car reply)) (data (cdr reply))
               (items (append (if (hash-table-p data) (quarto-get data "items") data) nil))
               (snapshot (with-current-buffer source (save-restriction (widen) (buffer-string))))
               (original (buffer-substring-no-properties start end)))
          ;; The server's primary edit range is authoritative (e.g. df.column).
          (when-let* ((first (car items)) (edit (quarto-get first "textEdit")))
            (let ((mapped (quarto-lsp--safe-edit buffer edit)))
              (when (<= (car mapped) end (cadr mapped))
                (setq start (car mapped) original (buffer-substring-no-properties start end)))))
          (list start end (mapcar (lambda (item) (quarto-get item "label")) items)
                :exclusive 'no
                :annotation-function (lambda (candidate)
                                       (let ((item (cl-find candidate items :test #'equal :key (lambda (i) (quarto-get i "label")))))
                                         (concat " " (quarto-get item "detail" ""))))
                :exit-function
                (lambda (candidate status)
                  (when (eq status 'finished)
                    (let* ((item (cl-find (substring-no-properties candidate) items :test #'equal :key (lambda (i) (quarto-get i "label"))))
                           (item (if (quarto-get item "data")
                                     (or (with-current-buffer buffer
                                           (let ((lsp-response-timeout 2))
                                             (ignore-errors (lsp-request "completionItem/resolve" item)))) item)
                                   item))
                           (primary (quarto-get item "textEdit"))
                           (edits (append (when primary (list primary)) (append (quarto-get item "additionalTextEdits") nil)))
                           (mapped (mapcar (lambda (edit) (quarto-lsp--safe-edit buffer edit)) edits)))
                      (with-current-buffer source (save-restriction (widen)
                        (unless (equal snapshot (concat (buffer-substring-no-properties (point-min) start) original
                                                        (buffer-substring-no-properties (+ start (length candidate)) (point-max))))
                          (user-error "Document changed during completion; server edits discarded"))
                        (atomic-change-group
                          (delete-region start (+ start (length candidate))) (goto-char start) (insert original)
                          (quarto-lsp--apply (or mapped (list (list start word-end (or (quarto-get item "insertText") candidate)))))))))))))
      (error (setq quarto-lsp-error "Completion failed; check the language server log or reconnect from C-c q L") nil))))

(defun quarto-lsp-eldoc (callback &rest _)
  (when-let* ((where (and (not (quarto-yaml-position-p)) (quarto-lsp--at-point))))
    (let ((source (quarto-source-buffer)) (revision (buffer-chars-modified-tick (quarto-source-buffer))))
      (with-current-buffer (car where)
        (lsp-request-async
         "textDocument/hover" (quarto-lsp--params (cdr where))
         (lambda (response)
           (when (and (buffer-live-p source) (= revision (buffer-chars-modified-tick source)))
             (when-let* ((contents (quarto-get response "contents")))
               (funcall callback (lsp--render-element contents)))))
         :mode 'detached :error-handler #'ignore))
      t)))

(defun quarto-signature-help ()
  (interactive)
  (let* ((reply (quarto-lsp--request "textDocument/signatureHelp"))
         (signatures (quarto-get (cdr reply) "signatures")))
    (message "%s" (mapconcat (lambda (s) (quarto-get s "label")) signatures "\n"))))

(defun quarto-lsp-xrefs (method)
  (let* ((reply (quarto-lsp--request method (when (equal method "textDocument/references") '(:context (:includeDeclaration t)))))
         (buffer (car reply)) (response (cdr reply))
         (locations (if (hash-table-p response) (list response) (append response nil))))
    (mapcar
     (lambda (location)
       (let* ((uri (or (quarto-get location "uri") (quarto-get location "targetUri")))
              (range (or (quarto-get location "range") (quarto-get location "targetSelectionRange")))
              (position (quarto-get range "start")))
         (with-current-buffer buffer
           (xref-make
            (format "%s:%s" (file-name-nondirectory (buffer-local-value 'buffer-file-name quarto-document--owner))
                    (1+ (quarto-get position "line" 0)))
            (if (equal uri (lsp--buffer-uri))
                (xref-make-buffer-location quarto-document--owner
                                           (or (quarto-map-position (quarto-lsp--point position) quarto-lsp--map t)
                                               (user-error "Server location is outside source code")))
              (xref-make-file-location (lsp--uri-to-path uri) (1+ (quarto-get position "line" 0)) (quarto-get position "character" 0))))))) locations)))

(defun quarto-rename (new-name)
  "Rename within this document, rejecting all unsafe or external edits."
  (interactive (list (read-string "New name: " (thing-at-point 'symbol t))))
  (let* ((source (quarto-source-buffer)) (revision (buffer-chars-modified-tick source))
         (reply (quarto-lsp--request "textDocument/rename" (list :newName new-name)))
         (buffer (car reply)) (workspace-edit (cdr reply)) edits)
    (cl-labels ((collect (uri changes)
                 (with-current-buffer buffer
                   (unless (equal uri (lsp--buffer-uri)) (user-error "Rename includes an external file; no edits applied")))
                 (dolist (edit (append changes nil)) (push (quarto-lsp--safe-edit buffer edit) edits))))
      (when-let* ((changes (quarto-get workspace-edit "changes"))) (maphash #'collect changes))
      (dolist (change (append (quarto-get workspace-edit "documentChanges") nil))
        (unless (quarto-get change "textDocument") (user-error "Rename contains a file operation; no edits applied"))
        (collect (quarto-get (quarto-get change "textDocument") "uri") (quarto-get change "edits"))))
    (unless (= revision (buffer-chars-modified-tick source)) (user-error "Document changed; rename discarded"))
    (unless edits (user-error "The server returned no rename edits"))
    (quarto-lsp--apply edits)))

(defun quarto-lsp--diagnostics ()
  "Collect current projection diagnostics, without a second Flycheck checker."
  (dolist (buffer quarto-lsp--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((source quarto-document--owner) (projection buffer) errors)
          (when (and (buffer-live-p source) (= quarto-lsp--revision (buffer-chars-modified-tick source)))
            (dolist (diagnostic (lsp--get-buffer-diagnostics))
              (when-let* ((position (quarto-map-position (quarto-lsp--point (quarto-get (quarto-get diagnostic "range") "start")) quarto-lsp--map t)))
                (push (list position (if (= (quarto-get diagnostic "severity" 1) 1) 'error 'warning)
                            (quarto-get diagnostic "message")) errors)))
            (with-current-buffer source
              (setf (alist-get projection quarto-lsp-diagnostics) errors)
              (when (bound-and-true-p flycheck-mode) (flycheck-buffer)))))))))

(defun quarto-lsp-close ()
  (when (eq (current-buffer) (quarto-source-buffer))
    (when (timerp quarto-lsp--timer) (cancel-timer quarto-lsp--timer))
    (dolist (entry quarto-lsp--documents)
      (let ((buffer (cdr entry)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remhash buffer-file-name lsp--virtual-buffer-mappings)
            (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (setq quarto-lsp--buffers (delq buffer quarto-lsp--buffers))))
    (setq quarto-lsp--documents nil)))

(add-hook 'quarto-document-change-hook #'quarto-lsp-changed)
(defun quarto-lsp--diagnostic-version-guard (original workspace params)
  "Discard versioned or dirty projection diagnostics from obsolete revisions."
  (let* ((uri (quarto-get params "uri")) (version (quarto-get params "version"))
         (buffer (cl-find-if (lambda (candidate)
                              (and (buffer-live-p candidate)
                                   (with-current-buffer candidate (equal uri (lsp--buffer-uri))))) quarto-lsp--buffers)))
    (when (or (not buffer)
              (with-current-buffer buffer
                (and (buffer-live-p quarto-document--owner)
                     (= quarto-lsp--revision (buffer-chars-modified-tick quarto-document--owner))
                     (or (not (numberp version)) (>= version lsp--cur-version)))))
      (funcall original workspace params))))
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--on-diagnostics :around #'quarto-lsp--diagnostic-version-guard))
(with-eval-after-load 'lsp-mode (add-hook 'lsp-diagnostics-updated-hook #'quarto-lsp--diagnostics))
(provide 'quarto-lsp)

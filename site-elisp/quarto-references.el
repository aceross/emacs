;;; quarto-references.el --- Research references and safe label edits -*- lexical-binding: t; -*-
(require 'quarto-context)
(require 'bibtex)
(require 'xref)
(declare-function citar-insert-citation "citar" ())
(defvar citar-bibliography)
(defvar-local quarto-authoring--bibliographies nil)
(defvar-local quarto-authoring--citations nil)
(defvar-local quarto-references--entries nil)
(defvar-local quarto-references-error nil)
(defvar-local quarto-references--metadata nil)
(defvar-local quarto-references--config-files nil)
(defvar-local quarto-references--fingerprint nil)
(defvar-local quarto-references--timer nil)
(defvar-local quarto-references--generation 0)
(defvar quarto-references--sources nil)
(defvar quarto-references--poll-timer nil)

(defun quarto-references--plain (text)
  "Make a compact description from bibliographic or caption TEXT."
  (replace-regexp-in-string
   "[ \t\n]+" " " (replace-regexp-in-string "[{}]" "" (format "%s" (or text "")))))

(defun quarto-references--files-state (files)
  (mapcar (lambda (file)
            (let ((attrs (file-attributes file)))
              (list file (file-attribute-modification-time attrs)
                    (file-attribute-size attrs) (file-attribute-inode-number attrs)))) files))

(defun quarto-references--read ()
  "Refresh BibTeX records, keeping the last good records for broken files."
  (let (entries errors)
    (dolist (file quarto-authoring--bibliographies)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (delay-mode-hooks (bibtex-mode))
            (let (records)
              (bibtex-map-entries
               (lambda (key begin _end)
                 (when (and (stringp key) (not (string-empty-p key)))
                   (let* ((fields (bibtex-parse-entry t))
                          (field (lambda (name) (cdr (assoc-string name fields t)))))
                     (push (cons key (list :author (quarto-references--plain (funcall field "author"))
                                          :year (quarto-references--plain (or (funcall field "year") (funcall field "date")))
                                          :title (quarto-references--plain (funcall field "title"))
                                          :file file :position begin
                                          :line (line-number-at-pos begin))) records)))))
              (setq entries (append entries (nreverse records)))))
        (error
         (push (format "%s: %s" (file-name-nondirectory file) (error-message-string err)) errors)
         (setq entries (append entries (cl-remove-if-not
                                        (lambda (entry) (equal file (plist-get (cdr entry) :file)))
                                        quarto-references--entries))))))
    (setq quarto-references--entries entries
          quarto-authoring--citations (delete-dups (mapcar #'car entries))
          quarto-references-error (when errors (string-join (nreverse errors) "; "))
          quarto-references--fingerprint
          (quarto-references--files-state (append quarto-references--config-files quarto-authoring--bibliographies)))))

(defun quarto-references--configuration-files (directory root)
  "Track metadata and project files between DIRECTORY and ROOT."
  (let ((dir (file-name-as-directory (expand-file-name directory))) files done)
    (while (not done)
      (setq files (append files (mapcar (lambda (name) (expand-file-name name dir))
                                        '("_quarto.yml" "_quarto.yaml" "_metadata.yml" "_metadata.yaml"))
                          (directory-files dir t "\\`_quarto-.+\\.ya?ml\\'")))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq done (or (equal (directory-file-name dir) (directory-file-name root)) (equal parent dir))
              dir parent)))
    files))

(defun quarto-authoring-bibliographies (context)
  "Use CONTEXT's bibliographies without changing personal Citar settings."
  (let* ((values (quarto-get (plist-get context :metadata) "bibliography"))
         (dir (file-name-directory (plist-get context :file)))
         (root (or (plist-get context :project-directory) dir)))
    (setq quarto-authoring--bibliographies
          (mapcar (lambda (file)
                    (let ((path (expand-file-name file dir)))
                      (if (file-readable-p path) path (expand-file-name file root))))
                  (if (stringp values) (list values) values))
          quarto-references--config-files (quarto-references--configuration-files dir root))
    (setq-local citar-bibliography quarto-authoring--bibliographies)
    (quarto-references--read)))

(defun quarto-references--metadata-key ()
  (list (quarto-document-metadata) quarto-format quarto-profile))

(defun quarto-references--resolve-values (source generation key context)
  "Combine current document bibliography settings with inherited metadata."
  (let* ((dir (file-name-directory (plist-get context :file)))
         (root (or (plist-get context :project-directory) dir))
         (configs (quarto-references--configuration-files dir root))
         (directory-metadata
          (mapcar (lambda (file) (list :directory (file-name-directory file)
                                      :code (with-temp-buffer (insert-file-contents file) (buffer-string))))
                  (reverse (cl-remove-if-not
                            (lambda (file) (and (string-match-p "/_metadata\\.ya?ml\\'" file) (file-readable-p file))) configs)))))
    (quarto-yaml-request
     "bibliography"
     (list :code (car key) :format (or (cadr key) (plist-get context :format) "html")
           :project_directory root :document_directory dir
           :project (or (quarto-get (quarto-get (plist-get context :inspection) "project") "config") (make-hash-table))
           :directory_metadata (vconcat directory-metadata))
     (lambda (metadata error)
       (when (buffer-live-p source)
         (with-current-buffer source
           (when (and (= generation quarto-references--generation) (equal key (quarto-references--metadata-key)))
             (if error (setq quarto-references-error error)
               (condition-case err
                   (progn
                     (setq quarto-references--metadata key)
                     (quarto-authoring-bibliographies (plist-put (copy-sequence context) :metadata metadata)))
                 (error (setq quarto-references-error (error-message-string err))))))))))))

(defun quarto-authoring-refresh-bibliographies ()
  "Refresh document/project bibliography information asynchronously."
  (interactive)
  (with-current-buffer (quarto-source-buffer)
    (let* ((source (current-buffer)) (generation (cl-incf quarto-references--generation))
           (key (quarto-references--metadata-key))
           (callback
            (lambda (context error)
              (when (buffer-live-p source)
                (with-current-buffer source
                  (when (= generation quarto-references--generation)
                    (cond
                     ((not (equal key (quarto-references--metadata-key)))
                      (quarto-references--schedule source))
                     ((and error (string-prefix-p "Document changed while" error))
                      (quarto-references--schedule source))
                     (error (setq quarto-references-error error))
                     (t (quarto-references--resolve-values source generation key context)))))))))
      (condition-case err
          (if (and buffer-file-name (file-readable-p buffer-file-name))
              (quarto-context-resolve callback t)
            ;; Unsaved files can still use a bibliography declared in their
            ;; own front matter. Full project inheritance starts on first save.
            (let ((file (or buffer-file-name (expand-file-name "document.qmd" default-directory))))
              (quarto-yaml-request
               "parse" (list :code (car key))
               (lambda (metadata error)
                 (funcall callback (list :metadata metadata :file file
                                         :project-directory (file-name-directory file)) error)))))
        (error (funcall callback nil (error-message-string err)))))))

(defun quarto-references--schedule (source)
  (when (buffer-live-p source)
    (with-current-buffer source
      (cl-incf quarto-references--generation)
      (when (timerp quarto-references--timer) (cancel-timer quarto-references--timer))
      (setq quarto-references--timer
            (run-with-idle-timer 0.5 nil
                                (lambda () (when (buffer-live-p source)
                                             (with-current-buffer source (quarto-authoring-refresh-bibliographies)))))))))

(defun quarto-references-changed (source _begin)
  (when (memq source quarto-references--sources)
    (with-current-buffer source
      (unless (equal quarto-references--metadata (quarto-references--metadata-key))
        (quarto-references--schedule source)))))

(defun quarto-references--poll ()
  "Notice saved external edits, deletion and atomic replacement of .bib files."
  (dolist (source quarto-references--sources)
    (when (buffer-live-p source)
      (with-current-buffer source
        (condition-case err
            (let* ((state (quarto-references--files-state
                           (append quarto-references--config-files quarto-authoring--bibliographies)))
                   (count (length quarto-references--config-files)))
              (unless (equal state quarto-references--fingerprint)
                (if (equal (seq-take state count) (seq-take quarto-references--fingerprint count))
                    (quarto-references--read)
                  (setq quarto-references--fingerprint state)
                  (quarto-references--schedule source))))
          (error (setq quarto-references-error (error-message-string err))))))))

(defun quarto-references-close ()
  (setq quarto-references--sources (delq (current-buffer) quarto-references--sources))
  (when (timerp quarto-references--timer) (cancel-timer quarto-references--timer))
  (unless quarto-references--sources
    (when (timerp quarto-references--poll-timer) (cancel-timer quarto-references--poll-timer))
    (setq quarto-references--poll-timer nil)))

(defun quarto-references-setup ()
  (unless (buffer-base-buffer)
    (cl-pushnew (current-buffer) quarto-references--sources)
    (add-hook 'kill-buffer-hook #'quarto-references-close nil t)
    (unless (timerp quarto-references--poll-timer)
      (setq quarto-references--poll-timer (run-with-timer 2 2 #'quarto-references--poll)))))

(defun quarto-insert-citation ()
  (interactive)
  (require 'citar)
  (let ((citar-bibliography (buffer-local-value 'quarto-authoring--bibliographies (quarto-source-buffer))))
    (unless citar-bibliography (user-error "No document bibliography; refresh it from C-c q B"))
    (citar-insert-citation)))

(defun quarto-reference-records ()
  "Return completion descriptions for citations and indexed document labels."
  (with-current-buffer (quarto-source-buffer)
    (append
     (mapcar (lambda (entry)
               (cons (car entry) (string-join (delq nil (mapcar (lambda (key)
                                                               (let ((value (plist-get (cdr entry) key)))
                                                                 (unless (string-empty-p value) value)))
                                                             '(:author :year :title))) " · ")))
             quarto-references--entries)
     (mapcar (lambda (target)
               (let ((description (quarto-references--plain (plist-get target :description))))
                 (when (string-match "!\\[\\(.*?\\)\\](" description)
                   (setq description (match-string 1 description)))
                 (setq description (replace-regexp-in-string "\\`[ >#:\t]+" "" description))
                 (cons (plist-get target :key) description)))
             (quarto-document-targets (quarto-document-model))))))

(defun quarto-reference-completion-at-point ()
  (when (quarto-document-reference-at)
    (let ((end (point)) (start (save-excursion (skip-chars-backward "[:alnum:]_:.+-") (point))))
      (when (and (eq (char-before start) ?@)
                 (or (= start (1+ (point-min)))
                     (not (string-match-p "[[:alnum:]\\\\]" (string (char-before (1- start))))))
                 ;; The shared parser excludes comments, inline code and email.
                 (cl-find start (quarto-document-references (quarto-document-model)) :key #'cdr))
        (let ((records (quarto-reference-records)))
          (list start end (delete-dups (mapcar #'car records)) :exclusive 'no
                :annotation-function (lambda (key) (concat " " (truncate-string-to-width (or (cdr (assoc key records)) "") 90 nil nil "…")))
                :company-doc-buffer
                (lambda (key)
                  (with-current-buffer (get-buffer-create " *Quarto reference*")
                    (let ((inhibit-read-only t))
                      (erase-buffer) (insert key "\n\n" (or (cdr (assoc key records)) "")) (special-mode))
                    (current-buffer)))))))))

(defun quarto-reference-at-point ()
  "Return an indexed label/reference occurrence at point, never a code symbol."
  (let ((pos (point)) (model (quarto-document-model)))
    (or (quarto-document-reference-at pos)
        (when-let* ((target (cl-find-if (lambda (target)
                                       (<= (plist-get target :start) pos (plist-get target :end)))
                                     (quarto-document-targets model))))
          (cons (plist-get target :key) (plist-get target :start))))))

(defun quarto-rename-label (new-name &optional old-name)
  "Rename one explicit label and its indexed references in this document.
Reject ambiguous labels, citation collisions, changed type prefixes and stale
source ranges before making one undoable edit. Other files are never edited."
  (interactive (list (read-string "New label: " (car (quarto-reference-at-point)))))
  (let ((old (or old-name (car (quarto-reference-at-point)) (user-error "No label or reference here"))))
    (with-current-buffer (quarto-source-buffer)
      (save-restriction
        (widen)
        (let* ((model (quarto-document-model))
               (targets (cl-remove-if-not (lambda (target) (equal old (plist-get target :key)))
                                         (quarto-document-targets model)))
               (prefix (and (string-match "\\`\\([[:alpha:]]+-\\)" old) (match-string 1 old)))
               (refs (cl-remove-if-not (lambda (ref) (equal old (car ref))) (quarto-document-references model)))
               (positions (append (mapcar (lambda (target) (plist-get target :start)) targets) (mapcar #'cdr refs))))
          (unless (= 1 (length targets)) (user-error "Label is missing or ambiguous; no edits applied"))
          (unless (and (string-match-p "\\`[[:alpha:]]\\(?:[[:alnum:]_:.+-]*[[:alnum:]_]\\)?\\'" new-name)
                       (or (not prefix) (string-prefix-p prefix new-name)))
            (user-error "Use a valid label and preserve its type prefix (%s)" (or prefix "none")))
          (when (or (member old quarto-authoring--citations) (member new-name quarto-authoring--citations))
            (user-error "Label collides with a citation key; no edits applied"))
          (when (and (not (equal old new-name)) (assoc new-name (quarto-document-labels model)))
            (user-error "Label %s already exists; no edits applied" new-name))
          (dolist (position positions)
            (unless (equal old (buffer-substring-no-properties position (+ position (length old))))
              (user-error "Label source changed; no edits applied")))
          (atomic-change-group
            (save-excursion
              (dolist (position (sort positions #'>))
                (goto-char position) (delete-region position (+ position (length old))) (insert new-name))))
          (message "Renamed %s to %s (%d references in this document)" old new-name (length refs)))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql quarto)))
  (or (car (quarto-reference-at-point)) (and (quarto-chunk-at) (thing-at-point 'symbol t))))
(cl-defmethod xref-backend-definitions ((_backend (eql quarto)) identifier)
  (let ((source (quarto-source-buffer)) (pos (point)))
    (with-current-buffer source
      (save-excursion
       (goto-char pos)
       (cond
       ((assoc identifier (quarto-document-labels (quarto-document-model)))
        (mapcar (lambda (target) (xref-make (or (cdr (assoc identifier (quarto-reference-records))) identifier)
                                          (xref-make-buffer-location source (plist-get target :start))))
                (cl-remove-if-not (lambda (target) (equal identifier (plist-get target :key)))
                                  (quarto-document-targets (quarto-document-model)))))
       ((assoc identifier quarto-references--entries)
        (let ((entry (cdr (assoc identifier quarto-references--entries))))
          (list (xref-make (plist-get entry :title) (xref-make-file-location (plist-get entry :file) (plist-get entry :line) 0)))))
       ((quarto-chunk-at) (quarto-lsp-xrefs "textDocument/definition")))))))
(cl-defmethod xref-backend-references ((_backend (eql quarto)) identifier)
  (if (and (quarto-chunk-at) (not (quarto-reference-at-point)))
      (quarto-lsp-xrefs "textDocument/references")
    (mapcar (lambda (ref) (xref-make identifier (xref-make-buffer-location (quarto-source-buffer) (cdr ref))))
            (cl-remove-if-not (lambda (ref) (equal identifier (car ref)))
                              (quarto-document-references (quarto-document-model))))))

(add-hook 'quarto-document-change-hook #'quarto-references-changed)
(provide 'quarto-references)

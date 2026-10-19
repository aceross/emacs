;;; awc-bibliography.el --- Reviewed project reference snapshots -*- lexical-binding: t; -*-

;; Zotero owns library metadata; a manuscript owns its reviewed build snapshot.
(require 'awc-research)
(require 'awc-project)
(require 'diff-mode)
(declare-function citar-select-ref "citar" (&rest args))
(declare-function quarto-source-buffer "quarto-document" ())
(declare-function quarto-authoring-refresh-bibliographies "quarto-references" ())
(declare-function quarto-references--configuration-files "quarto-references" (directory root))
(declare-function quarto-references--read "quarto-references" ())
(defvar quarto-references--metadata)
(defvar quarto-references-error)
(defvar quarto-authoring--bibliographies)
(defvar quarto-context--value)

(defconst awc-bibliography--marker "% awc-bibliography: reviewed snapshot\n")
(defconst awc-bibliography--local-fields '("file" "pdf" "local-url"))
(defvar-local awc-bibliography--plan nil)

(defun awc-bibliography--snapshot (file)
  "Read FILE without running its local variables, retaining an exact fingerprint."
  (when (file-remote-p file) (user-error "Use local bibliography files"))
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (or (buffer-modified-p buffer) (not (verify-visited-file-modtime buffer)))
      (user-error "Save or revert %s before preparing/applying references" file)))
  (list :file file :true (file-truename file)
        :attributes (let ((attrs (file-attributes file)))
                      (list (nth 5 attrs) (nth 7 attrs) (nth 10 attrs) (nth 11 attrs)))
        :digest (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents-literally file)
                    (secure-hash 'sha256 (current-buffer))))
        :text (when (file-exists-p file)
                (with-temp-buffer (insert-file-contents file) (buffer-string)))
        :coding (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    buffer-file-coding-system))))

(defun awc-bibliography--check (snapshots)
  (dolist (old snapshots)
    (unless (equal old (awc-bibliography--snapshot (plist-get old :file)))
      (user-error "Changed since preparation: %s; prepare a new review" (plist-get old :file)))))

(defun awc-bibliography--records (text file)
  "Read BibTeX TEXT strictly; retain raw fields and source ranges in FILE.
Comments, preambles and string declarations remain untouched in the target.
BibLaTeX types, including xdata, use Emacs's built-in BibTeX parser."
  (with-temp-buffer
    (insert text "\n")
    (delay-mode-hooks (bibtex-mode))
    (bibtex-set-dialect 'biblatex t)
    ;; Emacs's dialect table omits BibLaTeX's XData type. Keep its field
    ;; parser, but accept named entry types (also used by custom styles).
    (setq-local bibtex-entry-head
                (concat "\\(@[ \t]*[[:alpha:]]+\\)[ \t]*[({][ \t\n]*\\("
                        bibtex-reference-key "\\)"))
    (setq-local bibtex-entry-maybe-empty-head (concat bibtex-entry-head "?"))
    (goto-char (point-min))
    (let ((case-fold-search t) records)
      (while (re-search-forward "@\\([[:alpha:]]+\\)[ \t]*[{(]" nil t)
        (let ((type (downcase (match-string-no-properties 1)))
              (begin (match-beginning 0)))
          (if (member type '("comment" "preamble" "string"))
              (condition-case nil
                  (goto-char (scan-sexps (1- (point)) 1))
                (error (user-error "Malformed @%s in %s" type file)))
            (progn
              (goto-char begin)
              (unless (bibtex-valid-entry)
		(user-error "Malformed BibTeX near line %d in %s" (line-number-at-pos) file))
              (let* ((bounds (bibtex-valid-entry))
                     (fields (save-excursion (bibtex-parse-entry nil t)))
                     (values (save-excursion (bibtex-parse-entry t t)))
                     (key (cdr (assoc "=key=" fields))))
		(unless (and key (not (string-empty-p key)))
                  (user-error "Missing citation key in %s" file))
		(let ((names (mapcar (lambda (field) (downcase (car field))) (cddr fields))))
                  (unless (= (length names) (length (delete-dups (copy-sequence names))))
                    (user-error "Duplicate fields in %s (%s)" key file)))
		(push (list :key key :type (cdr (assoc "=type=" fields))
                            :fields (cddr fields) :values values :file file
                            :begin (car bounds) :end (cdr bounds)) records)
		(goto-char (cdr bounds)))))))
      (nreverse records))))

(defun awc-bibliography--catalog (snapshots &optional unique)
  (let ((catalog (make-hash-table :test 'equal)))
    (dolist (snapshot snapshots)
      (dolist (record (awc-bibliography--records
                       (or (plist-get snapshot :text) "") (plist-get snapshot :file)))
        (let ((key (plist-get record :key)))
          (when (and unique (gethash key catalog))
            (user-error "Duplicate citation key %s in declared project bibliographies" key))
          (puthash key (cons record (gethash key catalog)) catalog))))
    catalog))

(defun awc-bibliography--one (key catalog)
  (let ((records (gethash key catalog)))
    (unless (= (length records) 1)
      (user-error "Reference %s has %d source records; reconcile it first" key (length records)))
    (car records)))

(defun awc-bibliography--dependencies (record)
  (let ((values (plist-get record :values)))
    (append (when-let* ((key (cdr (assoc-string "crossref" values t)))) (list (string-trim key)))
            (when-let* ((keys (cdr (assoc-string "xdata" values t))))
              (mapcar #'string-trim (split-string keys "," t))))))

(defun awc-bibliography--fields (record &optional previous)
  "Merge RECORD's fields over PREVIOUS, preserving manual-only fields.
Local attachment fields are omitted. New source macros must be expanded to
literals first, so no unresolved @string dependency enters a snapshot."
  (let ((fields (copy-tree (plist-get previous :fields))))
    (dolist (field (plist-get record :fields))
      (let ((name (downcase (car field))) (value (cdr field)))
        (unless (member name awc-bibliography--local-fields)
          (unless (or (with-temp-buffer
                        (insert value)
                        (delay-mode-hooks (bibtex-mode))
                        (goto-char (point-min))
                        (when-let* ((bounds (bibtex-parse-field-string)))
                          (= (cdr bounds) (point-max))))
                      (string-match-p "\\`[0-9]+\\'" value)
                      (and (equal name "month")
                           (member (downcase value) '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))))
            (user-error "Expand BibTeX string macro in %s/%s to a literal first"
                        (plist-get record :key) name))
          (if-let* ((old (assoc-string name fields t)))
              (setcdr old value)
            (setq fields (append fields (list (cons name value))))))))
    (cl-remove-if (lambda (field) (member (downcase (car field)) awc-bibliography--local-fields)) fields)))

(defun awc-bibliography--render (key record fields)
  (concat "@" (plist-get record :type) "{" key ",\n"
          (mapconcat (lambda (field) (format "  %s = %s" (car field) (cdr field))) fields ",\n")
          "\n}"))

(defun awc-bibliography--zotero (key)
  "Read the explicitly associated Zotero item; never update the local library."
  (let* ((links (awc-research--read-links))
         (link (gethash key links))
         (library (and link (awc-research--library (gethash "library" link))))
         (item-key (and link (gethash "item" link))))
    (unless (and item-key (string-match-p "\\`[A-Z0-9]\\{8\\}\\'" item-key))
      (user-error "Associate %s with Zotero using C-c w n z before refreshing" key))
    (maphash (lambda (other candidate)
               (when (and (not (equal other key)) (equal library (gethash "library" candidate))
                          (equal item-key (gethash "item" candidate)))
                 (user-error "This Zotero item has multiple local keys; reconcile them before refreshing"))) links)
    (let* ((item (awc-research--get (format "%s/items/%s" library item-key)))
           (text (awc-research--get (format "%s/items?itemKey=%s&format=bibtex" library item-key) t))
           (records (awc-bibliography--records text "Zotero export"))
           (saved-key (alist-get 'citationKey (alist-get 'data item))))
      (unless (and (equal (alist-get 'key item) item-key) (= 1 (length records))
                   (or (not saved-key) (equal saved-key (plist-get (car records) :key))))
        (user-error "Zotero identity/export mismatch; resolve it before refreshing"))
      (plist-put (car records) :identity (awc-research--zotero-uri link)))))

(defun awc-bibliography--target (target files root)
  (unless (and (member target files) (string-equal (downcase (or (file-name-extension target) "")) "bib")
               (not (file-symlink-p target)) (file-in-directory-p target root)
               (file-regular-p target) (file-writable-p target))
    (user-error "Choose an existing writable .bib declared inside this project; automatic exports need a separate snapshot"))
  (let ((text (plist-get (awc-bibliography--snapshot target) :text)) (case-fold-search t))
    (when (string-match-p "^[ \t%]*\\(?:do not edit\\|automatically generated\\|auto[- ]export\\)" text)
      (user-error "Generated bibliography: use a separate reviewed snapshot"))))

(defun awc-bibliography--build (key target files root &optional zotero inputs)
  "Prepare one review without changing any file.
FILES are the document's resolved bibliographies; INPUTS tracks metadata too."
  (require 'citar)
  (setq files (delete-dups (mapcar #'expand-file-name files)) target (expand-file-name target))
  (awc-bibliography--target target files root)
  (unless (cl-every (lambda (file) (equal (downcase (or (file-name-extension file) "")) "bib")) files)
    (user-error "This command checks BibTeX .bib files; mixed CSL JSON/YAML sources are not supported yet"))
  (dolist (file files)
    (unless (file-readable-p file) (user-error "Missing declared bibliography: %s" file)))
  (let* ((sources (delete-dups (mapcar #'expand-file-name (default-value 'citar-bibliography))))
         (snapshots (mapcar #'awc-bibliography--snapshot
                            (delete-dups (append files sources inputs
                                                 (when zotero (list (awc-research--links-file)))))))
         (project (awc-bibliography--catalog (cl-remove-if-not (lambda (s) (member (plist-get s :file) files)) snapshots) t))
         (catalog (awc-bibliography--catalog (cl-remove-if-not (lambda (s) (member (plist-get s :file) sources)) snapshots)))
         (record (if zotero (awc-bibliography--zotero key) (awc-bibliography--one key catalog)))
         (previous (car (gethash key project)))
         (original (cl-find target snapshots :key (lambda (s) (plist-get s :file)) :test #'equal))
         (text (plist-get original :text))
         (seen (make-hash-table :test 'equal)) additions)
    (when (and previous (not (equal target (plist-get previous :file))))
      (user-error "%s already belongs to %s; choose that target" key (plist-get previous :file)))
    (let ((baseline (if zotero (awc-bibliography--one key catalog) record)))
      (when (and previous (not (awc-research--same-reference-p (plist-get previous :values) (plist-get baseline :values))))
        (user-error "Project key %s has conflicting metadata/identity; reconcile it first" key))
      (maphash (lambda (other records)
                 (when (and (not (equal key other))
                            (awc-research--same-reference-p (plist-get baseline :values) (plist-get (car records) :values)))
                   (user-error "This project already cites the source as %s; preserve that key" other))) project))
    (cl-labels
     ((collect (current chosen-key old stack)
        (when (member chosen-key stack) (user-error "Cyclic reference dependency: %s" chosen-key))
        (unless (gethash chosen-key seen)
          (puthash chosen-key t seen)
          (let* ((fields (awc-bibliography--fields current old))
                 (rendered (awc-bibliography--render chosen-key current fields))
                 (merged (car (awc-bibliography--records rendered "candidate"))))
            (dolist (dependency (awc-bibliography--dependencies merged))
              (if-let* ((existing (car (gethash dependency project))))
                  (collect existing dependency existing (cons chosen-key stack))
                (collect (awc-bibliography--one dependency catalog) dependency nil (cons chosen-key stack))))
            (when (or (equal chosen-key key) (not (gethash chosen-key project)))
              (push (cons chosen-key rendered) additions))))))
     (collect record key previous nil))
    (let ((replacement (cdr (assoc key additions))))
      (when previous
        (setq text (concat (substring text 0 (1- (plist-get previous :begin))) replacement
                           (substring text (1- (plist-get previous :end)))))
        (setq additions (assoc-delete-all key additions)))
      (dolist (addition (nreverse additions))
        (setq text (concat text (unless (string-suffix-p "\n" text) "\n") "\n" (cdr addition) "\n"))))
    (unless (string-prefix-p awc-bibliography--marker text)
      (setq text (concat awc-bibliography--marker text)))
    (awc-bibliography--check snapshots)
    (list :key key :target target :root root :files files :snapshots snapshots
          :old (plist-get original :text) :new text :coding (plist-get original :coding)
          :source (if zotero (format "%s (Zotero; citation key preserved)" (plist-get record :identity))
                    "local Citar bibliography snapshot"))))

(defun awc-bibliography--document ()
  "Resolve the saved Quarto source using the existing bibliography model."
  (require 'quarto-mode)
  (with-current-buffer (quarto-source-buffer)
    (unless (and buffer-file-name (string-suffix-p ".qmd" buffer-file-name t)
                 (not (buffer-modified-p)))
      (user-error "Run this command from a saved Quarto document"))
    (let* ((inputs (cons buffer-file-name
                         (quarto-references--configuration-files (file-name-directory buffer-file-name) (awc-project-root))))
           (before (mapcar #'awc-bibliography--snapshot inputs))
           (deadline (+ (float-time) 20)))
      (setq quarto-references--metadata nil quarto-references-error nil)
      (quarto-authoring-refresh-bibliographies)
      (while (and (not quarto-references--metadata) (not quarto-references-error)
                  (< (float-time) deadline))
        (accept-process-output nil 0.05))
      (awc-bibliography--check before)
      (when quarto-references-error (user-error "%s" quarto-references-error))
      (unless quarto-references--metadata (user-error "Bibliography resolution timed out; retry C-c q B"))
      (unless quarto-authoring--bibliographies
	(user-error "Declare a project bibliography first, then refresh with C-c q B"))
      (list :buffer (current-buffer) :file buffer-file-name
            :files quarto-authoring--bibliographies
            :root (or (plist-get quarto-context--value :project-directory) (awc-project-root))
            :inputs inputs :snapshots before))))

(defvar awc-bibliography-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "C-c C-c") #'awc-bibliography-apply)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode awc-bibliography-review-mode diff-mode "Reference review"
  "Review a fixed candidate; C-c C-c applies it, q leaves it unapplied."
  (setq buffer-read-only t))

(defun awc-bibliography--review (plan)
  (if (equal (plist-get plan :old) (plist-get plan :new))
      (progn (message "Project reference %s is already current" (plist-get plan :key)) nil)
    (let ((old (make-temp-file "awc-bib-before-"))
          (new (make-temp-file "awc-bib-after-"))
          (buffer (generate-new-buffer "*Project bibliography review*")) success)
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (plist-get plan :old) nil old nil 'silent)
              (write-region (plist-get plan :new) nil new nil 'silent))
            (let ((status (call-process "diff" nil buffer nil "-u" "--label" "before" "--label" "reviewed snapshot" old new)))
              (unless (memq status '(0 1)) (user-error "Cannot generate bibliography review diff")))
            (with-current-buffer buffer
              (goto-char (point-min))
              (insert (format "Reference: %s\nTarget: %s\nSource: %s\n\nC-c C-c: apply this diff.  q: leave unapplied.\nThe target is a manually reviewed snapshot; do not apply to an automatic export.\nExisting source-field changes need review; manual-only fields are retained.\n\n"
                              (plist-get plan :key) (plist-get plan :target) (plist-get plan :source)))
              (awc-bibliography-review-mode)
              (setq-local awc-bibliography--plan plan)
              (set-buffer-modified-p nil)
              (goto-char (point-min)))
            (pop-to-buffer buffer)
            (setq success t)
            buffer)
        (delete-file old) (delete-file new)
        (unless success (kill-buffer buffer))))))

;;;###autoload
(defun awc-bibliography-prepare (key &optional target zotero)
  "Review adding KEY to this Quarto project's declared TARGET bibliography.
With ZOTERO, refresh from the explicitly associated item, preserving KEY.
Otherwise use the local Citar snapshot. Nothing is written until apply."
  (interactive (list (awc-research--global-key)))
  (let* ((context (awc-bibliography--document))
         (files (plist-get context :files))
         (target (or target (if (= 1 (length files)) (car files)
                              (completing-read "Project bibliography: " files nil t))))
         (plan (awc-bibliography--build key target files (plist-get context :root) zotero (plist-get context :inputs))))
    (awc-bibliography--check (plist-get context :snapshots))
    (setq plan (plist-put plan :document (plist-get context :buffer)))
    (awc-bibliography--review plan)))

;;;###autoload
(defun awc-bibliography-refresh (key &optional target)
  "Review refreshing KEY from Zotero in the current project; preserve its key."
  (interactive (progn
                 (awc-bibliography--document)
                 (require 'citar)
                 (with-current-buffer (quarto-source-buffer) (list (citar-select-ref)))))
  (awc-bibliography-prepare key target t))

(defun awc-bibliography-apply ()
  "Apply exactly the reviewed candidate after checking every captured input."
  (interactive)
  (unless awc-bibliography--plan (user-error "No unapplied bibliography review here"))
  (let* ((plan awc-bibliography--plan) (target (plist-get plan :target))
         (snapshots (plist-get plan :snapshots)) temporary backup)
    (awc-bibliography--target target (plist-get plan :files) (plist-get plan :root))
    (awc-bibliography--check snapshots)
    (when (file-locked-p target) (user-error "Bibliography is locked; finish the other edit first"))
    (unwind-protect
        (progn
          (lock-file target)
          (setq temporary (make-temp-file (concat target ".awc-")))
          (let ((coding-system-for-write (or (plist-get plan :coding) 'utf-8-unix)))
            (write-region (plist-get plan :new) nil temporary nil 'silent))
          (set-file-modes temporary (file-modes target))
          (awc-bibliography--check snapshots)
          (make-directory awc-research-data-directory t)
          (setq backup (make-temp-file (expand-file-name "bibliography-before-" awc-research-data-directory)))
          (copy-file target backup t)
          (set-file-modes backup #o600)
          (awc-bibliography--check snapshots)
          (rename-file temporary target t)
          (setq temporary nil awc-bibliography--plan nil)
          (when-let* ((buffer (find-buffer-visiting target)))
            (with-current-buffer buffer (revert-buffer t t)))
          (when-let* ((document (plist-get plan :document)))
            (when (buffer-live-p document)
              (with-current-buffer document (quarto-references--read))))
          (message "Applied project reference %s; previous bibliography: %s" (plist-get plan :key) backup)
          target)
      (when (and temporary (file-exists-p temporary)) (delete-file temporary))
      (unlock-file target))))

(provide 'awc-bibliography)
;;; awc-bibliography.el ends here

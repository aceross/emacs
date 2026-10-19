;;; awc-research.el --- Zotero references and Obsidian literature notes -*- lexical-binding: t; -*-

;; Zotero is read-only here.  Existing bibliographies and notes are never
;; rewritten by import or note creation.  New records live in a separate file.
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url-http)
(require 'url-util)
(require 'bibtex)
(require 'awc-files)
(require 'autorevert)
(declare-function citar-get-value "citar" (field key-or-entry))
(declare-function citar-get-entry "citar" (key))
(declare-function citar-get-entries "citar" ())
(declare-function citar-get-files "citar" (&optional keys))
(declare-function citar-select-ref "citar" (&rest args))
(declare-function citar-register-notes-source "citar" (name config))
(declare-function citar-file--parse-file-field "citar-file" (field dirs &optional key))
(declare-function citar-file--get-notes "citar-file" (&optional keys))
(declare-function citar-file--get-from-file-field "citar-file" (&optional keys))
(declare-function citar-file-open "citar-file" (file))
(declare-function citar-file-open-external "citar-file" (file))
(defvar citar-bibliography)
(defvar citar--entries)
(defvar citar-notes-paths)
(defvar citar-notes-source)
(defvar citar-file-note-extensions)
(defvar citar-file-sources)
(defvar url-http-response-status)
(defvar awc-research-personal-library-id)
(define-error 'awc-research-http-error "Zotero API request failed" 'user-error)

(defgroup awc-research nil "References and literature notes." :group 'citar)
(defcustom awc-research-vault-directory "~/Documents/Obsidian Vault/"
  "Existing Obsidian vault.  Never created implicitly."
  :type 'directory)
(defcustom awc-research-notes-subdirectory "Literature"
  "Directory within the vault for new literature notes."
  :type 'string)
(defcustom awc-research-legacy-notes-directories
  '("~/Documents/zettelkasten/zettelkasten/")
  "Existing Org notes to keep available through Citar."
  :type '(repeat directory))
(defcustom awc-research-data-directory
  (expand-file-name "var/research/" user-emacs-directory)
  "Local bibliography, Zotero associations and their backups."
  :type 'directory)
(defcustom awc-research-zotero-library "users/0"
  "Local API library, users/0, users/NUMBER or groups/NUMBER."
  :type 'string)
(defcustom awc-research-zotero-api-url "http://127.0.0.1:23119/api/"
  "Loopback Zotero API base. A disposable test server can use another port."
  :type 'string)

(defun awc-research--bibliography ()
  (expand-file-name "zotero.bib" awc-research-data-directory))

(defun awc-research--links-file ()
  (expand-file-name "zotero-associations.json" awc-research-data-directory))

(defun awc-research--legacy-links-file ()
  (expand-file-name "zotero-links.json" awc-research-data-directory))

(defun awc-research--read-legacy-links ()
  (if (file-exists-p (awc-research--legacy-links-file))
      (with-temp-buffer
        (insert-file-contents (awc-research--legacy-links-file))
        (json-parse-buffer :object-type 'hash-table))
    (make-hash-table :test 'equal)))

(defun awc-research--read-links ()
  (require 'awc-research-portable)
  (awc-research-portable--links))

(defun awc-research--write-owned (file text)
  "Atomically replace our FILE with TEXT, preserving an existing version."
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (buffer-modified-p buffer)
      (user-error "Save or revert unsaved edits to %s before importing" file)))
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-temp-file (concat file ".tmp-"))))
    (unwind-protect
        (progn
          (when (file-exists-p file)
            (copy-file file (make-temp-file (concat file ".backup-")) t))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region text nil temporary nil 'silent))
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun awc-research--library (library)
  (unless (and (stringp library) (string-match-p "\\`\\(?:users/[0-9]+\\|groups/[1-9][0-9]*\\)\\'" library))
    (user-error "Use users/NUMBER or groups/NUMBER for the Zotero library"))
  library)

(defun awc-research--get (path &optional raw)
  "GET PATH from Zotero.  Return JSON unless RAW requests the response text."
  (unless (string-match-p "\\`http://\\(?:127\\.0\\.0\\.1\\|localhost\\):[0-9]+/api/\\'" awc-research-zotero-api-url)
    (user-error "Zotero API must use a loopback address and explicit port"))
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Zotero-API-Version" . "3")))
         (url-proxy-services nil)
         (url-show-status nil)
         (buffer (condition-case nil
                     (url-retrieve-synchronously
                      (concat awc-research-zotero-api-url path) t t 8)
                   (error nil))))
    (unless (buffer-live-p buffer)
      (user-error "Cannot reach Zotero; start it and enable its local API"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (eq url-http-response-status 200)
            (signal 'awc-research-http-error
                    (list (format "HTTP %s%s" url-http-response-status
                                  (if (eq url-http-response-status 403)
                                      "; enable Settings → Advanced → Allow other applications" ""))
                          url-http-response-status)))
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (user-error "Invalid Zotero response"))
          (let ((text (decode-coding-string
                       (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
            (if raw text
              (json-parse-string text :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil))))
      (kill-buffer buffer))))

(defun awc-research--entry (text)
  "Read the one BibTeX record in TEXT, preserving its citation key."
  (with-temp-buffer
    ;; bibtex-map-entries needs a terminating line for forward progress.
    (insert text "\n")
    (delay-mode-hooks (bibtex-mode))
    (let (entries)
      (bibtex-map-entries
       (lambda (_key _begin _end) (push (bibtex-parse-entry t) entries)))
      (unless (= (length entries) 1)
        (user-error "Expected one exported Zotero reference"))
      (car entries))))

(defun awc-research--normal (text)
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "" (or text ""))))

(defun awc-research--same-reference-p (left right)
  "Match DOI, or title and year when both DOI values are not available."
  (let ((ld (citar-get-value "doi" left)) (rd (citar-get-value "doi" right)))
    (if (and ld rd (not (string-empty-p ld)) (not (string-empty-p rd)))
        (equal (downcase ld) (downcase rd))
      (let ((title (awc-research--normal (citar-get-value "title" left))))
        (and (not (string-empty-p title))
             (equal title (awc-research--normal (citar-get-value "title" right)))
             (equal (citar-get-value "year" left) (citar-get-value "year" right)))))))

(defun awc-research--activate-bibliography ()
  "Add the supplement to global Citar sources, leaving project locals alone."
  (when (file-readable-p (awc-research--bibliography))
    (set-default 'citar-bibliography
                 (cl-adjoin (awc-research--bibliography)
                            (default-value 'citar-bibliography) :test #'equal))))

(defun awc-research-import-item (item-key &optional library)
  "Associate Zotero ITEM-KEY with a preserved Citar key; import if absent.
Only our supplemental bibliography and local association file may change.
LIBRARY defaults to `awc-research-zotero-library'.  Return the citation key."
  (require 'citar)
  (require 'awc-research-portable)
  (setq library (awc-research--library (or library awc-research-zotero-library)))
  (unless (string-match-p "\\`[A-Z0-9]\\{8\\}\\'" item-key)
    (user-error "Invalid Zotero item key"))
  (awc-research--activate-bibliography)
  (let* ((snapshots (mapcar #'awc-bibliography--snapshot
                           (awc-research-portable--inputs (awc-research--bibliography))))
         (item (awc-research--get (format "%s/items/%s" library item-key)))
         (data (alist-get 'data item))
         (library (awc-research-portable--library library item))
         (awc-research-personal-library-id
          (if (and (string-prefix-p "users/" library) (not (equal library "users/0")))
              (string-to-number (substring library 6)) awc-research-personal-library-id))
         (text (awc-research--get
                (format "%s/items?itemKey=%s&format=bibtex" library item-key) t))
         (entry (awc-research--entry text))
         (export-key (citar-get-value "=key=" entry))
         (zotero-key (alist-get 'citationKey data))
         (links (awc-research--read-links))
         (citar--entries nil)
         (entries (let ((citar-bibliography (default-value 'citar-bibliography)))
                    (citar-get-entries)))
         existing matches)
    (unless (equal (alist-get 'key item) item-key)
      (user-error "Zotero returned a different item identity"))
    (unless (and export-key (not (string-empty-p export-key)))
      (user-error "Zotero export has no citation key"))
    (when (and zotero-key (not (equal export-key zotero-key)))
      (user-error "Zotero export key differs from its saved key; resolve before importing"))
    (maphash (lambda (key link)
               (when (and (equal (gethash "library" link) library)
                          (equal (gethash "item" link) item-key))
                 (push key matches))) links)
    (when (> (length matches) 1)
      (user-error "This Zotero item has multiple local citation keys"))
    (setq existing (car matches))
    (when (and existing (not (gethash existing entries))
               (not (gethash existing (awc-research-portable--read))))
      (user-error "Previously linked citation %s is missing; restore its bibliography" existing))
    (unless existing
      (setq matches nil)
      (maphash (lambda (key candidate)
                 (when (awc-research--same-reference-p entry candidate)
                   (push key matches))) entries)
      (when (> (length matches) 1)
        (user-error "Multiple existing keys match this paper: %s" (string-join matches ", ")))
      (setq existing (car matches)))
    (let* ((key (or existing export-key)) (previous (gethash key links)))
      (when (and (not existing) (gethash key entries))
        (user-error "Citation key %s already belongs to a different reference" key))
      (when (and previous
                 (not (and (equal library (gethash "library" previous))
                           (equal item-key (gethash "item" previous)))))
        (user-error "Citation %s is already linked to another Zotero item" key))
      (awc-bibliography--check snapshots)
      (unless (gethash key entries)
        ;; Restore the transferred canonical key in our own supplement.
        ;; Paths belong to this device's cache, not new bibliography records.
        (awc-research--write-owned
         (awc-research--bibliography)
         (concat (when (file-exists-p (awc-research--bibliography))
                   (with-temp-buffer
                     (insert-file-contents (awc-research--bibliography)) (buffer-string)))
                 "\n" (let ((record (car (awc-bibliography--records text "Zotero export"))))
                          (awc-bibliography--render key record (awc-bibliography--fields record))) "\n")))
      (let ((link (make-hash-table :test 'equal)))
        (puthash "library" library link)
        (puthash "item" item-key link)
        (puthash "zoteroCitationKey" (or zotero-key export-key) link)
        (awc-research-portable--save-item
         key link (when-let* ((field (citar-get-value "file" entry)))
                    (citar-file--parse-file-field field (list (expand-file-name "~/")) key)) entry))
      (awc-research--activate-bibliography)
      (message "Zotero reference ready: %s" key)
      key)))

;;;###autoload
(defun awc-research-import-zotero (query)
  "Search Zotero for QUERY, then import/associate one chosen reference."
  (interactive (list (read-string "Search Zotero (title/author): ")))
  (when (string-empty-p (string-trim query)) (user-error "Enter a search term"))
  (let* ((library (awc-research--library awc-research-zotero-library))
         (items (awc-research--get
                 (format "%s/items/top?q=%s&limit=100" library (url-hexify-string query))))
         (choices (mapcar
                   (lambda (item)
                     (let ((data (alist-get 'data item)))
                       (cons (format "%s · %s [%s]" (alist-get 'title data)
                                     (or (alist-get 'date data) "") (alist-get 'key item))
                             (alist-get 'key item)))) items)))
    (unless choices (user-error "No Zotero matches"))
    (when (= (length choices) 100)
      (user-error "100 results returned; narrow the search"))
    (awc-research-import-item
     (cdr (assoc (completing-read "Reference: " choices nil t) choices)) library)))

(defun awc-research--property (name)
  "Read our scalar YAML property NAME in the current Markdown buffer."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---[ \t]*\n")
      (let ((start (match-end 0)))
        (goto-char start)
        (when (re-search-forward "^---[ \t]*$" nil t)
          (let ((end (match-beginning 0)))
            (goto-char start)
            (when (re-search-forward (concat "^" (regexp-quote name) ":[ \t]*\\(.+\\)$") end t)
              (let ((value (string-trim (match-string-no-properties 1))))
                (cond ((string-prefix-p "\"" value)
                       (condition-case nil (json-parse-string value) (error nil)))
                      ((and (string-prefix-p "'" value) (string-suffix-p "'" value))
                       (replace-regexp-in-string "''" "'" (substring value 1 -1) t t))
                      (t value))))))))))

(defun awc-research--markdown-notes ()
  "Index citekey properties in vault notes, preferring unsaved open buffers."
  (let ((notes (make-hash-table :test 'equal))
        (vault (expand-file-name awc-research-vault-directory))
        (entries (let ((citar-bibliography (default-value 'citar-bibliography))
                       (citar--entries nil))
                   (citar-get-entries))))
    (when (file-directory-p vault)
      (dolist (file (directory-files-recursively
                     vault "\\.md\\'" nil
                     (lambda (directory)
                       (not (string-prefix-p "." (file-name-nondirectory directory))))))
        (when (file-in-directory-p file vault)
          (let* ((buffer (find-buffer-visiting file))
                 (key (or (if buffer
                          (with-current-buffer buffer (awc-research--property "citekey"))
                        (with-temp-buffer
                          (insert-file-contents file)
                          (awc-research--property "citekey")))
                          (when (gethash (file-name-base file) entries) (file-name-base file)))))
            (when (and (stringp key) (not (string-empty-p key)))
              (puthash key (cons file (gethash key notes)) notes))))))
    notes))

(defun awc-research--notes (&optional keys)
  "Return Markdown and legacy Org notes for KEYS, or all notes."
  (let* ((notes (awc-research--markdown-notes))
         (citar-notes-paths
          (cl-remove-if-not #'file-directory-p
                            (mapcar #'expand-file-name awc-research-legacy-notes-directories)))
         (citar-file-note-extensions '("org")))
    (when citar-notes-paths
      (maphash (lambda (key files) (puthash key (append (gethash key notes) files) notes))
               (citar-file--get-notes keys)))
    (when keys
      (maphash (lambda (key _) (unless (member key keys) (remhash key notes))) notes))
    notes))

(defun awc-research--has-notes ()
  (let ((notes (awc-research--notes)))
    (lambda (key) (and (gethash key notes) t))))

(defun awc-research--files (&optional keys)
  "Return readable files from explicit Zotero associations, without network."
  (let ((files (make-hash-table :test 'equal))
        (bib-files (citar-file--get-from-file-field keys)))
    (maphash (lambda (key link)
               (when (or (null keys) (member key keys))
                 (when-let* ((paths (cl-remove-if
                                    (lambda (path) (or (file-remote-p path) (not (file-readable-p path))
                                                       (member path (gethash key bib-files))))
                                    (append (gethash "files" link) nil))))
                   (puthash key paths files))))
             (awc-research--read-links))
    files))

(defun awc-research--has-files ()
  (let ((files (awc-research--files)))
    (lambda (key) (and (gethash key files) t))))

(defun awc-research--note-name (key)
  "Return a portable filename without changing citation KEY."
  (concat (substring (replace-regexp-in-string "[^[:alnum:]_-]" "-" key)
                     0 (min 80 (length key)))
          "--" (substring (secure-hash 'sha256 key) 0 10) ".md"))

(defun awc-research--zotero-uri (link)
  (let ((library (gethash "library" link)))
    (format "zotero://select/%s/items/%s"
            (if (string-prefix-p "users/" library) "library" library) (gethash "item" link))))

(defun awc-research--global-key ()
  "Use a literature note's key, otherwise select from global references."
  (require 'citar)
  (or (awc-research--property "citekey")
      (let ((citar-bibliography (default-value 'citar-bibliography))
            (citar--entries nil))
        (citar-select-ref))))

(defun awc-research--file-uri (file)
  "Encode FILE as a URI, including literal #, ? and Unicode in filenames."
  (concat "file://" (mapconcat #'url-hexify-string
                              (split-string (expand-file-name file) "/") "/")))

(defun awc-research--protect-note ()
  "Keep backups and recovery files for vault Markdown outside the vault."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (equal (file-name-extension buffer-file-name) "md")
             (file-in-directory-p buffer-file-name (expand-file-name awc-research-vault-directory)))
    (let ((backups (expand-file-name "backups/" awc-research-data-directory))
          (autosaves (expand-file-name "autosave/" awc-research-data-directory)))
      (awc-files-private-directory backups)
      (awc-files-private-directory autosaves)
      (setq-local make-backup-files t
                  backup-inhibited nil
                  backup-by-copying t
                  backup-directory-alist (list (cons "." backups))
                  auto-save-file-name-transforms (list (list ".*" autosaves t)))
      ;; Retain the vault's established filenames.  When reapplying this
      ;; hook, move a current-session recovery copy before changing its name.
      (when buffer-auto-save-file-name (rename-auto-save-file))
      (auto-save-mode 1)
      ;; Auto Revert only refreshes unmodified buffers.  Modified buffers
      ;; retain the native supersession/save confirmation checks.
      (auto-revert-mode 1))))

;;;###autoload
(defun awc-research-open-note (key &optional entry)
  "Visit the existing Markdown note for KEY, or create a new literature note.
ENTRY is supplied by Citar.  Existing notes are never modified."
  (interactive (list (awc-research--global-key)))
  (require 'citar)
  (let* ((citar-bibliography (default-value 'citar-bibliography))
         (citar--entries nil)
         (notes (gethash key (awc-research--markdown-notes)))
         (vault (expand-file-name awc-research-vault-directory))
         (directory (expand-file-name awc-research-notes-subdirectory vault))
         (file (expand-file-name (awc-research--note-name key) directory)))
    (if notes
        (find-file (if (= (length notes) 1) (car notes)
                     (completing-read "Existing literature note: " notes nil t)))
      (unless (file-directory-p vault) (user-error "Configure an existing Obsidian vault first"))
      (unless (file-in-directory-p file vault) (user-error "Literature notes must stay inside the vault"))
      (setq entry (or entry (citar-get-entry key)))
      (unless entry (user-error "Unknown citation key: %s" key))
      (when (or (file-exists-p file) (file-symlink-p file) (find-buffer-visiting file))
        (user-error "Note path is already occupied; inspect %s" file))
      (let* ((link (gethash key (awc-research--read-links)))
             (title (or (citar-get-value "title" entry) key))
             (text (concat
                    "---\ntitle: " (json-encode-string title)
                    "\ncitekey: " (json-encode-string key)
                    "\ntype: literature\nstatus: unread\n"
                    (when link
                      (concat "zotero: " (json-encode-string (awc-research--zotero-uri link))
                              "\nzotero_item: " (json-encode-string (gethash "item" link))
                              "\nzotero_library: " (json-encode-string (gethash "library" link)) "\n"))
                    "---\n\n# " title "\n\n"
                    "Citation: [@" key "]\n\n"
                    (when link (format "[Open in Zotero](%s)\n\n" (awc-research--zotero-uri link)))
                    (when link (awc-research-portable--pdf-links link))
                    "## Summary\n\n## Source passages\n\n## My analysis\n\n## Related notes\n")))
        (make-directory directory t)
        ;; MUSTBENEW prevents races from overwriting a file created elsewhere.
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil file nil 'silent nil 'excl))
        (find-file file)))))

;;;###autoload
(defun awc-research-open-zotero (key)
  "Open the explicitly associated Zotero item for KEY."
  (interactive (list (awc-research--global-key)))
  (require 'citar)
  (let ((link (gethash key (awc-research--read-links))))
    (unless link (user-error "Associate this reference with M-x awc-research-import-zotero first"))
    (citar-file-open-external (awc-research--zotero-uri link))))

;;;###autoload
(defun awc-research-open-pdf (key)
  "Open an existing local PDF for KEY using Citar and PDF Tools."
  (interactive (list (awc-research--global-key)))
  (require 'citar)
  (let* ((citar-bibliography (default-value 'citar-bibliography))
         (citar--entries nil)
         (link (gethash key (awc-research--read-links)))
         (files (delete-dups (cl-remove-if-not
                 #'awc-research-portable--pdf-file-p
                 (if link (append (gethash "files" link) nil)
                   (gethash key (citar-get-files (list key))))))))
    (unless files (user-error "No readable device PDF for %s; resolve with C-c w n f or link one with C-c w n P" key))
    (citar-file-open (if (= (length files) 1) (car files)
                       (completing-read "PDF: " files nil t)))))

;;;###autoload
(defun awc-research-open-in-obsidian ()
  "Open the current Markdown file in Obsidian."
  (interactive)
  (require 'citar)
  (unless (and buffer-file-name (file-exists-p buffer-file-name)
               (file-in-directory-p buffer-file-name (expand-file-name awc-research-vault-directory)))
    (user-error "Visit a saved note in the configured vault first"))
  (citar-file-open-external
   (concat "obsidian://open?path=" (url-hexify-string (expand-file-name buffer-file-name)))))

(defun awc-research-setup ()
  "Register our Citar notes source without touching existing files."
  (require 'citar)
  (require 'citar-markdown)
  (add-hook 'find-file-hook #'awc-research--protect-note)
  (awc-research--activate-bibliography)
  (cl-pushnew (list :items #'awc-research--files :hasitems #'awc-research--has-files)
              citar-file-sources :test #'equal)
  (citar-register-notes-source
   'awc-research (list :name "Literature notes" :category 'file
                       :items #'awc-research--notes :hasitems #'awc-research--has-notes
                       :open #'find-file :create #'awc-research-open-note
                       :transform #'file-name-nondirectory))
  (setq citar-notes-source 'awc-research))

(provide 'awc-research)
;;; awc-research.el ends here

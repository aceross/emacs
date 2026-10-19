;;; awc-research-portable-test.el --- Identity transfer and PDF preservation -*- lexical-binding: t; -*-
(require 'awc-research-test)
(require 'awc-research-portable)

(defmacro awc-research-portable-test--fixture (&rest body)
  (declare (indent 0))
  `(awc-research-test--fixture
     (let ((awc-research-personal-library-id nil)) ,@body)))

(defun awc-research-portable-test--apply (plan)
  (with-temp-buffer
    (setq-local awc-research-portable--plan plan)
    (awc-research-portable-apply)))

(defun awc-research-portable-test--pdf (file)
  ;; Unit tests inspect only the type header. Integration uses a real PDF.
  (with-temp-file file (insert "%PDF-1.4\nSynthetic unit attachment.\n"))
  file)

(defun awc-research-portable-test--packet (key library item &optional title)
  (let ((row (make-hash-table :test 'equal)) (links (make-hash-table :test 'equal)))
    (puthash "library" library row) (puthash "item" item row)
    (when title
      (let ((hint (make-hash-table :test 'equal)))
        (puthash "title" title hint) (puthash "year" "2024" hint) (puthash "reference" hint row)))
    (puthash key row links)
    (awc-research-portable--encode links t)))

(ert-deftest awc-research-portable-export-is-path-free-reviewed-and-cancellable ()
  (awc-research-portable-test--fixture
    (let ((pdf (awc-research-portable-test--pdf (expand-file-name "source é #1.pdf" root)))
          (packet (expand-file-name "transfer.json" root)))
      (setq awc-research-test--export
            (format "@article{saved24:key,title={A source},year={2024},file={PDF:%s:application/pdf}}" pdf))
      (awc-research-import-item "ABC12345" "groups/42")
      (let* ((state (awc-research-test--read (awc-research--links-file)))
             (cache (awc-research-test--read (awc-research-portable--cache-file)))
             (plan (awc-research-portable--prepare 'export packet))
             (review (save-window-excursion (awc-research-portable--review plan))))
        (should-not (file-exists-p packet))
        (with-current-buffer review
          (should (eq (key-binding (kbd "C-c C-c")) #'awc-research-portable-apply))
          (should buffer-read-only))
        (kill-buffer review)
        (should-not (file-exists-p packet))
        (awc-research-portable-test--apply plan)
        (let ((text (awc-research-test--read packet)))
          (should-not (string-match-p (regexp-quote root) text))
          (should-not (string-match-p "\"files\"\\|\"path\"\\|\"attachments\"" text))
          (should (gethash "saved24:key" (awc-research-portable--decode text t))))
        (should (equal state (awc-research-test--read (awc-research--links-file))))
        (should (equal cache (awc-research-test--read (awc-research-portable--cache-file))))
        (should-not (file-exists-p (awc-research--legacy-links-file)))))))

(ert-deftest awc-research-portable-legacy-reading-never-migrates-or-rewrites ()
  (awc-research-portable-test--fixture
    (let* ((pdf (awc-research-portable-test--pdf (expand-file-name "legacy.pdf" root)))
           (legacy (make-hash-table :test 'equal)) (row (make-hash-table :test 'equal)))
      (puthash "library" "users/0" row) (puthash "item" "ABC12345" row)
      (puthash "files" (vector pdf) row) (puthash "legacy:key" row legacy)
      (make-directory awc-research-data-directory)
      (with-temp-file (awc-research--legacy-links-file) (insert (json-encode legacy)))
      (let ((before (awc-research-test--read (awc-research--legacy-links-file))))
        (should (equal (append (gethash "files" (gethash "legacy:key" (awc-research--read-links))) nil) (list pdf)))
        (should-not (file-exists-p (awc-research--links-file)))
        (should-not (file-exists-p (awc-research-portable--cache-file)))
        (let ((awc-research-personal-library-id 12345))
          (let ((plan (awc-research-portable--prepare 'export (expand-file-name "portable.json" root))))
            (should (string-match-p "users/12345" (plist-get plan :new)))
            (should-not (string-match-p (regexp-quote pdf) (plist-get plan :new)))))
        (should (equal before (awc-research-test--read (awc-research--legacy-links-file))))))))

(ert-deftest awc-research-portable-personal-alias-needs-actual-identity ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345")
    (should-error (awc-research-portable--prepare 'export (expand-file-name "transfer.json" root)) :type 'user-error)
    (cl-letf (((symbol-function 'awc-research--get)
               (lambda (&rest _) '((key . "ABC12345") (library . ((type . "user") (id . 12345)))))))
      (let ((plan (awc-research-portable--prepare 'export (expand-file-name "transfer.json" root))))
        (should (string-match-p "users/12345" (plist-get plan :new)))
        (should-not (string-match-p "users/0" (plist-get plan :new)))))))

(ert-deftest awc-research-portable-import-reuses-canonical-key-without-cache ()
  (awc-research-portable-test--fixture
    (let ((packet (expand-file-name "transfer.json" root)))
      (with-temp-file packet
        (insert (awc-research-portable-test--packet "legacy:key" "groups/42" "ABC12345" "A source")))
      (awc-research-portable-test--apply (awc-research-portable--prepare 'import packet))
      (should-not (file-exists-p (awc-research-portable--cache-file)))
      (let ((citar--entries nil))
        (should (equal "legacy:key" (awc-research-import-item "ABC12345" "groups/42"))))
      (let ((text (awc-research-test--read (awc-research--bibliography))))
        (should (string-match-p "@article{legacy:key," text))
        (should-not (string-match-p "@article{saved24:key," text)))
      (should (gethash "legacy:key" (awc-research--read-links)))
      (let ((again (awc-research-portable--prepare 'import packet)))
        (should (equal (plist-get again :old) (plist-get again :new)))))))

(ert-deftest awc-research-portable-rejects-ambiguous-or-path-bearing-packets ()
  (dolist (text '("{}" "{" "{\"version\":2,\"associations\":[]}"
                  "{\"version\":1,\"associations\":[{\"citekey\":\"a\",\"library\":\"users/0\",\"item\":\"ABC12345\"}]}"
                  "{\"version\":1,\"associations\":[{\"citekey\":\"a\",\"library\":\"groups/42\",\"item\":\"ABC12345\",\"files\":[\"/tmp/a.pdf\"]}]}"
                  "{\"version\":1,\"associations\":[{\"citekey\":\"a\",\"library\":\"groups/42\",\"item\":\"bad\"}]}"
                  "{\"version\":1,\"associations\":[{\"citekey\":\"a\",\"library\":\"groups/42\",\"item\":\"ABC12345\"},{\"citekey\":\"b\",\"library\":\"groups/42\",\"item\":\"ABC12345\"}]}"
                  "{\"version\":1,\"associations\":[{\"citekey\":\"a\",\"library\":\"groups/42\",\"item\":\"ABC12345\"},{\"citekey\":\"a\",\"library\":\"groups/42\",\"item\":\"DEF12345\"}]}"))
    (should-error (awc-research-portable--decode text t) :type 'user-error)))

(ert-deftest awc-research-portable-import-refuses-existing-identity-and-key-conflicts ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let ((packet (expand-file-name "transfer.json" root))
          (state (awc-research-test--read (awc-research--links-file))))
      (dolist (text (list (awc-research-portable-test--packet "saved24:key" "groups/99" "ABC12345")
                          (awc-research-portable-test--packet "different:key" "groups/42" "ABC12345")))
        (with-temp-file packet (insert text))
        (should-error (awc-research-portable--prepare 'import packet) :type 'user-error)
        (should (equal state (awc-research-test--read (awc-research--links-file))))))))

(ert-deftest awc-research-portable-import-refuses-global-bibliography-collision ()
  (awc-research-portable-test--fixture
    (let ((packet (expand-file-name "transfer.json" root)))
      (with-temp-file primary (insert "@book{legacy:key,title={A different source},year={1990}}\n"))
      (with-temp-file packet (insert (awc-research-portable-test--packet "legacy:key" "groups/42" "ABC12345" "A source")))
      (should-error (awc-research-portable--prepare 'import packet) :type 'user-error)
      (should-not (file-exists-p (awc-research--links-file))))))

(ert-deftest awc-research-portable-review-refuses-stale-and-dirty-inputs ()
  (awc-research-portable-test--fixture
    (let ((packet (expand-file-name "transfer.json" root)))
      (with-temp-file packet (insert (awc-research-portable-test--packet "legacy:key" "groups/42" "ABC12345")))
      (let ((plan (awc-research-portable--prepare 'import packet)))
        (with-temp-buffer (insert-file-contents packet) (goto-char (point-max)) (insert "\n") (write-region (point-min) (point-max) packet nil 'silent))
        (should-error (awc-research-portable-test--apply plan) :type 'user-error))
      (let* ((plan (awc-research-portable--prepare 'import packet)) (buffer (find-file-noselect packet)))
        (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
        (should-error (awc-research-portable-test--apply plan) :type 'user-error)
        (should (buffer-modified-p buffer)))
      (should-not (file-exists-p (awc-research--links-file))))))

(ert-deftest awc-research-portable-write-failure-preserves-previous-state ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let* ((packet (expand-file-name "transfer.json" root))
           (plan (awc-research-portable--prepare 'export packet))
           (state (awc-research-test--read (awc-research--links-file))))
      (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (signal 'file-error '("Fixture rename failure")))))
        (should-error (awc-research-portable-test--apply plan) :type 'file-error))
      (should-not (file-exists-p packet))
      (should-not (directory-files root nil "transfer.json.tmp-"))
      (should (equal state (awc-research-test--read (awc-research--links-file)))))))

(ert-deftest awc-research-portable-url-decoder-refuses-remote-and-preserves-special-characters ()
  (let ((file "/tmp/Paper é #1 ?.pdf"))
    (should (equal file (awc-research-portable--url-path (awc-research--file-uri file)))))
  (dolist (url '("https://example.org/a.pdf" "file://remote-host/tmp/a.pdf" "file:///tmp/a.pdf#fragment" "file:///ssh:remote:/a.pdf" "file:///tmp/a%00.pdf"))
    (should-error (awc-research-portable--url-path url) :type 'user-error)))

(ert-deftest awc-research-portable-multiple-pdfs-require-choice-and-open-offline ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let ((first (awc-research-portable-test--pdf (expand-file-name "PDF é #1 ?.pdf" root)))
          (second (awc-research-portable-test--pdf (expand-file-name "Appendix.pdf" root))) opened)
      (cl-letf (((symbol-function 'awc-research--get)
                 (lambda (path &optional raw)
                   (cond
                    (raw (awc-research--file-uri (if (string-match-p "PDF12345" path) first second)))
                    ((string-suffix-p "/children" path)
                     '(((key . "PDF12345") (data . ((itemType . "attachment") (contentType . "application/pdf") (parentItem . "ABC12345"))))
                       ((key . "PDF67890") (data . ((itemType . "attachment") (contentType . "application/pdf") (parentItem . "ABC12345"))))))
                    (t '((key . "ABC12345") (library . ((type . "group") (id . 42)))))))))
        (should (= 2 (length (awc-research-resolve-pdfs "saved24:key")))))
      (cl-letf (((symbol-function 'awc-research--get) (lambda (&rest _) (error "Opening must remain offline")))
                ((symbol-function 'completing-read) (lambda (_ choices &rest _) (should (= 2 (length choices))) second))
                ((symbol-function 'citar-file-open) (lambda (file) (setq opened file))))
        (awc-research-open-pdf "saved24:key")
        (should (equal opened second)))
      (let ((text (with-current-buffer (awc-research-open-note "saved24:key") (buffer-string))))
        (should (string-match-p "zotero://open-pdf/groups/42/items/PDF12345" text))
        (should-not (string-match-p "file://" text)))
      (should (= (file-modes (awc-research-portable--cache-file)) #o600)))))

(ert-deftest awc-research-portable-associated-pdf-never-uses-copied-bibtex-path ()
  (awc-research-portable-test--fixture
    (let ((foreign (awc-research-portable-test--pdf (expand-file-name "other-device.pdf" root)))
          (packet (expand-file-name "transfer.json" root)))
      (with-temp-file primary
        (insert (format "@article{legacy:key,title={A source},year={2024},file={%s}}\n" foreign)))
      (with-temp-file packet (insert (awc-research-portable-test--packet "legacy:key" "groups/42" "ABC12345" "A source")))
      (awc-research-portable-test--apply (awc-research-portable--prepare 'import packet))
      (should-error (awc-research-open-pdf "legacy:key") :type 'user-error))))

(ert-deftest awc-research-portable-offline-manual-link-stays-local ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let* ((pdf (awc-research-portable-test--pdf (expand-file-name "offline.pdf" root)))
           (state (awc-research-test--read (awc-research--links-file))))
      (cl-letf (((symbol-function 'awc-research--get) (lambda (&rest _) (error "Manual linking is offline"))))
        (awc-research-link-local-pdf "saved24:key" pdf))
      (should (member pdf (append (gethash "files" (gethash "saved24:key" (awc-research--read-links))) nil)))
      (should (equal state (awc-research-test--read (awc-research--links-file))))
      (should-not (string-match-p (regexp-quote pdf)
                                 (plist-get (awc-research-portable--prepare 'export (expand-file-name "transfer.json" root)) :new))))))

(ert-deftest awc-research-portable-api-failure-keeps-cache-but-confirmed-absence-clears-it ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (awc-research-link-local-pdf "saved24:key" (awc-research-portable-test--pdf (expand-file-name "old.pdf" root)))
    (let ((cache (awc-research-test--read (awc-research-portable--cache-file))))
      (cl-letf (((symbol-function 'awc-research--get) (lambda (&rest _) (user-error "Fixture API unavailable"))))
        (should-error (awc-research-resolve-pdfs "saved24:key") :type 'user-error))
      (should (equal cache (awc-research-test--read (awc-research-portable--cache-file))))
      (cl-letf (((symbol-function 'awc-research--get)
                 (lambda (path &optional _) (unless (string-suffix-p "/children" path) '((key . "ABC12345"))))))
        (should-error (awc-research-resolve-pdfs "saved24:key") :type 'user-error))
      (should (seq-empty-p (gethash "files" (gethash "saved24:key" (awc-research--read-links))))))))

(ert-deftest awc-research-portable-wrong-library-parent-or-url-preserves-cache ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (awc-research-link-local-pdf "saved24:key" (awc-research-portable-test--pdf (expand-file-name "old.pdf" root)))
    (let ((cache (awc-research-test--read (awc-research-portable--cache-file))))
      (dolist (failure '(library parent url))
        (cl-letf (((symbol-function 'awc-research--get)
                   (lambda (path &optional raw)
                     (cond (raw "file://foreign-host/source.pdf")
                           ((string-suffix-p "/children" path)
                            `(((key . "PDF12345") (data . ((itemType . "attachment") (contentType . "application/pdf")
                                                           (parentItem . ,(if (eq failure 'parent) "OTHER123" "ABC12345")))))))
                           (t `((key . "ABC12345") (library . ((type . "group") (id . ,(if (eq failure 'library) 99 42))))))))))
          (should-error (awc-research-resolve-pdfs "saved24:key") :type 'user-error))
        (should (equal cache (awc-research-test--read (awc-research-portable--cache-file))))))))

(ert-deftest awc-research-portable-resolver-refuses-state-changed-during-api-read ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let ((state (awc-research-test--read (awc-research--links-file))))
      (cl-letf (((symbol-function 'awc-research--get)
                 (lambda (path &optional _)
                   (if (string-suffix-p "/children" path)
                       (progn (with-temp-file (awc-research--links-file) (insert state "\n")) nil)
                     '((key . "ABC12345"))))))
        (should-error (awc-research-resolve-pdfs "saved24:key") :type 'user-error))
      (should-not (file-exists-p (awc-research-portable--cache-file))))))

(ert-deftest awc-research-portable-http-missing-file-clears-but-denied-file-preserves ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (awc-research-link-local-pdf "saved24:key" (awc-research-portable-test--pdf (expand-file-name "old.pdf" root)))
    (let ((before (awc-research-test--read (awc-research-portable--cache-file))) (status 403))
      (cl-letf (((symbol-function 'awc-research--get)
                 (lambda (path &optional raw)
                   (cond (raw (signal 'awc-research-http-error (list "Fixture HTTP error" status)))
                         ((string-suffix-p "/children" path)
                          '(((key . "PDF12345") (data . ((itemType . "attachment") (contentType . "application/pdf") (parentItem . "ABC12345"))))))
                         (t '((key . "ABC12345")))))))
        (should-error (awc-research-resolve-pdfs "saved24:key") :type 'awc-research-http-error)
        (should (equal before (awc-research-test--read (awc-research-portable--cache-file))))
        (setq status 404)
        (should-error (awc-research-resolve-pdfs "saved24:key") :type 'user-error)
        (should (seq-empty-p (gethash "files" (gethash "saved24:key" (awc-research--read-links)))))))))

(ert-deftest awc-research-portable-library-namespaces-are-distinct ()
  (let* ((text (concat "{\"version\":1,\"associations\":["
                       "{\"citekey\":\"personal\",\"library\":\"users/42\",\"item\":\"ABC12345\"},"
                       "{\"citekey\":\"group\",\"library\":\"groups/42\",\"item\":\"ABC12345\"}]}"))
         (links (awc-research-portable--decode text t)))
    (should (= 2 (hash-table-count links)))
    (should-not (equal (awc-research-portable--identity (gethash "personal" links))
                       (awc-research-portable--identity (gethash "group" links))))))

(ert-deftest awc-research-portable-export-refuses-symlink-and-internal-state ()
  (awc-research-portable-test--fixture
    (awc-research-import-item "ABC12345" "groups/42")
    (let ((alias (expand-file-name "alias.json" root)))
      (make-symbolic-link (awc-research--links-file) alias)
      (should-error (awc-research-portable--prepare 'export alias) :type 'user-error)
      (should-error (awc-research-portable--prepare 'export (awc-research--links-file)) :type 'user-error)
      (let* ((target (expand-file-name "transfer.json" root))
             (plan (awc-research-portable--prepare 'export target)))
        (cl-letf (((symbol-function 'file-locked-p) (lambda (_) "another-editor")))
          (should-error (awc-research-portable--prepare 'export target) :type 'user-error)
          (should-error (awc-research-portable-test--apply plan) :type 'user-error))
        (should-not (file-exists-p target))))))

(ert-deftest awc-research-portable-item-import-refuses-external-bibliography-edit ()
  (awc-research-portable-test--fixture
    (cl-letf (((symbol-function 'awc-research--get)
               (lambda (_ &optional raw)
                 (if raw
                     (progn
                       (with-temp-file primary (insert "@book{external,title={Edited elsewhere},year={2026}}\n"))
                       awc-research-test--export)
                   '((key . "ABC12345") (data . ((citationKey . "saved24:key"))))))))
      (should-error (awc-research-import-item "ABC12345" "groups/42") :type 'user-error))
    (should (string-match-p "Edited elsewhere" (awc-research-test--read primary)))
    (should-not (file-exists-p (awc-research--links-file)))
    (should-not (file-exists-p (awc-research--bibliography)))))

(provide 'awc-research-portable-test)
;;; awc-research-portable-test.el ends here

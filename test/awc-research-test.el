;;; awc-research-test.el --- Preserve references and literature notes -*- lexical-binding: t; -*-
(require 'ert)
(require 'citar)
(require 'awc-research)

(defmacro awc-research-test--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (make-temp-file "awc-research-test-" t))
          (awc-research-data-directory (expand-file-name "data/" root))
          (awc-research-vault-directory (expand-file-name "vault/" root))
          (awc-research-legacy-notes-directories (list (expand-file-name "legacy/" root)))
          (primary (expand-file-name "existing.bib" root))
          (citar-bibliography (list primary))
          (citar--entries nil)
          (citar-file-sources (copy-tree citar-file-sources))
          (citar-notes-sources (copy-tree citar-notes-sources))
          (citar-notes-source citar-notes-source)
          (citar-library-paths nil)
          (awc-research-test--export
           "@article{saved24:key, title={A source}, author={Person, Alice}, year={2024}, doi={10.1/source}}")
          (awc-research-test--api-key "saved24:key")
          (enable-local-variables nil)
          (auto-save-default nil)
          (create-lockfiles nil))
     (make-directory awc-research-vault-directory)
     (make-directory (car awc-research-legacy-notes-directories))
     (with-temp-file primary (insert "@book{unrelated, title={An old book}, year={1990}}\n"))
     (unwind-protect
         (cl-letf (((symbol-function 'awc-research--get)
                    (lambda (path &optional raw)
                      (if raw awc-research-test--export
                        `((key . "ABC12345")
                          (data . ((title . "A source") (citationKey . ,awc-research-test--api-key))))))))
           (awc-research-setup)
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer) root))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory root t))))

(defun awc-research-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest awc-research-import-is-idempotent-and-preserves-originals ()
  (awc-research-test--fixture
    (let ((original (awc-research-test--read primary)))
      (should (equal "saved24:key" (awc-research-import-item "ABC12345")))
      (let* ((bib (awc-research--bibliography))
             (links (awc-research--links-file))
             (bib-text (awc-research-test--read bib))
             (link-text (awc-research-test--read links))
             (bib-time (file-attribute-modification-time (file-attributes bib)))
             (link-time (file-attribute-modification-time (file-attributes links))))
        (should (string-match-p "@article{saved24:key," bib-text))
        (should (equal "saved24:key" (awc-research-import-item "ABC12345")))
        (should (equal bib-text (awc-research-test--read bib)))
        (should (equal link-text (awc-research-test--read links)))
        (should (equal bib-time (file-attribute-modification-time (file-attributes bib))))
        (should (equal link-time (file-attribute-modification-time (file-attributes links)))))
      (should (equal original (awc-research-test--read primary))))))

(ert-deftest awc-research-existing-key-wins-and-keeps-zotero-attachment ()
  (awc-research-test--fixture
    (let ((pdf (expand-file-name "source with spaces.pdf" root)))
      (with-temp-file pdf (insert "PDF fixture"))
      (setq awc-research-test--export
            (format "@article{saved24:key,title={A source},year={2024},doi={10.1/source},file={PDF:%s:application/pdf}}" pdf))
      (with-temp-file primary
        (insert "@article{legacy:key,title={A source},year={2024},doi={10.1/source}}"))
      (let ((original (awc-research-test--read primary)))
        (should (equal "legacy:key" (awc-research-import-item "ABC12345")))
        (should-not (file-exists-p (awc-research--bibliography)))
        (should (equal original (awc-research-test--read primary)))
        (should (member pdf (gethash "legacy:key" (citar-get-files '("legacy:key")))))
        (should (equal "saved24:key"
                       (gethash "zoteroCitationKey" (gethash "legacy:key" (awc-research--read-links)))))))))

(ert-deftest awc-research-collision-does-not-write ()
  (awc-research-test--fixture
    (with-temp-file primary
      (insert "@book{saved24:key,title={A different source},year={1991},doi={10.1/different}}"))
    (let ((original (awc-research-test--read primary)))
      (should-error (awc-research-import-item "ABC12345") :type 'user-error)
      (should-not (file-exists-p awc-research-data-directory))
      (should (equal original (awc-research-test--read primary))))))

(ert-deftest awc-research-export-must-agree-with-saved-zotero-key ()
  (awc-research-test--fixture
    (setq awc-research-test--api-key "a-new-key")
    (should-error (awc-research-import-item "ABC12345") :type 'user-error)
    (should-not (file-exists-p awc-research-data-directory))))

(ert-deftest awc-research-repeat-keeps-canonical-key-after-zotero-key-change ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let ((original (awc-research-test--read (awc-research--bibliography))))
      (setq awc-research-test--api-key "new-key"
            awc-research-test--export "@article{new-key,title={A source},year={2024},doi={10.1/source}}")
      (should (equal "saved24:key" (awc-research-import-item "ABC12345")))
      (should (equal original (awc-research-test--read (awc-research--bibliography)))))))

(ert-deftest awc-research-note-reopens-with-unsaved-edits-and-rename ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let* ((buffer (awc-research-open-note "saved24:key"))
           (file (buffer-file-name buffer))
           (original (awc-research-test--read file))
           (renamed (expand-file-name "A renamed note.md" awc-research-vault-directory)))
      (should (string-match-p "citekey: \"saved24:key\"" original))
      (should (string-match-p "zotero://select/library/items/ABC12345" original))
      (should-not (string-match-p ":" (file-name-nondirectory file)))
      (with-current-buffer buffer
        (goto-char (point-max)) (insert "Unfinished personal analysis."))
      (should (eq buffer (awc-research-open-note "saved24:key")))
      (should (equal original (awc-research-test--read file)))
      (should (buffer-modified-p buffer))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (rename-file file renamed)
      (should (equal renamed (buffer-file-name (awc-research-open-note "saved24:key"))))
      (should (equal original (awc-research-test--read renamed)))
      (should (= 1 (length (directory-files-recursively awc-research-vault-directory "\\.md\\'")))))))

(ert-deftest awc-research-existing-filename-note-and-legacy-org-stay-available ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let ((md (expand-file-name "saved24:key.md" awc-research-vault-directory))
          (org (expand-file-name "saved24:key.org" (car awc-research-legacy-notes-directories))))
      (with-temp-file md (insert "# Existing Markdown\nMy notes.\n"))
      (with-temp-file org (insert "#+title: Existing Org\nMore notes.\n"))
      (let ((original (awc-research-test--read md)))
        (should (equal md (buffer-file-name (awc-research-open-note "saved24:key"))))
        (should (equal original (awc-research-test--read md)))
        (should (member md (gethash "saved24:key" (citar-get-notes '("saved24:key")))))
        (should (member org (gethash "saved24:key" (citar-get-notes '("saved24:key")))))))))

(ert-deftest awc-research-note-refuses-existing-target-without-known-key ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let* ((dir (expand-file-name awc-research-notes-subdirectory awc-research-vault-directory))
           (file (expand-file-name (awc-research--note-name "saved24:key") dir)))
      (make-directory dir)
      (with-temp-file file (insert "Existing unrelated note."))
      (should-error (awc-research-open-note "saved24:key") :type 'user-error)
      (should (equal "Existing unrelated note." (awc-research-test--read file))))))

(ert-deftest awc-research-note-cannot-escape-vault ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let ((awc-research-notes-subdirectory "../outside"))
      (should-error (awc-research-open-note "saved24:key") :type 'user-error))
    (make-symbolic-link root (expand-file-name "escape" awc-research-vault-directory))
    (let ((awc-research-notes-subdirectory "escape"))
      (should-error (awc-research-open-note "saved24:key") :type 'user-error))))

(ert-deftest awc-research-project-bibliography-stays-local ()
  (awc-research-test--fixture
    (with-temp-buffer
      (setq-local citar-bibliography '("/tmp/project-only.bib"))
      (awc-research-import-item "ABC12345")
      (should (equal citar-bibliography '("/tmp/project-only.bib")))
      (should (member (awc-research--bibliography) (default-value 'citar-bibliography))))))

(ert-deftest awc-research-missing-pdf-is-an-actionable-error ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (should-error (awc-research-open-pdf "saved24:key") :type 'user-error)))

(ert-deftest awc-research-api-403-cleans-response-buffer ()
  (let ((response (generate-new-buffer " *zotero-test*")))
    (with-current-buffer response
      (setq-local url-http-response-status 403)
      (insert "HTTP/1.1 403 Forbidden\r\n\r\nDisabled"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) response)))
      (should-error (awc-research--get "users/0/items") :type 'user-error))
    (should-not (buffer-live-p response))))

(ert-deftest awc-research-attachment-not-duplicated-and-new-note-has-no-device-path ()
  (awc-research-test--fixture
    (let ((pdf (expand-file-name "paper #1 é.pdf" root)))
      (with-temp-file pdf (insert "PDF fixture"))
      (setq awc-research-test--export
            (format "@article{saved24:key,title={A source},year={2024},file={PDF:%s:application/pdf}}" pdf))
      (awc-research-import-item "ABC12345")
      (should (equal (gethash "saved24:key" (citar-get-files '("saved24:key"))) (list pdf)))
      (should (string-match-p "paper%20%231%20%C3%A9.pdf" (awc-research--file-uri pdf)))
      (let ((text (with-current-buffer (awc-research-open-note "saved24:key") (buffer-string))))
        (should-not (string-match-p "file://\\|paper%20%231" text))
        (should (string-match-p "zotero://select/library/items/ABC12345" text))))))

(ert-deftest awc-research-import-preserves-unsaved-supplement-edits ()
  (awc-research-test--fixture
    (awc-research-import-item "ABC12345")
    (let* ((file (awc-research--bibliography))
           (original (awc-research-test--read file))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
      (should-error (awc-research--write-owned file "replacement") :type 'user-error)
      (should (buffer-modified-p buffer))
      (should (equal original (awc-research-test--read file))))))

(ert-deftest awc-research-vault-edits-have-central-recovery ()
  (awc-research-test--fixture
    (let* ((file (expand-file-name "existing.md" awc-research-vault-directory))
           (find-file-hook '(awc-research--protect-note)))
      (with-temp-file file (insert "Original note.\n"))
      (with-current-buffer (find-file-noselect file)
        (should make-backup-files)
        (should (file-in-directory-p buffer-auto-save-file-name awc-research-data-directory))
        (goto-char (point-max))
        (insert "Unsaved thought.\n")
        (do-auto-save t)
        (should (file-exists-p buffer-auto-save-file-name))
        (should (string-match-p "Unsaved thought" (awc-research-test--read buffer-auto-save-file-name)))
        (should (equal "Original note.\n" (awc-research-test--read file))))
      (with-temp-buffer
        (setq buffer-file-name primary)
        (let ((before make-backup-files))
          (awc-research--protect-note)
          (should (equal before make-backup-files)))))))

(provide 'awc-research-test)

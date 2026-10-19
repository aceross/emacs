;;; awc-bibliography-test.el --- Project snapshot preservation -*- lexical-binding: t; -*-
(require 'awc-research-test)
(require 'awc-bibliography)

(defmacro awc-bibliography-test--fixture (&rest body)
  (declare (indent 0))
  `(awc-research-test--fixture
    (let* ((target (expand-file-name "references.bib" root))
           (files (list target))
           (original "% Project annotation stays verbatim.\n@book{project-only, title={Unrelated}, year={1980}}\n"))
      (with-temp-file target (insert original))
      (awc-research-import-item "ABC12345")
      ,@body)))

(defun awc-bibliography-test--apply (plan)
  (with-temp-buffer
    (setq-local awc-bibliography--plan plan)
    (awc-bibliography-apply)))

(ert-deftest awc-bibliography-review-cancel-apply-and-idempotence ()
  (awc-bibliography-test--fixture
   (let* ((plan (awc-bibliography--build "saved24:key" target files root))
          (before (awc-bibliography--snapshot target))
          (review (save-window-excursion (awc-bibliography--review plan))))
     (should (equal original (awc-research-test--read target)))
     (with-current-buffer review
       (should (derived-mode-p 'diff-mode))
       (should buffer-read-only)
       (should (eq (key-binding (kbd "C-c C-c")) #'awc-bibliography-apply))
       (should (string-match-p "+@article{saved24:key," (buffer-string))))
     (kill-buffer review)
     (should (equal before (awc-bibliography--snapshot target)))
     (set-file-modes target #o644)
     (awc-bibliography-test--apply plan)
     (should (string-match-p (regexp-quote original) (awc-research-test--read target)))
     (should (= (file-modes target) #o644))
     (let* ((applied (awc-bibliography--snapshot target))
            (backups (directory-files awc-research-data-directory nil "bibliography-before-"))
            (again (awc-bibliography--build "saved24:key" target files root)))
       (should (equal (plist-get again :old) (plist-get again :new)))
       (should-not (awc-bibliography--review again))
       (should (equal applied (awc-bibliography--snapshot target)))
       (should (= 1 (length backups)))
       (should (equal original (awc-research-test--read (expand-file-name (car backups) awc-research-data-directory))))))))

(ert-deftest awc-bibliography-portable-copy-omits-attachment-paths ()
  (awc-bibliography-test--fixture
   (with-temp-file primary
     (insert "@article{local:key, title={Unicode é and C#}, year={2024}, file={/home/another-device/paper.pdf}, pdf={C:/PDF/paper.pdf}, local-url={file:///tmp/paper.pdf}, url={https://example.org/paper}}\n"))
   (let ((plan (awc-bibliography--build "local:key" target files root)))
     (should (string-match-p "Unicode é and C#" (plist-get plan :new)))
     (should (string-match-p "https://example.org/paper" (plist-get plan :new)))
     (should-not (string-match-p "/home/another-device\\|C:/PDF\\|file:///tmp" (plist-get plan :new))))))

(ert-deftest awc-bibliography-zotero-refresh-keeps-legacy-key-and-manual-fields ()
  (awc-research-test--fixture
   (with-temp-file primary
     (insert "@article{legacy:key,title={A source},year={2024},doi={10.1/source}}\n"))
   (awc-research-import-item "ABC12345" "groups/42")
   (let* ((target (expand-file-name "references.bib" root))
          (before (awc-research-test--read primary)))
     (with-temp-file target
       (insert "% Preserve unrelated text.\n@article{legacy:key,title={A source},year={2024},doi={10.1/source},annotation={My hand-edited interpretation},file={/old/device.pdf}}\n"))
     (setq awc-research-test--api-key "new-native-key"
           awc-research-test--export "@article{new-native-key,title={A corrected source é},year={2025},doi={10.1/source},file={/new/device.pdf}}")
     (let ((calls nil) (get (symbol-function 'awc-research--get)))
       (cl-letf (((symbol-function 'awc-research--get)
                  (lambda (path &optional raw) (push path calls) (funcall get path raw))))
         (let ((plan (awc-bibliography--build "legacy:key" target (list target) root t)))
           (should (equal (length calls) 2))
           (should (cl-every (lambda (path) (string-prefix-p "groups/42/" path)) calls))
           (should (string-match-p "@article{legacy:key," (plist-get plan :new)))
           (should (string-match-p "A corrected source é" (plist-get plan :new)))
           (should (string-match-p "My hand-edited interpretation" (plist-get plan :new)))
           (should-not (string-match-p "new-native-key\\|device.pdf" (plist-get plan :new)))
           (awc-bibliography-test--apply plan))))
     (should (equal before (awc-research-test--read primary))))))

(ert-deftest awc-bibliography-dependency-closure-and-cycles ()
  (awc-bibliography-test--fixture
   (with-temp-file primary
     (insert "@incollection{child,title={Chapter},crossref={parent},xdata={shared}}\n@book{parent,title={Collected volume},year={2024}}\n@xdata{shared,publisher={Press}}\n"))
   (let* ((plan (awc-bibliography--build "child" target files root))
          (records (awc-bibliography--records (plist-get plan :new) target)))
     (should (equal (sort (mapcar (lambda (r) (plist-get r :key)) records) #'string<)
                    '("child" "parent" "project-only" "shared"))))
   (with-temp-file primary
     (insert "@incollection{child,title={Chapter},crossref={parent}}\n@book{parent,title={Volume},crossref={child}}\n"))
   (should-error (awc-bibliography--build "child" target files root) :type 'user-error)
   (with-temp-file primary (insert "@incollection{child,title={Chapter},crossref={absent}}\n"))
   (should-error (awc-bibliography--build "child" target files root) :type 'user-error)
   (should (equal original (awc-research-test--read target)))))

(ert-deftest awc-bibliography-refuses-project-and-source-ambiguity ()
  (awc-bibliography-test--fixture
   (let ((other (expand-file-name "other.bib" root)))
     (with-temp-file other (insert "@book{project-only,title={Duplicate},year={1980}}\n"))
     (should-error (awc-bibliography--build "saved24:key" target (list target other) root) :type 'user-error))
   (with-temp-file primary
     (insert "@article{saved24:key,title={A source},year={2024}}\n"))
   (should-error (awc-bibliography--build "saved24:key" target files root) :type 'user-error)
   (should (equal original (awc-research-test--read target)))))

(ert-deftest awc-bibliography-refuses-identity-collisions-and-key-migration ()
  (awc-bibliography-test--fixture
   (dolist (entry '("@article{saved24:key,title={Another item},year={1999},doi={10.1/other}}\n"
                    "@article{established-key,title={A source},year={2024},doi={10.1/source}}\n"))
     (with-temp-file target (insert entry))
     (should-error (awc-bibliography--build "saved24:key" target files root) :type 'user-error)
     (should (equal entry (awc-research-test--read target))))))

(ert-deftest awc-bibliography-refuses-malformed-and-unresolved-macros ()
  (awc-bibliography-test--fixture
   (dolist (entry '("@article{bad,title={Unclosed}\n"
                    "@article{bad,title=unknownmacro}\n"
                    "@article{bad,title={Prefix} # unknownmacro}\n"))
     (with-temp-file primary (insert entry))
     (should-error (awc-bibliography--build "bad" target files root) :type 'user-error))
   (should (equal original (awc-research-test--read target)))))

(ert-deftest awc-bibliography-refuses-generated-missing-and-escaping-targets ()
  (awc-bibliography-test--fixture
   (with-temp-file target (insert "% DO NOT EDIT: automatic export\n" original))
   (should-error (awc-bibliography--build "saved24:key" target files root) :type 'user-error)
   (should-error (awc-bibliography--build "saved24:key" target nil root) :type 'user-error)
   (delete-file target)
   (make-symbolic-link primary target)
   (should-error (awc-bibliography--build "saved24:key" target files root) :type 'user-error)
   (delete-file target)
   (should-error (awc-bibliography--build "saved24:key" target files root) :type 'user-error)))

(ert-deftest awc-bibliography-review-refuses-stale-inputs-and-unsaved-edits ()
  (awc-bibliography-test--fixture
   (let* ((config (expand-file-name "_metadata.yml" root))
          (plan (awc-bibliography--build "saved24:key" target files root nil (list config))))
     (with-temp-file config (insert "bibliography: different.bib\n"))
     (should-error (awc-bibliography-test--apply plan) :type 'user-error)
     (should (equal original (awc-research-test--read target))))
   (let ((plan (awc-bibliography--build "saved24:key" target files root)))
     (with-temp-file primary (insert "@book{changed,title={Changed source},year={2000}}\n"))
     (should-error (awc-bibliography-test--apply plan) :type 'user-error))
   (let* ((plan (awc-bibliography--build "saved24:key" target files root))
          (buffer (find-file-noselect target)))
     (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved manual addition"))
     (should-error (awc-bibliography-test--apply plan) :type 'user-error)
     (should (equal original (awc-research-test--read target)))
     (should (buffer-modified-p buffer)))))

(ert-deftest awc-bibliography-rename-failure-leaves-target-and-no-lock ()
  (awc-bibliography-test--fixture
   (let ((plan (awc-bibliography--build "saved24:key" target files root)))
     (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (signal 'file-error '("Fixture rename failure")))))
       (should-error (awc-bibliography-test--apply plan) :type 'file-error))
     (should-not (file-locked-p target))
     (should-not (directory-files root nil "references.bib.awc-"))
     (should (equal original (awc-research-test--read target))))))

(ert-deftest awc-bibliography-keeps-comments-strings-and-crlf ()
  (awc-bibliography-test--fixture
   (let ((text "@comment{Preserve text including @book{not-a-record}}\n@string{press = {A Press}}\n@book{manual,title={Keep},publisher=press}\n"))
     (let ((coding-system-for-write 'utf-8-dos))
       (write-region text nil target nil 'silent))
     (let ((plan (awc-bibliography--build "saved24:key" target files root)))
       (awc-bibliography-test--apply plan)
       (should (string-match-p (regexp-quote text) (awc-research-test--read target)))
       (with-temp-buffer
         (insert-file-contents-literally target)
         (should (string-match-p "manual,title={Keep},publisher=press}\r\n" (buffer-string)))
         (goto-char (point-min))
         (should-not (re-search-forward "[^\r]\n" nil t)))))))

(ert-deftest awc-bibliography-stale-target-and-zotero-association-do-not-apply ()
  (awc-bibliography-test--fixture
   (let ((plan (awc-bibliography--build "saved24:key" target files root t)))
     (with-temp-file (awc-research--links-file) (insert "{}\n"))
     (should-error (awc-bibliography-test--apply plan) :type 'user-error)
     (should (equal original (awc-research-test--read target))))
   (let ((plan (awc-bibliography--build "saved24:key" target files root)))
     (with-temp-file target (insert original "% Changed by another editor.\n"))
     (let ((changed (awc-research-test--read target)))
       (should-error (awc-bibliography-test--apply plan) :type 'user-error)
       (should (equal changed (awc-research-test--read target)))))))

(ert-deftest awc-bibliography-unavailable-zotero-and-export-mismatch-do-not-write ()
  (awc-bibliography-test--fixture
   (cl-letf (((symbol-function 'awc-research--get) (lambda (&rest _) (user-error "Fixture API unavailable"))))
     (should-error (awc-bibliography--build "saved24:key" target files root t) :type 'user-error))
   (setq awc-research-test--api-key "different-export-key")
   (should-error (awc-bibliography--build "saved24:key" target files root t) :type 'user-error)
   (should (equal original (awc-research-test--read target)))))

(ert-deftest awc-bibliography-diff-failure-cleans-review-buffer ()
  (awc-bibliography-test--fixture
    (let ((plan (awc-bibliography--build "saved24:key" target files root))
          (before (cl-remove-if-not (lambda (buffer) (string-prefix-p "*Project bibliography review*" (buffer-name buffer))) (buffer-list))))
      (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (signal 'file-missing '("Fixture missing diff")))))
        (should-error (awc-bibliography--review plan) :type 'file-missing))
      (should (equal before (cl-remove-if-not (lambda (buffer) (string-prefix-p "*Project bibliography review*" (buffer-name buffer))) (buffer-list)))))))

(provide 'awc-bibliography-test)
;;; awc-bibliography-test.el ends here

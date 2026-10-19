;;; quarto-assistance-test.el --- Authoring and safe mapped edits -*- lexical-binding: t; -*-
(require 'ert)
(require 'quarto-mode)

(ert-deftest quarto-assistance-waits-for-initialized-workspace ()
  (require 'lsp-mode)
  (let ((source (generate-new-buffer " *starting source*"))
        (projection (generate-new-buffer " *starting projection*"))
        (workspace (make-lsp--workspace :status 'starting))
        (quarto-lsp-auto-start nil))
    (unwind-protect
        (with-current-buffer source
          (insert "```{python}\nanswer = 42\n```\n")
          (let ((data (quarto-document-projection "python")))
            (with-current-buffer projection
              (delay-mode-hooks (python-mode))
              (insert (car data))
              (setq-local quarto-document--owner source quarto-lsp--map (cdr data)
                          lsp--buffer-workspaces (list workspace))))
          (setq quarto-lsp--documents (list (cons "python" projection)))
          (goto-char (point-min)) (search-forward "answer")
          (should-not (quarto-lsp--at-point))
          (should-not (quarto-lsp-eldoc #'ignore))
          (setf (lsp--workspace-status workspace) 'initialized)
          (should (eq (car (quarto-lsp--at-point)) projection)))
      (kill-buffer projection) (kill-buffer source))))

(ert-deftest quarto-context-missing-executable-does-not-leak-buffers ()
  (let ((directory (make-temp-file "quarto-missing-tool-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "report.qmd" directory)
                default-directory (file-name-as-directory directory))
          (write-region "---\ntitle: Fixture\n---\n" nil buffer-file-name nil 'silent)
          (let ((exec-path nil) (before (buffer-list)))
            (dotimes (_ 3)
              (should-error (quarto-context-resolve #'ignore) :type 'user-error))
            (should-not (cl-set-difference (buffer-list) before))))
      (delete-directory directory t))))

(ert-deftest quarto-assistance-rejects-edits-outside-code ()
  (let ((source (generate-new-buffer " *source*")) (projection (generate-new-buffer " *projection*")))
    (unwind-protect
        (with-current-buffer source
          (insert "# Prose 😀\n```{python}\nx = 1\n```\nProse\n```{python}\nprint(x)\n```\n")
          (let ((data (quarto-document-projection "python")))
            (with-current-buffer projection
              (insert (car data))
              (setq quarto-lsp--map (cdr data) quarto-document--owner source)
              (let ((edit (json-parse-string "{\"range\":{\"start\":{\"line\":2,\"character\":0},\"end\":{\"line\":2,\"character\":1}},\"newText\":\"value\"}")))
                (should (equal "value" (nth 2 (quarto-lsp--safe-edit projection edit)))))
              (dolist (range '("0,0,0,1" "2,0,6,1" "1,0,2,1"))
                (let* ((numbers (mapcar #'string-to-number (split-string range ",")))
                       (edit (json-parse-string
                              (format "{\"range\":{\"start\":{\"line\":%d,\"character\":%d},\"end\":{\"line\":%d,\"character\":%d}},\"newText\":\"bad\"}"
                                      (nth 0 numbers) (nth 1 numbers) (nth 2 numbers) (nth 3 numbers)))))
                  (should-error (quarto-lsp--safe-edit projection edit) :type 'user-error))))))
      (kill-buffer projection) (kill-buffer source))))

(ert-deftest quarto-assistance-utf16-mapping ()
  (with-temp-buffer
    (insert "x = '😀λ'; y = 1\n")
    (goto-char 1) (search-forward "; ")
    (let* ((pos (point)) (encoded (quarto-lsp--position pos))
           (decoded (json-parse-string (json-serialize encoded))))
      (should (= (plist-get encoded :character) pos))
      (should (= pos (quarto-lsp--point decoded))))))

(ert-deftest quarto-assistance-bibliography-is-local ()
  (let ((directory (make-temp-file "quarto-bib-" t)) (citar-bibliography '("personal.bib")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "paper.qmd" directory))
          (with-temp-file (expand-file-name "refs.bib" directory) (insert "@article{smith2024, title={Example}}\n"))
          (let ((metadata (make-hash-table :test #'equal)))
            (puthash "bibliography" "refs.bib" metadata)
            (quarto-authoring-bibliographies (list :metadata metadata :file buffer-file-name :project-directory directory)))
          (should (local-variable-p 'citar-bibliography))
          (should (equal quarto-authoring--citations '("smith2024")))
          (insert "# Section {#sec-one}\n@sm")
          (should (member "smith2024" (nth 2 (quarto-reference-completion-at-point))))
          (should (equal (caar (quarto-imenu-index)) "# Section {#sec-one}")))
      (delete-directory directory t))
    (should (equal citar-bibliography '("personal.bib")))))

(ert-deftest quarto-assistance-preserves-containers-in-multiline-edits ()
  (let ((source (generate-new-buffer " *container source*"))
        (projection (generate-new-buffer " *container projection*")))
    (unwind-protect
        (with-current-buffer source
          (insert "> - ~~~{python}\n>   x = 1\n>   ~~~\n")
          (let ((data (quarto-document-projection "python")))
            (with-current-buffer projection
              (insert (car data))
              (setq quarto-lsp--map (cdr data) quarto-document--owner source)
              (let ((edit (json-parse-string "{\"range\":{\"start\":{\"line\":1,\"character\":0},\"end\":{\"line\":1,\"character\":5}},\"newText\":\"x = (1 +\\n     2)\"}")))
                (quarto-lsp--apply (list (quarto-lsp--safe-edit projection edit))))))
          (should (equal (buffer-string) "> - ~~~{python}\n>   x = (1 +\n>        2)\n>   ~~~\n"))
          (should (equal (quarto-chunk-code (car (quarto-document-chunks (quarto-document-model)))) "x = (1 +\n     2)\n")))
      (kill-buffer projection) (kill-buffer source))))

(provide 'quarto-assistance-test)

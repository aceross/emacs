;;; quarto-research-test.el --- Readiness and research authoring -*- lexical-binding: t; -*-
(require 'ert)
(require 'quarto-mode)

(ert-deftest quarto-references-rename-uses-exact-source-ranges ()
  (with-temp-buffer
    (insert "![A λ figure](plot.png){#fig-old}\nSee @fig-old, @fig-old-long and @fig-old.\n`😀 @fig-old` and <!-- λ @fig-old --> @fig-old\n<!--\n```{r}\n@fig-old\n```\n-->\n```r\n@fig-old\n```\n")
    (goto-char 1) (search-forward "#fig-old")
    (should (equal (cdr (assoc "fig-old" (quarto-reference-records))) "A λ figure"))
    (quarto-rename "fig-new")
    (should (string-match-p "{#fig-new}" (buffer-string)))
    (should (string-match-p "See @fig-new, @fig-old-long and @fig-new\\." (buffer-string)))
    (should (string-match-p "`😀 @fig-old` and <!-- λ @fig-old --> @fig-new" (buffer-string)))
    (should (string-match-p "```r\n@fig-old\n```" (buffer-string)))
    (should (= 3 (length (cl-remove-if-not (lambda (ref) (equal "fig-new" (car ref)))
                                         (quarto-document-references (quarto-document-model))))))))

(ert-deftest quarto-references-chunk-labels-and-captions ()
  (dolist (fixture '("~~~~{r fig-old}\n#| fig-cap: A plot\nplot(1)\n~~~~\nSee @fig-old.\n"
                     "> - ~~~~{python}\n>   #| label: 'fig-old' # keep comment\n>   #| fig-cap: A plot\n>   print(1)\n>   ~~~~\nSee @fig-old.\n"))
    (with-temp-buffer
      (insert fixture) (goto-char 1) (search-forward "fig-old")
      (should (equal "A plot" (cdr (assoc "fig-old" (quarto-reference-records)))))
      (quarto-rename-label "fig-new")
      (should-not (string-match-p "fig-old" (buffer-string)))
      (should (equal "fig-new" (quarto-chunk-label (car (quarto-document-chunks (quarto-document-model)))))))))

(ert-deftest quarto-references-rename-also-updates-caption-references ()
  (with-temp-buffer
    (insert "![Original](plot.png){#fig-old}\n```{r}\n#| label: fig-other\n#| fig-cap: 'Compared with @fig-old.'\nplot(1)\n```\n")
    (goto-char 1) (search-forward "#fig-old")
    (quarto-rename-label "fig-new")
    (should (string-match-p "Compared with @fig-new\\." (buffer-string)))
    (goto-char 1) (search-forward "Compared with @fig-")
    (should-not (quarto-yaml-completion-at-point))
    (should (member "fig-new" (nth 2 (quarto-reference-completion-at-point))))))

(ert-deftest quarto-references-multiline-captions-retain-source-positions ()
  (with-temp-buffer
    (insert "![Original](plot.png){#fig-old}\n> - ~~~~{r}\n>   #| label: fig-other\n>   #| fig-cap: >\n>   #|   Measured λ values,\n>   #|   compared with @fig-old.\n>   #| tbl-cap: 'Another @fig-old.' # Keep @fig-old literal\n>   plot(1)\n>   ~~~~\n")
    (should (equal "Measured λ values, compared with @fig-old."
                   (cdr (assoc "fig-other" (quarto-reference-records)))))
    (goto-char 1) (search-forward "#fig-old")
    (quarto-rename-label "fig-new")
    (should (string-match-p "compared with @fig-new\\." (buffer-string)))
    (should (string-match-p "'Another @fig-new\\.' # Keep @fig-old literal" (buffer-string)))))

(ert-deftest quarto-references-rejects-ambiguous-colliding-and-partial-renames ()
  (dolist (fixture '("# One {#sec-one}\n# Two {#sec-one}\n@sec-one\n"
                     "```{r sec-one}\n#| label: sec-one\n```\n@sec-one\n"))
    (with-temp-buffer
      (insert fixture) (search-backward "@sec-one") (forward-char 2)
      (should-error (quarto-rename-label "sec-two") :type 'user-error)
      (should (equal fixture (buffer-string)))))
  (with-temp-buffer
    (insert "# One {#sec-one}\n# Two {#sec-two}\n@sec-one\n")
    (goto-char 1) (search-forward "#sec-one")
    (dolist (name '("sec-two" "fig-one" "sec-bad space"))
      (should-error (quarto-rename-label name) :type 'user-error))
    (let ((quarto-authoring--citations '("sec-three")))
      (should-error (quarto-rename-label "sec-three") :type 'user-error))
    (let ((before (buffer-string)))
      (put-text-property 1 18 'read-only t)
      (should-error (quarto-rename-label "sec-three"))
      (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest quarto-references-completion-is-confined-to-real-references ()
  (with-temp-buffer
    (insert "# Section {#sec-one}\n@sec-o\n`@sec-o`\n<!-- @sec-o -->\nname@sec-o\n[link](path/@sec-o)\nhttps://example.org/@sec-o\n@\n")
    (goto-char 1) (search-forward "\n@sec-o")
    (should (member "sec-one" (nth 2 (quarto-reference-completion-at-point))))
    (dotimes (_ 5) (search-forward "@sec-o") (should-not (quarto-reference-completion-at-point)))
    (search-forward "\n@")
    (should (member "sec-one" (nth 2 (quarto-reference-completion-at-point))))))

(ert-deftest quarto-references-bibliography-records-and-external-replacement ()
  (let ((directory (make-temp-file "quarto-references-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "paper.qmd" directory))
          (let ((file (expand-file-name "refs.bib" directory))
                (metadata (make-hash-table :test #'equal)))
            (with-temp-file file
              (insert "@article{smith2024,\n author={Smith, Jane and Doe, John},\n year={2024},\n title={A {Nested} Title}\n}\n"))
            (puthash "bibliography" "refs.bib" metadata)
            (quarto-authoring-bibliographies (list :metadata metadata :file buffer-file-name :project-directory directory))
            (should (equal "Smith, Jane and Doe, John · 2024 · A Nested Title"
                           (cdr (assoc "smith2024" (quarto-reference-records)))))
            (should (= 1 (length (xref-backend-definitions 'quarto "smith2024"))))
            (quarto-references-setup)
            (let ((replacement (expand-file-name "new.bib" directory)))
              (with-temp-file replacement (insert "@book{jones, title={Updated}, year={2025}}\n"))
              (rename-file replacement file t))
            (quarto-references--poll)
            (should (equal '("jones") quarto-authoring--citations))
            (delete-file file) (quarto-references--poll)
            (should quarto-references-error)
            (should (equal '("jones") quarto-authoring--citations))
            (quarto-references-close)
            (should-not (memq (current-buffer) quarto-references--sources))))
      (delete-directory directory t))))

(provide 'quarto-research-test)

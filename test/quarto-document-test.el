;;; quarto-document-test.el --- Source model checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'quarto-mode)

(ert-deftest quarto-document-fences-and-containers ()
  (with-temp-buffer
    (insert "---\ntitle: Example\n---\n# Heading {#sec-one}\n\n::: {.callout-note}\n~~~~{r}\n#| label: fig-plot\nx <- 1\n~~~\n~~~~\n:::\n\n- List\n\n  ````{python}\n  #| eval: false\n  x = 2\n  ```\n  ````\n\n```{rust}\nlet x = 1;\n```\n```r\nx <- 9\n```\n~~~{r}\n~~~\n")
    (let* ((model (quarto-document-model)) (chunks (quarto-document-chunks model)))
      (should (= 5 (length chunks)))
      (should (equal (mapcar #'quarto-chunk-language chunks) '("r" "python" "rust" nil "r")))
      (should (equal (quarto-chunk-label (car chunks)) "fig-plot"))
      (should (eq :false (cdr (assoc "eval" (quarto-chunk-options (nth 1 chunks))))))
      (should-not (quarto-chunk-executable (nth 2 chunks)))
      (should-not (quarto-chunk-executable (nth 3 chunks)))
      (should (equal (quarto-chunk-code (nth 1 chunks)) "#| eval: false\nx = 2\n```\n"))
      (should (equal (quarto-chunk-code (nth 4 chunks)) ""))
      (should-not (quarto-document-diagnostics model)))))

(ert-deftest quarto-document-malformed-and-references ()
  (with-temp-buffer
    (insert "# A {#sec-a}\n# B {#sec-a}\nSee @sec-missing and @smith2024.\n```{r}\nx <- 1\n~~~\n")
    (let ((model (quarto-document-model)))
      (should (= 3 (length (quarto-document-diagnostics model))))
      (should-not (quarto-chunk-executable (car (quarto-document-chunks model)))))))

(ert-deftest quarto-document-projection-unicode-and-revisions ()
  (with-temp-buffer
    (insert "Prose λ\n~~~{python}\nα = 42\n~~~\nText\n  ```{python}\n  print(α)\n  ```\n")
    (let* ((model (quarto-document-model)) (projection (quarto-document-projection "python"))
           (mapping (cdr projection)) (entry (cadr mapping)))
      (should (equal (car projection) "\n\nα = 42\n\n\n\nprint(α)\n\n"))
      (should (= (cadr entry) (quarto-map-position (car entry) mapping t)))
      (should (= (car entry) (quarto-map-position (cadr entry) mapping)))
      (should-not (quarto-map-position 1 mapping t))
      (goto-char 1) (insert "New line\n")
      (should-not (eq model (quarto-document-model)))
      (should (equal (mapcar #'quarto-chunk-id (quarto-document-chunks model))
                     (mapcar #'quarto-chunk-id (quarto-document-chunks (quarto-document-model))))))))

(ert-deftest quarto-document-polymode-uses-index ()
  (with-temp-buffer
    (insert "```{rust}\nlet value = 1;\n```\n\n~~~~{r}\nx <- 1\n~~~~\n\n  ````{python}\n  value = 2\n  ````\n")
    (poly-quarto-mode)
    (goto-char 1) (forward-line 1)
    (should-not (eq (car (pm-innermost-span)) 'body))
    (forward-line 4)
    (should (eq (car (pm-innermost-span)) 'body))
    (should (eq 'ess-r-mode (buffer-local-value 'major-mode (pm-span-buffer (pm-innermost-span)))))
    (forward-line 4)
    (should (eq 'python-mode (buffer-local-value 'major-mode (pm-span-buffer (pm-innermost-span)))))))

(ert-deftest quarto-document-does-not-change-standalone-indentation ()
  (with-temp-buffer
    (python-mode)
    (setq-local electric-indent-inhibit 'untouched)
    (quarto-inner-setup)
    (should (eq electric-indent-inhibit 'untouched))))

(ert-deftest quarto-document-literal-references-and-container-prefixes ()
  (with-temp-buffer
    (insert "# Section {#sec-one}\nSee @sec-one. Literal `@fig-missing` and ``@tbl-`example``.\n> - ~~~{python}\n>   value = 2\n>   ~~~\n")
    (should-not (quarto-document-diagnostics (quarto-document-model)))
    (should (equal (quarto-chunk-code (car (quarto-document-chunks (quarto-document-model)))) "value = 2\n"))))

(provide 'quarto-document-test)

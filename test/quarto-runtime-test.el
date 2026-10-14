;;; quarto-runtime-test.el --- Queue and result ownership -*- lexical-binding: t; -*-
(require 'ert)
(require 'quarto-runtime)

(ert-deftest quarto-runtime-events-cannot-cross-documents-or-requests ()
  (with-temp-buffer
    (let ((request (make-quarto-request :id 1 :document "first" :revision 3 :state 'running)))
      (setq quarto-runtime--current request)
      (quarto-runtime--event (current-buffer) (json-parse-string "{\"type\":\"text\",\"id\":1,\"document\":\"second\",\"revision\":3,\"text\":\"wrong\"}"))
      (should-not (quarto-request-events request))
      (quarto-runtime--event (current-buffer) (json-parse-string "{\"type\":\"text\",\"id\":1,\"document\":\"first\",\"revision\":2,\"text\":\"old\"}"))
      (should-not (quarto-request-events request))
      (quarto-runtime--event (current-buffer) (json-parse-string "{\"type\":\"text\",\"id\":1,\"document\":\"first\",\"revision\":3,\"text\":\"right\"}"))
      (should (= 1 (length (quarto-request-events request)))))))

(ert-deftest quarto-runtime-bulk-skips-eval-false-but-explicit-does-not ()
  (with-temp-buffer
    (insert "```{r}\n#| eval: false\nx <- 1\n```\n```{python}\nx = 1\n```\n")
    (let (selected)
      (cl-letf (((symbol-function 'quarto-execution-request) (lambda (chunks &rest _) (setq selected chunks))))
        (quarto-run-all) (should (= 1 (length selected)))
        (should (equal "python" (quarto-chunk-language (car selected))))
        (goto-char 1) (quarto-run-chunk)
        (should (equal "r" (quarto-chunk-language (car selected))))))))

(ert-deftest quarto-runtime-results-preserve-focus-and-reuse-window ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *results source*")))
      (unwind-protect
          (progn (switch-to-buffer source)
                 (let ((window (selected-window)))
                   (quarto-results-show)
                   (let ((pane quarto-runtime--pane) (count (length (window-list))))
                     (quarto-results-show)
                     (should (eq pane quarto-runtime--pane))
                     (should (= count (length (window-list))))
                     (should (eq window (selected-window))))))
        (with-current-buffer source (when (buffer-live-p quarto-runtime--pane) (kill-buffer quarto-runtime--pane)))
        (kill-buffer source)))))

(ert-deftest quarto-runtime-statement-navigation-is-confined-to-chunks ()
  (with-temp-buffer
    (insert "Prose\n```{python}\ndef calculate(x):\n    return x + 1\n\nanswer = (1 +\n          2)\n```\n~~~{r}\nvalue <- sum(\n  1, 2\n)\n~~~\n")
    (poly-quarto-mode)
    (goto-char 1) (search-forward "def calc")
    (let ((bounds (quarto-statement-bounds)))
      (should (string-match-p (regexp-quote "return x + 1") (buffer-substring-no-properties (car bounds) (cadr bounds)))))
    (search-forward "answer = (1")
    (let ((bounds (quarto-statement-bounds)))
      (should (string-match-p "2)" (buffer-substring-no-properties (car bounds) (cadr bounds)))))
    (search-forward "sum(")
    (let ((bounds (quarto-statement-bounds)))
      (should (<= (quarto-chunk-head-end (quarto-chunk-at)) (car bounds)))
      (should (>= (quarto-chunk-body-end (quarto-chunk-at)) (cadr bounds))))))

(provide 'quarto-runtime-test)

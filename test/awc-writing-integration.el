;;; awc-writing-integration.el --- Real Vale and installed modes -*- lexical-binding: t; -*-
(load (expand-file-name "quarto-test-setup.el" (file-name-directory load-file-name)) nil t)
(setenv "AWC_SAFETY_TEST_ROOT" (expand-file-name "configured/" (getenv "AWC_WRITING_ROOT")))
(load (expand-file-name "safety-startup-check.el" (file-name-directory load-file-name)) nil t)
(require 'awc-writing)
(require 'init-workflow)
(require 'writegood-mode)
(defvar awc-writing-test-root (getenv "AWC_WRITING_ROOT"))
(unless (and awc-writing-test-root (file-in-directory-p awc-writing-test-root "/tmp/"))
  (error "Writing acceptance requires disposable fixtures"))
(setq awc-writing-program (getenv "AWC_WRITING_VALE"))
(defvar awc-writing-test-project (expand-file-name "project with spaces é/" awc-writing-test-root))
(defvar awc-writing-test-expected
  (json-parse-string (with-temp-buffer
                       (insert-file-contents (expand-file-name "expected.json" awc-writing-test-root))
                       (buffer-string)) :object-type 'alist :array-type 'list))
(defvar awc-writing-test-writegood nil)

(defun awc-writing-integration-wait (source)
  (quarto-test-wait (lambda () (not (buffer-local-value 'awc-writing--process source))) "Vale result" 20))

(defun awc-writing-integration-navigate (source expected &optional graphical)
  "Exercise real keyboard jumps and inspect the source highlight for EXPECTED."
  (let ((results (buffer-local-value 'awc-writing--results source))
        (first t))
    (switch-to-buffer results)
    (goto-char (point-min))
    (setq next-error-last-buffer results)
    (dolist (finding expected)
      (if first
          (progn (execute-kbd-macro (kbd "M-g n")) (setq first nil))
        (execute-kbd-macro (kbd "M-g n")))
      (should (eq (current-buffer) source))
      (pcase-let ((`(,line ,start ,end ,_rule ,match) finding))
        (should (= (line-number-at-pos) line))
        (should (= (- (point) (line-beginning-position)) (1- start)))
        (should (equal (buffer-substring-no-properties (point) (+ (point) (length match))) match))
        (should (overlayp compilation-highlight-overlay))
        (should (equal (buffer-substring-no-properties (overlay-start compilation-highlight-overlay)
                                                       (overlay-end compilation-highlight-overlay)) match))
        (should (= (- (overlay-end compilation-highlight-overlay) (overlay-start compilation-highlight-overlay)) (1+ (- end start)))))
      (when graphical (redisplay t)))
    (execute-kbd-macro (kbd "M-g p"))
    (should (= (line-number-at-pos) (caar (last expected 2))))
    ;; RET is also the actual result-buffer binding and follows its exact range.
    (switch-to-buffer results)
    (goto-char (point-min))
    (search-forward ": error:")
    (beginning-of-line)
    (execute-kbd-macro (kbd "RET"))
    (should (eq (current-buffer) source))))

(dolist (entry awc-writing-test-expected)
  (let* ((name (symbol-name (car entry)))
         (file (expand-file-name name awc-writing-test-project))
         (source (find-file-noselect file))
         (located (cl-remove-if (lambda (case) (equal (car (last case)) "ABC")) (cdr entry))))
    (switch-to-buffer source)
    (delay-mode-hooks
      (funcall (cond ((string-suffix-p ".org" name) #'org-mode)
                     ((string-suffix-p ".qmd" name) #'poly-quarto-mode)
                     (t #'markdown-mode))))
    ;; Measure the installed alternative's actual regex candidates on the
    ;; unprojected source, recording locations for manual noise classification.
    (let ((case-fold-search t) candidates)
      (dolist (pair `((weasel . ,(writegood-weasels-font-lock-keywords-regexp))
                      (passive . ,(writegood-passive-voice-font-lock-keywords-regexp))
                      (duplicate . ,writegood-duplicates-font-lock-keywords-regexp)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (cdr pair) nil t)
            (push (list (symbol-name (car pair)) (line-number-at-pos (match-beginning 0))
                        (match-string-no-properties 0)) candidates))))
      (push (cons name (vconcat (nreverse candidates))) awc-writing-test-writegood))
    (execute-kbd-macro (kbd "C-c w W c"))
    (awc-writing-integration-wait source)
    (should (string-match-p (format "Writing QA: %d findings" (length located))
                           (with-current-buffer (buffer-local-value 'awc-writing--results source) (buffer-string))))
    (awc-writing-integration-navigate source located)
    (princ (format "PASS mode, menu, M-g n and RET: %s; %d exact source ranges\n" name (length located)))))

(with-temp-file (expand-file-name "writegood.json" awc-writing-test-root)
  (insert (json-encode awc-writing-test-writegood)))

;; Verify path-specific rules through --path, a project dictionary and an
;; unsaved snapshot.  The file on disk must remain byte-for-byte unchanged.
(let* ((file (expand-file-name "chapters/relaxed.md" awc-writing-test-project))
       (source (find-file-noselect file))
       (original (with-temp-buffer (insert-file-contents file) (buffer-string))))
  (with-current-buffer source
    (goto-char (point-max)) (insert "\nFIXME\n")
    (awc-writing-check)
    (awc-writing-integration-wait source)
    (let ((report (with-current-buffer awc-writing--results (buffer-string))))
      (should (string-match-p "2 findings" report))
      (should-not (string-match-p "Academic.Wordiness\\|Academic.Spelling" report)))
    (should (equal original (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (buffer-modified-p))
    (set-buffer-modified-p nil)))

;; A missing ruleset/dictionary and an empty configuration must diagnose a
;; failed check rather than present a clean manuscript.
(let* ((file (expand-file-name "broken.md" awc-writing-test-root))
       (source (find-file-noselect file)))
  (with-current-buffer source
    (insert "TODO\n")
    (with-temp-file (expand-file-name ".vale.ini" awc-writing-test-root)
      (insert "[*.md]\nBasedOnStyles = MissingStyle\n"))
    (awc-writing-check)
    (awc-writing-integration-wait source)
    (should (with-current-buffer awc-writing--results (string-match-p "Writing QA failed" (buffer-string))))
    (with-temp-file (expand-file-name ".vale.ini" awc-writing-test-root) (insert "[*.md]\n"))
    (awc-writing-check)
    (awc-writing-integration-wait source)
    (should (with-current-buffer awc-writing--results
              (string-match-p "Rules may be absent, disabled" (buffer-string))))
    (set-buffer-modified-p nil)))

(let* ((dict (expand-file-name "styles/config/dictionaries/en_GB.dic" awc-writing-test-project))
       (source (find-file-noselect (expand-file-name "draft.md" awc-writing-test-project))))
  (rename-file dict (concat dict ".hidden"))
  (unwind-protect
      (with-current-buffer source
        (awc-writing-check)
        (awc-writing-integration-wait source)
        (should (with-current-buffer awc-writing--results (string-match-p "Writing QA failed" (buffer-string)))))
    (rename-file (concat dict ".hidden") dict)))

(princ "PASS project path overrides, dictionary/inline exceptions, unsaved text, empty/missing rules and dictionary failure\n")
;;; awc-writing-integration.el ends here

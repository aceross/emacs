;;; run-safety-tests.el --- Built-in-only safety checks -*- lexical-binding: t; -*-
;; emacs -Q --batch -l test/run-safety-tests.el
(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "site-elisp" root))
  (add-to-list 'load-path (expand-file-name "test" root)))
(require 'awc-files-test)
(ert-run-tests-batch-and-exit)

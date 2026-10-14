;;; run-tests.el --- Run tests with existing packages, never install -*- lexical-binding: t; -*-
;; emacs-xwidgets -Q --batch -l ~/.emacs.d/test/run-tests.el
(let* ((root (file-name-directory (directory-file-name
                                   (file-name-directory (or load-file-name buffer-file-name)))))
       (build (expand-file-name "straight/build" root)))
  (dolist (directory (directory-files build t "^[^.]") )
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (add-to-list 'load-path (expand-file-name "site-elisp" root))
  (add-to-list 'load-path (expand-file-name "test" root)))
(require 'ess-site)
(require 'quarto-preview-test)
(require 'awc-project-test)
(require 'quarto-edit-test)
(require 'awc-r-test)
(require 'awc-r-lsp-test)
(ert-run-tests-batch-and-exit)

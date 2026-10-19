;;; bibliography-graphical-session-check.el --- Private GUI acceptance -*- lexical-binding: t; -*-
(defvar awc-p1a-gui-repo
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "test/" awc-p1a-gui-repo))
(require 'awc-bibliography-test)
(setq quarto-lsp-auto-start nil)
(defun awc-p1a-step (label)
  (write-region (concat label "\n") nil
                (expand-file-name "progress.txt" awc-reliability-root) t 'silent))
(defun awc-p1a-gui-finish ()
  (condition-case err
      (progn
        (awc-research-test--fixture
          (let* ((file (expand-file-name "paper.qmd" root))
                 (target (expand-file-name "references.bib" root))
                 (default-directory root))
            (setq awc-research-test--export "@article{saved24:key,title={Review of evidence é},author={Person, Alice},year={2024},doi={10.1/source},file={/other-device/paper.pdf}}")
            (awc-p1a-step "before import")
            (awc-research-import-item "ABC12345")
            (awc-p1a-step "after import")
            (with-temp-file (expand-file-name "_quarto.yml" root)
              (insert "project:\n  type: default\n"))
            (with-temp-file target (insert "% Manually reviewed bibliography.\n"))
            (with-temp-file file
              (insert "---\nengine: markdown\nbibliography: references.bib\n---\n\nA source.\n"))
            (switch-to-buffer (find-file-noselect file))
            (awc-p1a-step (format "before menu transient=%S" (symbol-file (quote transient-setup))))
            (execute-kbd-macro (kbd "C-c w n"))
            (awc-p1a-step "after menu")
            (should (eq (key-binding (kbd "b")) #'awc-bibliography-prepare))
            (should (eq (key-binding (kbd "r")) #'awc-bibliography-refresh))
            (execute-kbd-macro (kbd "C-q"))
            (awc-p1a-step "after quit")
            (let ((review (awc-bibliography-prepare "saved24:key")))
              (awc-p1a-step "after prepare")
              (should (eq review (window-buffer (selected-window))))
              (redisplay t)
              (awc-p1a-step "after redisplay")
              (let ((coding-system-for-write 'no-conversion))
                (write-region (x-export-frames nil 'png) nil
                              (expand-file-name "review.png" awc-reliability-root) nil 'silent))
              (awc-p1a-step "after export")
              (execute-kbd-macro (kbd "C-c C-c"))
              (awc-p1a-step "after apply")
              (should (string-match-p "@article{saved24:key," (awc-research-test--read target)))
              (should-not awc-bibliography--plan)
              (kill-buffer review))))
        (with-temp-file (expand-file-name "gui-result.txt" awc-reliability-root)
          (insert (format "PASS: %s %S; actual research menu, visible diff, keyboard apply\n" emacs-version window-system)))
        (awc-reliability-finish))
    (error
     (with-temp-file (expand-file-name "gui-result.txt" awc-reliability-root)
       (insert (format "FAIL: %S\n" err)))
     (kill-emacs 1))))
(add-hook 'emacs-startup-hook (lambda () (run-at-time 4 nil #'awc-p1a-gui-finish)) t)

;;; bibliography-graphical-session-check.el ends here

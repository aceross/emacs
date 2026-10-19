;;; awc-writing-gui-check.el --- Private normal Wayland acceptance -*- lexical-binding: t; -*-
(require 'ert)
(require 'json)
(defvar awc-writing-gui-root (getenv "AWC_WRITING_ROOT"))
(defvar awc-writing-gui-queue
  (json-parse-string (with-temp-buffer
                       (insert-file-contents (expand-file-name "expected.json" awc-writing-gui-root))
                       (buffer-string)) :object-type 'alist :array-type 'list))
(defvar awc-writing-gui-source nil)
(defvar awc-writing-gui-expected nil)
(defvar awc-writing-gui-ranges 0)
(defvar awc-writing-gui-deadline (+ (float-time) 60))
(setq awc-writing-program (getenv "AWC_WRITING_VALE"))

(defun awc-writing-gui-finish (status &optional error)
  (with-temp-file (expand-file-name "result.json" awc-gui-root)
    (insert (json-encode `((status . ,status) (ranges . ,awc-writing-gui-ranges)
                          (warnings . ,(vconcat awc-gui-warnings)) (error . ,error)))))
  (kill-emacs (if (equal status "passed") 0 1)))

(defun awc-writing-gui-step ()
  (condition-case problem
      (cond
       ((> (float-time) awc-writing-gui-deadline) (error "Graphical check timed out"))
       ((and awc-writing-gui-source
             (buffer-local-value 'awc-writing--process awc-writing-gui-source))
        (run-at-time 0.1 nil #'awc-writing-gui-step))
       (awc-writing-gui-source
        (let* ((source awc-writing-gui-source)
               (results (buffer-local-value 'awc-writing--results source))
               (next-error-highlight t))
          (should (with-current-buffer results
                    (string-match-p (format "Writing QA: %d findings" (length awc-writing-gui-expected)) (buffer-string))))
          (switch-to-buffer results)
          (goto-char (point-min))
          (setq next-error-last-buffer results)
          (dolist (finding awc-writing-gui-expected)
            (execute-kbd-macro (kbd "M-g n"))
            (should (eq (current-buffer) source))
            (should (eq (window-buffer (selected-window)) source))
            (should (= (line-number-at-pos) (nth 0 finding)))
            (should (= (- (point) (line-beginning-position)) (1- (nth 1 finding))))
            (should (equal (buffer-substring-no-properties (overlay-start compilation-highlight-overlay)
                                                           (overlay-end compilation-highlight-overlay))
                           (nth 4 finding)))
            (cl-incf awc-writing-gui-ranges))
          (switch-to-buffer results)
          (goto-char (point-min)) (search-forward ": error:") (beginning-of-line)
          (execute-kbd-macro (kbd "RET"))
          (should (eq (current-buffer) source))
          ;; Save the native frame with a visible source range and report.
          (switch-to-buffer source)
          (delete-other-windows)
          (display-buffer results '((display-buffer-below-selected) (window-height . 0.4)))
          (redisplay t)
          (when (fboundp 'x-export-frames)
            (let ((coding-system-for-write 'no-conversion))
              (write-region (x-export-frames nil 'png) nil
                            (expand-file-name (concat (file-name-extension (buffer-file-name source)) ".png") awc-gui-root)
                            nil 'silent)))
          (setq awc-writing-gui-source nil)
          (run-at-time 0.1 nil #'awc-writing-gui-step)))
       (awc-writing-gui-queue
        (let* ((entry (pop awc-writing-gui-queue))
               (file (expand-file-name (symbol-name (car entry))
                                       (expand-file-name "project with spaces é/" awc-writing-gui-root))))
          (find-file file)
          (should (display-graphic-p))
          (should-not (bound-and-true-p awc-writing--process))
          (should-not (bound-and-true-p lsp-mode))
          (setq awc-writing-gui-source (or (buffer-base-buffer) (current-buffer))
                awc-writing-gui-expected
                (cl-remove-if (lambda (case) (equal (car (last case)) "ABC")) (cdr entry)))
          (execute-kbd-macro (kbd "C-c w W c"))
          (run-at-time 0.1 nil #'awc-writing-gui-step)))
       (t
        (should-not awc-gui-warnings)
        (should-not (cl-some (lambda (process) (string-prefix-p "writing-qa" (process-name process))) (process-list)))
        (awc-writing-gui-finish "passed")))
    (error (awc-writing-gui-finish "failed" (error-message-string problem)))))

(run-at-time 1 nil #'awc-writing-gui-step)
;;; awc-writing-gui-check.el ends here

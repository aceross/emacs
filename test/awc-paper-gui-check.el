;;; awc-paper-gui-check.el --- Private normal manuscript acceptance -*- lexical-binding: t; -*-
(require 'ert)
(require 'json)
(defvar awc-paper-gui-project (file-name-as-directory (getenv "AWC_PAPER_QA_PROJECT")))
(defvar awc-paper-gui-report nil)
(defvar awc-paper-gui-phase 0)
(defvar awc-paper-gui-answers nil)
(defvar awc-paper-gui-answer-count 0)
(defvar awc-paper-gui-last-prompt nil)
(defvar awc-paper-gui-reviewed-token nil)
(defvar awc-paper-gui-deadline (+ (float-time) 150))
(defvar awc-paper-gui-rules nil)
(defvar awc-paper-gui-config nil)
;; Use the ordinary textual confirmation path in this private test profile.
(setq use-dialog-box nil)
(server-start)

(defun awc-paper-gui-finish (status &optional problem)
  (when awc-paper-gui-rules
    (with-temp-file (expand-file-name "rules/terms.txt" awc-paper-gui-project) (insert awc-paper-gui-rules)))
  (when awc-paper-gui-config
    (with-temp-file (expand-file-name ".awc-paper.json" awc-paper-gui-project) (insert awc-paper-gui-config)))
  (with-temp-file (expand-file-name "result.json" awc-gui-root)
    (insert (json-encode `((status . ,status) (phase . ,awc-paper-gui-phase)
                          (minibuffer_answers . ,awc-paper-gui-answer-count)
                          (warnings . ,(vconcat awc-gui-warnings)) (error . ,problem)))))
  (kill-emacs (if (equal status "passed") 0 1)))

(defun awc-paper-gui-answer ()
  (condition-case problem
      (cond
       ((> (float-time) awc-paper-gui-deadline) (error "Timed out answering the native minibuffer"))
       ((not awc-paper-gui-answers) nil)
       ((and (active-minibuffer-window)
             (not (equal (minibuffer-prompt) awc-paper-gui-last-prompt)))
        (let* ((answer (pop awc-paper-gui-answers))
               (short (and (member answer '("yes" "no"))
                           (eq (indirect-function 'yes-or-no-p) (indirect-function 'y-or-n-p)))))
          (setq awc-paper-gui-last-prompt (minibuffer-prompt)
                unread-command-events (append (cond ((eq answer 'quit) (list ?\C-g))
                                                    (short (list (if (equal answer "yes") ?y ?n)))
                                                    (t (append (string-to-list answer) (list ?\r))))
                                              unread-command-events)))
        (cl-incf awc-paper-gui-answer-count)
        (run-at-time 0.1 nil #'awc-paper-gui-answer))
       (t (run-at-time 0.1 nil #'awc-paper-gui-answer)))
    (error (awc-paper-gui-finish "failed" (error-message-string problem)))))

(defun awc-paper-gui-input (answers keys)
  (setq awc-paper-gui-answers answers awc-paper-gui-last-prompt nil)
  (run-at-time 0.1 nil #'awc-paper-gui-answer)
  (execute-kbd-macro (kbd keys)))

(defun awc-paper-gui-capture (name)
  (switch-to-buffer awc-paper-gui-report)
  (delete-other-windows)
  (goto-char (point-min))
  (redisplay t)
  (let ((coding-system-for-write 'no-conversion))
    (write-region (x-export-frames nil 'png) nil (expand-file-name name awc-gui-root) nil 'silent)))

(defun awc-paper-gui-data ()
  (buffer-local-value 'awc-paper--data awc-paper-gui-report))

(defun awc-paper-gui-step ()
  (condition-case problem
      (progn
        (when (> (float-time) awc-paper-gui-deadline) (error "Manuscript GUI check timed out"))
        (cond
         ((zerop awc-paper-gui-phase)
          (find-file (expand-file-name "paper.qmd" awc-paper-gui-project))
          (should (display-graphic-p))
          (execute-kbd-macro (kbd "C-c w M s"))
          (setq awc-paper-gui-report (current-buffer) awc-paper-gui-phase 1))
         ((and (= awc-paper-gui-phase 8) (file-exists-p (expand-file-name "slow.pid" awc-paper-gui-project)))
          (switch-to-buffer awc-paper-gui-report)
          (execute-kbd-macro (kbd "k"))
          (setq awc-paper-gui-phase 9))
         ((or (active-minibuffer-window)
              (buffer-local-value 'awc-paper--process awc-paper-gui-report)) nil)
         (t
          (should-not (gethash "error" (awc-paper-gui-data)))
          (switch-to-buffer awc-paper-gui-report)
          (pcase awc-paper-gui-phase
            (1
             (setq awc-paper-gui-reviewed-token (gethash "review_token" (awc-paper-gui-data))
                   awc-paper-gui-phase 6)
             (awc-paper-gui-input '("no") "c"))
            (6
             (should (equal awc-paper-gui-reviewed-token (gethash "review_token" (awc-paper-gui-data))))
             (should (string-match-p "Status:" (buffer-string)))
             (setq awc-paper-gui-phase 7)
             (awc-paper-gui-input '("yes" quit) "f"))
            (7
             (should (equal awc-paper-gui-reviewed-token (gethash "review_token" (awc-paper-gui-data))))
             (should-not awc-paper--last-checkpoint)
             (should (string-match-p "Status:" (buffer-string)))
             (awc-paper-gui-capture "review-cancelled.png")
             (setq awc-paper-gui-phase 2)
             (awc-paper-gui-input '("yes") "c"))
            (2
             (should (cl-every (lambda (item) (equal (gethash "state" item) "passed")) (gethash "checks" (awc-paper-gui-data))))
             (setq awc-paper-gui-phase 3)
             (awc-paper-gui-input '("submission-policy" "Synthetic GUI reviewer" "Inspected manuscript supplement and synthetic policy") "m"))
            (3
             (should (gethash "ready" (awc-paper-gui-data)))
             (let ((review (car (gethash "manual" (awc-paper-gui-data)))))
               (should (equal (gethash "reviewer" review) "Synthetic GUI reviewer"))
               (should (equal (gethash "note" review) "Inspected manuscript supplement and synthetic policy")))
             (setq awc-paper-gui-phase 4)
             (awc-paper-gui-input '("yes" "GUI reviewed checkpoint") "f"))
            (4
             (should (file-readable-p (expand-file-name awc-paper--last-checkpoint awc-paper-gui-project)))
             (goto-char (point-min)) (search-forward "Open last frozen manifest") (beginning-of-line)
             (execute-kbd-macro (kbd "RET"))
             (should (string-match-p "/checkpoints/.*\\.json\\'" buffer-file-name))
             (should (equal (gethash "label" (json-parse-string (buffer-string))) "GUI reviewed checkpoint"))
             (awc-paper-gui-capture "status.png")
             (setq awc-paper-gui-rules (with-temp-buffer
                                          (insert-file-contents (expand-file-name "rules/terms.txt" awc-paper-gui-project))
                                          (buffer-string)))
             (with-temp-file (expand-file-name "rules/terms.txt" awc-paper-gui-project) (insert awc-paper-gui-rules "Changed policy\n"))
             (switch-to-buffer awc-paper-gui-report) (execute-kbd-macro (kbd "g"))
             (setq awc-paper-gui-phase 5))
            (5
             (should (cl-every (lambda (item) (equal (gethash "state" item) "stale")) (gethash "checks" (awc-paper-gui-data))))
             (should (equal (gethash "state" (car (gethash "manual" (awc-paper-gui-data)))) "stale"))
             (awc-paper-gui-capture "stale.png")
             (with-temp-file (expand-file-name "rules/terms.txt" awc-paper-gui-project) (insert awc-paper-gui-rules))
             (setq awc-paper-gui-rules nil)
             ;; Run a clearly displayed synthetic slow command, then cancel it
             ;; through the actual report binding while its runner is active.
             (setq awc-paper-gui-config (with-temp-buffer
                                           (insert-file-contents (expand-file-name ".awc-paper.json" awc-paper-gui-project))
                                           (buffer-string)))
             (when (file-exists-p (expand-file-name "slow.pid" awc-paper-gui-project))
               (delete-file (expand-file-name "slow.pid" awc-paper-gui-project)))
             (with-temp-file (expand-file-name "scripts/slow.py" awc-paper-gui-project)
               (insert "import os,time\nfrom pathlib import Path\nPath('slow.pid').write_text(str(os.getpid()))\ntime.sleep(30)\n"))
             (let ((config (json-parse-string awc-paper-gui-config :object-type 'hash-table :array-type 'list)))
               (puthash "checks" (list (let ((item (make-hash-table :test 'equal)))
                                         (puthash "id" "slow" item) (puthash "argv" '("python3" "scripts/slow.py") item)
                                         (puthash "success" "exit-code" item) item)) config)
               (with-temp-file (expand-file-name ".awc-paper.json" awc-paper-gui-project) (insert (json-encode config))))
             (setq awc-paper-gui-phase 8)
             (awc-paper-gui-input '("yes") "c"))
            (9
             (should (equal (gethash "state" (car (gethash "checks" (awc-paper-gui-data)))) "cancelled"))
             (should (= awc-paper-gui-answer-count 10))
             (should-not awc-gui-warnings)
             (should-not (cl-some (lambda (proc) (string-prefix-p "paper-runner" (process-name proc))) (process-list)))
             (awc-paper-gui-finish "passed")))))
        (run-at-time 0.1 nil #'awc-paper-gui-step))
    (error (awc-paper-gui-finish "failed" (error-message-string problem)))))

(run-at-time 1 nil #'awc-paper-gui-step)
;;; awc-paper-gui-check.el ends here

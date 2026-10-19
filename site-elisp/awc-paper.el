;;; awc-paper.el --- Explicit manuscript checks and checkpoint review -*- lexical-binding: t; -*-
;;; Commentary:
;; The standalone runner owns policy and provenance.  This module only renders
;; its JSON records and requests explicit actions; opening a source starts none.
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'button)
(require 'compile)
(defgroup awc-paper nil "Manuscript lifecycle checkpoints." :group 'tools)
(defcustom awc-paper-python "python3" "Python for the standard-library runner." :type 'string)
(defconst awc-paper--runner
  (expand-file-name "../tools/paper/awc-paper.py"
                    (file-name-directory (file-truename (or load-file-name buffer-file-name)))))
(defvar-local awc-paper--root nil)
(defvar-local awc-paper--process nil)
(defvar-local awc-paper--data nil)
(defvar-local awc-paper--last-checkpoint nil)

(defun awc-paper--project ()
  "Resolve the explicitly configured local manuscript project."
  (or awc-paper--root
      (progn
        (when (file-remote-p default-directory) (user-error "Manuscript checks require a local project"))
        (or (locate-dominating-file default-directory ".awc-paper.json")
            (user-error "No project .awc-paper.json; see doc/manuscript-checkpoints.org")))))

(defun awc-paper--dirty (root)
  "List unsaved file buffers belonging to ROOT, including indirect sources."
  (delete-dups
   (cl-loop for buffer in (buffer-list)
            for file = (buffer-local-value 'buffer-file-name buffer)
            when (and file (not (file-remote-p file)) (file-in-directory-p file root)
                      (buffer-modified-p buffer)) collect file)))

(defun awc-paper--saved (root)
  (when (awc-paper--dirty root)
    (user-error "Save or discard unsaved manuscript project buffers explicitly before this action")))

(defun awc-paper--link (label path root &optional diagnostic)
  "Insert a link to a scoped project PATH, without treating output as Lisp."
  (let ((target (expand-file-name path root)))
    (when (file-in-directory-p target root)
      (insert-text-button label 'follow-link t
                          'action (lambda (_)
                                    (if (not diagnostic) (find-file-other-window target)
                                      (require 'compile)
                                      (let ((view (get-buffer-create (format "*Paper log: %s*" target))))
                                        (with-current-buffer view
                                          (compilation-mode)
                                          (let ((inhibit-read-only t))
                                            (erase-buffer)
                                            (compilation-forget-errors)
                                            ;; A header lets next-error visit a diagnostic
                                            ;; even when it is the log's very first line.
                                            (insert (format "Manuscript validation log: %s\n\n" target))
                                            (insert-file-contents target)
                                            (goto-char (point-min)))
                                          (setq-local default-directory root compilation-search-path (list root)))
                                        (pop-to-buffer view))))))))

(defun awc-paper--render (data)
  "Render DATA in this project's read-only status buffer."
  (let ((inhibit-read-only t) (root awc-paper--root))
    (setq awc-paper--data data)
    (erase-buffer)
    (insert (format "Manuscript checkpoint · %s\n\n" (abbreviate-file-name root)))
    (if-let* ((problem (gethash "error" data)))
        (insert "Unavailable: " problem "\n")
      (let* ((snapshot (gethash "snapshot" data))
             (git (gethash "git" snapshot))
             (dirty (awc-paper--dirty root)))
        (insert (format "Status: %s%s\nInput snapshot: %s\nGit revision: %s\n"
                        (if dirty "STALE" (upcase (gethash "state" data)))
                        (if dirty " — UNSAVED BUFFERS; disk results do not cover these edits" "")
                        (gethash "id" snapshot) (or (gethash "head" git) "unversioned/unavailable")))
        (when (gethash "input_status" git) (insert "Git inputs have uncommitted changes (included in hashes).\n"))
        (when dirty (insert "Unsaved: " (string-join dirty ", ") "\n"))
        (when-let* ((missing (gethash "missing" snapshot))) (insert "Missing inputs: " (string-join missing ", ") "\n"))
        (insert "\nAutomatic checks (passed means the declared contract passed):\n")
        (dolist (check (gethash "checks" data))
          (insert (format "  %-20s %-12s %s\n" (gethash "id" check) (upcase (gethash "state" check)) (or (gethash "detail" check) "")))
          (when-let* ((count (gethash "total" check))) (insert (format "    Prose words: %d\n" count)))
          (when-let* ((log (gethash "log" check))) (insert "    ") (awc-paper--link "Open diagnostic log" log root t) (insert "\n")))
        (unless (gethash "checks" data) (insert "  No automatic checks declared; this is incomplete.\n"))
        (insert "\nOutputs:\n")
        (maphash (lambda (name info)
                   (insert "  ") (awc-paper--link name name root)
                   (insert (if info (format " · %d bytes · %s\n" (gethash "bytes" info) (gethash "sha256" info)) " · missing\n")))
                 (or (gethash "outputs" data) (make-hash-table)))
        (insert "\nRecorded tool versions:\n")
        (maphash (lambda (name info)
                   (insert (format "  %s: %s\n" name (or (gethash "unavailable" info)
                                                          (car (split-string (or (gethash "version" info) "no version reported") "\n" t))))))
                 (or (gethash "tools" (gethash "run" data)) (make-hash-table)))
        (insert "\nManual requirements:\n")
        (dolist (item (gethash "manual" data))
          (insert (format "  %s: %s — %s\n" (gethash "id" item) (upcase (gethash "state" item)) (gethash "description" item)))
          (when-let* ((source (gethash "source" item))) (insert (format "    Policy: %s (reviewed %s)\n" source (gethash "date" item))))
          (when-let* ((note (gethash "note" item))) (insert (format "    %s: %s\n" (gethash "reviewer" item) note))))
        (insert "\nReviewed execution plan (project working directory; argv, no shell):\n")
        (maphash (lambda (name command) (insert (format "  Tool version %s: %S\n" name command)))
                 (or (gethash "tools" (gethash "plan" data)) (make-hash-table)))
        (dolist (spec (gethash "checks" (gethash "plan" data)))
          (insert (format "  %s: %S\n    Contract: %s; outputs: %S; env: %S\n"
                          (gethash "id" spec) (gethash "argv" spec)
                          (or (gethash "success" spec) (gethash "type" spec))
                          (gethash "outputs" spec) (gethash "env" spec))))
        (insert "\nInputs (declared coverage; undeclared dependencies are not verified):\n")
        (maphash (lambda (file _hash) (insert "  ") (awc-paper--link file file root) (insert "\n"))
                 (gethash "files" snapshot))
        (when-let* ((checkpoint (gethash "checkpoint" data))) (setq awc-paper--last-checkpoint checkpoint))
        (when awc-paper--last-checkpoint
          (insert "\n") (awc-paper--link "Open last frozen manifest" awc-paper--last-checkpoint root) (insert "\n"))))
    (insert "\ng: refresh disk status; c: review/run; m: record manual verification; f: review/freeze; k: cancel.\nReadiness covers the declared checks, not scholarly accuracy.\n")
    (goto-char (point-min))))

(defun awc-paper-cancel ()
  "Ask this project's runner to cancel and reap its owned command helpers."
  (interactive)
  (let* ((root (awc-paper--project))
         (report (get-buffer (format "*Paper: %s*" root))))
    (when (buffer-live-p report)
      (with-current-buffer report
        (when (process-live-p awc-paper--process)
          (process-put awc-paper--process 'awc-paper-cancelled t)
          (signal-process awc-paper--process 'SIGTERM))))))

(define-derived-mode awc-paper-mode special-mode "Manuscript"
  "Review project validation results and deliberate manuscript checkpoints."
  (add-hook 'kill-buffer-hook #'awc-paper-cancel nil t))
(define-key awc-paper-mode-map (kbd "g") #'awc-paper-status)
(define-key awc-paper-mode-map (kbd "c") #'awc-paper-check)
(define-key awc-paper-mode-map (kbd "m") #'awc-paper-verify)
(define-key awc-paper-mode-map (kbd "f") #'awc-paper-freeze)
(define-key awc-paper-mode-map (kbd "k") #'awc-paper-cancel)
(define-key awc-paper-mode-map (kbd "TAB") #'forward-button)
(define-key awc-paper-mode-map (kbd "<backtab>") #'backward-button)
(define-key awc-paper-mode-map (kbd "RET") #'push-button)

(defun awc-paper--start (root action &optional arguments callback)
  "Run a bounded runner ACTION for ROOT, then CALLBACK with its JSON record."
  (let* ((default-directory root)
         (program (or (executable-find awc-paper-python) (user-error "Cannot find Python for manuscript checks")))
         (report (get-buffer-create (format "*Paper: %s*" root))))
    (with-current-buffer report
      (when (process-live-p awc-paper--process) (user-error "This project's manuscript action is still running"))
      (unless (derived-mode-p 'awc-paper-mode) (awc-paper-mode))
      (setq awc-paper--root root)
      (let ((inhibit-read-only t)) (erase-buffer) (insert (format "Manuscript %s running…\nProject: %s\n" action root)))
      (let* ((stdout (generate-new-buffer " *paper-stdout*"))
             (stderr (generate-new-buffer " *paper-stderr*"))
             (command (append (list program awc-paper--runner "--config" (expand-file-name ".awc-paper.json" root))
                              (when-let* ((token (plist-get arguments :expect))) (list "--expect" token))
                              (list action) (plist-get arguments :args)))
             process)
        (condition-case error
            (progn
              (setq process
                    (make-process
                     :name "paper-runner" :buffer stdout :stderr stderr :noquery t :coding 'utf-8-unix :connection-type 'pipe
                     :command command
                     :sentinel
                     (lambda (proc _event)
                       (when (memq (process-status proc) '(exit signal failed))
                         (unwind-protect
                             (when (buffer-live-p report)
                               (with-current-buffer report
                                 (setq awc-paper--process nil)
                                 (condition-case problem
                                     (let ((data (if (and (process-get proc 'awc-paper-cancelled)
                                                          (with-current-buffer stdout (string-empty-p (buffer-string))))
                                                     (let ((record (make-hash-table :test 'equal)))
                                                       (puthash "error" "Manuscript action cancelled; refresh status for retained results" record) record)
                                                   (with-current-buffer stdout
                                                     (json-parse-string (buffer-string) :object-type 'hash-table :array-type 'list
                                                                        :null-object nil :false-object nil)))))
                                       (awc-paper--render data)
                                       (when (and callback (not (gethash "error" data))) (funcall callback data)))
                                   (error (awc-paper--render
                                           (let ((data (make-hash-table :test 'equal)))
                                             (puthash "error" (format "%s; %s" (error-message-string problem)
                                                                     (with-current-buffer stderr (buffer-string))) data) data))))))
                           (when (buffer-live-p stdout) (kill-buffer stdout))
                           (when (buffer-live-p stderr) (kill-buffer stderr)))))))
              (when-let* ((pipe (get-buffer-process stderr))) (set-process-sentinel pipe #'ignore) (set-process-query-on-exit-flag pipe nil))
              (setq awc-paper--process process))
          (error (when (buffer-live-p stdout) (kill-buffer stdout))
                 (when (buffer-live-p stderr) (kill-buffer stderr))
                 (signal (car error) (cdr error))))))
    (pop-to-buffer report)))

;;;###autoload
(defun awc-paper-status ()
  "Hash the current disk inputs and display results; execute no project code."
  (interactive)
  (awc-paper--start (awc-paper--project) "status"))

;;;###autoload
(defun awc-paper-check ()
  "Review the exact project execution plan, then explicitly run its checks."
  (interactive)
  (let ((root (awc-paper--project)))
    (awc-paper--saved root)
    (awc-paper--start
     root "status" nil
     (lambda (data)
       (when (yes-or-no-p "Run the displayed manuscript commands and tool probes? ")
         (awc-paper--saved root)
         (awc-paper--start root "check" (list :expect (gethash "review_token" data) :args '("--execute"))))))))

;;;###autoload
(defun awc-paper-verify ()
  "Record an explicitly reviewed manual requirement against this snapshot."
  (interactive)
  (let ((root (awc-paper--project)))
    (awc-paper--saved root)
    (awc-paper--start
     root "status" nil
     (lambda (data)
       (let* ((choices (cl-loop for item in (gethash "manual" data)
                                unless (equal (gethash "state" item) "not-applicable") collect (gethash "id" item)))
              (id (if choices (completing-read "Reviewed requirement: " choices nil t) (user-error "No applicable manual requirement")))
              (reviewer (read-string "Reviewer: "))
              (note (read-string "Concrete review note: ")))
         (awc-paper--saved root)
         (awc-paper--start root "verify" (list :expect (gethash "review_token" data)
                                               :args (list id "--reviewer" reviewer "--note" note))))))))

;;;###autoload
(defun awc-paper-freeze ()
  "Review and write a checkpoint, preserving failed/unrun states explicitly."
  (interactive)
  (let ((root (awc-paper--project)))
    (awc-paper--saved root)
    (awc-paper--start
     root "status" nil
     (lambda (data)
       (when (yes-or-no-p (format "Freeze this reviewed %s snapshot, including unresolved items? " (gethash "state" data)))
         (let ((label (read-string "Checkpoint label: ")))
           (awc-paper--saved root)
           (awc-paper--start root "freeze" (list :expect (gethash "review_token" data) :args (list "--label" label)))))))))

(provide 'awc-paper)
;;; awc-paper.el ends here

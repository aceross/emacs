;;; reliability-startup-check.el --- Normal startup with disposable state -*- lexical-binding: t; -*-
;; Loaded from the fixture's early-init, before the unchanged configuration.
(defvar awc-reliability-start (float-time))
(defvar awc-reliability-gcs gcs-done)
(defvar awc-reliability-gc-time gc-elapsed)
(defvar awc-reliability-warnings nil)
(defvar awc-reliability-repo
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar awc-reliability-root (getenv "AWC_RELIABILITY_ROOT"))
(unless (and awc-reliability-root (file-in-directory-p awc-reliability-root "/tmp/"))
  (error "Reliability fixtures must be under /tmp"))
(setq straight-safe-mode t
      org-roam-db-location (expand-file-name "notes.db" awc-reliability-root))
(with-eval-after-load 'org-roam
  (setq org-roam-directory (expand-file-name "notes/" awc-reliability-root))
  (make-directory org-roam-directory t))
(require 'server)
(setq server-name "reliability"
      server-socket-dir (expand-file-name "server/" awc-reliability-root))
(advice-add 'display-warning :before
            (lambda (type message &optional level &rest _)
              (push (list :type (format "%S" type) :message (format "%s" message)
                          :level (format "%S" level)) awc-reliability-warnings)))

(defun awc-reliability-finish ()
  "Record real startup before running optional-tool checks, then exit."
  (let ((record (list :emacs emacs-version
                      :init_seconds (float-time (time-subtract after-init-time before-init-time))
                      :through_startup_hook_seconds (- (float-time) awc-reliability-start)
                      :gcs (- gcs-done awc-reliability-gcs)
                      :gc_seconds (- gc-elapsed awc-reliability-gc-time)
                      :gc_threshold gc-cons-threshold :gc_percentage gc-cons-percentage
                      :warnings (vconcat (reverse awc-reliability-warnings))))
        failure)
    (condition-case err
        (progn
          (when init-file-had-error (error "Native startup reported an init error"))
          (unless (and file-name-handler-alist (= gc-cons-threshold 16777216)
                       (= gc-cons-percentage 0.1))
            (error "Startup did not restore file handlers/GC policy"))
          (when awc-reliability-warnings (error "Startup warnings require review"))
          (when (getenv "AWC_RELIABILITY_MISSING")
            (load (expand-file-name "test/reliability-missing-tools.el" awc-reliability-repo) nil t)))
      (error (setq failure (error-message-string err))))
    (require 'json)
    (let ((file (getenv "AWC_RELIABILITY_RESULT")))
      (with-temp-file file
        (insert (json-serialize (append record (list :failure (or failure :null)))) "\n"))
      (with-temp-file (concat file ".messages")
        (insert (with-current-buffer "*Messages*" (buffer-string)))))
    (kill-emacs (if failure 1 0))))

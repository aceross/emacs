;;; awc-r-httpgd.el --- Session-local asynchronous R plot viewer -*- lexical-binding: t; -*-
(require 'awc-r)
(require 'url)
(require 'browse-url)
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))

(defcustom awc-r-httpgd-timeout 20
  "Seconds allowed for device creation and the viewer's HTTP response."
  :type 'number :group 'ess)

(defconst awc-r-httpgd--command
  "local({ old <- options(error=NULL); on.exit(options(old)); tryCatch({
 if (!requireNamespace('httpgd', quietly=TRUE)) stop('Install httpgd explicitly in this project R library, then try again')
 state <- getOption('awc.httpgd')
 url <- if (!is.null(state)) tryCatch(httpgd::hgd_url(which=state$device), error=function(e) NULL) else NULL
 if (is.null(url) || !identical(url, state$url)) {
   httpgd::hgd(host='127.0.0.1', port=0, token=TRUE, silent=TRUE)
   state <- list(device=grDevices::dev.cur(), url=httpgd::hgd_url())
   options(awc.httpgd=state)
 } else grDevices::dev.set(state$device)
 cat('\nAWC_HTTPGD_URL=', state$url, '\nAWC_HTTPGD_END\n', sep='')
 }, error=function(e) cat('\nAWC_HTTPGD_ERROR=', conditionMessage(e), '\n', sep='')) })\n")

(defun awc-r-httpgd--url (output)
  "Extract a complete loopback viewer URL from R OUTPUT."
  (when (string-match
         "^AWC_HTTPGD_URL=\\(http://\\(?:127\\.0\\.0\\.1\\|localhost\\):[0-9]+/[^[:space:]]+\\)[\r\n]"
         output)
    (match-string 1 output)))

(defun awc-r-httpgd--active-p (proc request)
  (and (process-live-p proc) (eq request (process-get proc 'awc-httpgd-request))))

(defun awc-r-httpgd--finish (proc request &optional error)
  (when (eq request (process-get proc 'awc-httpgd-request))
    (process-put proc 'awc-httpgd-request nil)
    (when-let* ((timer (process-get proc 'awc-httpgd-timer))) (cancel-timer timer))
    (when error (message "R plots: %s" error))))

(defun awc-r-httpgd--show (proc url)
  "Show PROC's actual URL, reusing its browser and preserving source focus."
  (if (and (display-graphic-p) (featurep 'xwidget-internal))
      (progn
        (require 'xwidget)
        (let* ((browser (process-get proc 'awc-httpgd-browser))
               (window (display-buffer
                        (if (buffer-live-p browser) browser (process-buffer proc))
                        '((display-buffer-reuse-window display-buffer-pop-up-window)
                          (inhibit-same-window . t)))))
          (unless (window-live-p window) (user-error "Cannot display R plots"))
          (with-selected-window window
            (if (buffer-live-p browser)
                (with-current-buffer browser
                  (unless (equal url (process-get proc 'awc-httpgd-shown-url))
                    (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url)))
              (xwidget-webkit-browse-url url t)
              (setq browser (current-buffer))
              (setq-local awc-r--owner proc)
              (process-put proc 'awc-httpgd-browser browser)))))
    (browse-url url))
  (process-put proc 'awc-httpgd-shown-url url))

(defun awc-r-httpgd--poll (proc request url)
  "Wait asynchronously for URL to return HTTP 200 before displaying it."
  (when (awc-r-httpgd--active-p proc request)
    (let ((url-request-extra-headers nil)
          (url-show-status nil))
      (url-retrieve
       url
       (lambda (status)
         (let ((ready (and (not (plist-get status :error))
                           (boundp 'url-http-response-status)
                           (= (or url-http-response-status 0) 200))))
           (kill-buffer (current-buffer))
           (when (awc-r-httpgd--active-p proc request)
             (if ready
                 (progn
                   (awc-r-httpgd--finish proc request)
                   (condition-case err
                       (when-let* ((frame (process-get proc 'awc-httpgd-frame))
                                   (_ (frame-live-p frame)))
                         (with-selected-frame frame (awc-r-httpgd--show proc url)))
                     (error (message "R plots: %s" (error-message-string err)))))
               (run-at-time 0.2 nil #'awc-r-httpgd--poll proc request url)))))
       nil t t))))

(defun awc/r-httpgd-init-and-open ()
  "Create or reuse this project's httpgd device, then open its ready viewer.
Package installation is always explicit.  A busy R session is left alone."
  (interactive)
  (let* ((proc (awc-r-process t))
         (request (list (float-time)))
         (buffer (get-buffer-create (format " *httpgd: %s*" (process-name proc)))))
    (when (process-get proc 'awc-httpgd-request)
      (user-error "The plot viewer is still starting for this R session"))
    (when (or (process-get proc 'busy) (process-get proc 'running-async?))
      (user-error "R is busy; interrupt with C-c C-k or wait for the prompt"))
    (process-put proc 'awc-httpgd-request request)
    (process-put proc 'awc-httpgd-frame (selected-frame))
    (process-put proc 'awc-httpgd-timer
                 (run-at-time awc-r-httpgd-timeout nil #'awc-r-httpgd--finish
                              proc request "Viewer startup timed out; check the R session and retry"))
    (condition-case err
        (awc-r-async-command
         awc-r-httpgd--command buffer proc
         (lambda (_proc _output)
           (when (awc-r-httpgd--active-p proc request)
             (let* ((output (with-current-buffer buffer (buffer-string)))
                    (url (awc-r-httpgd--url output)))
               (if url
                   (progn (process-put proc 'awc-httpgd-url url)
                          (awc-r-httpgd--poll proc request url))
                 (awc-r-httpgd--finish
                  proc request
                  (if (string-match "AWC_HTTPGD_ERROR=\\([^\r\n]+\\)" output)
                      (match-string 1 output) "R did not return a viewer URL; check its output")))))))
      (error (awc-r-httpgd--finish proc request) (signal (car err) (cdr err))))))

;; Preserve existing entry points; all now create/reuse the same owned device.
(defalias 'awc/r-httpgd-open #'awc/r-httpgd-init-and-open)
(defalias 'awc/r-httpgd-init #'awc/r-httpgd-init-and-open)

(provide 'awc-r-httpgd)
;;; awc-r-httpgd.el ends here

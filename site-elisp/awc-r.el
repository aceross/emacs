;;; awc-r.el --- Project-owned ESS sessions -*- lexical-binding: t; -*-
(require 'awc-project)
(require 'ess-site)

(defun awc-r--fontify-repl (original begin end &optional verbose)
  "Prepare skipped R console output before ORIGINAL fontifies BEGIN to END.
ESS narrows to each prompt's output.  Emacs cannot prepare syntax for an
earlier, unparsed region while narrowed to a later one.  Process that gap
using ESS's own prompt boundaries, preserving isolation between outputs."
  (when (derived-mode-p 'inferior-ess-r-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((start (max (point-min) syntax-propertize--done)))
          (when (< start begin)
            (funcall original start begin verbose))))))
  (funcall original begin end verbose))

(advice-add 'inferior-ess-fontify-region :around #'awc-r--fontify-repl)

(defvar awc-r--sessions (make-hash-table :test #'equal))
(defvar awc-r--starting nil)
(defvar-local awc-r--owner nil "R process owning this auxiliary buffer.")

(defun awc-r--source-p ()
  "Whether this is a local, file-backed R source or Quarto R chunk."
  (and (not awc-r--starting) (derived-mode-p 'ess-r-mode)
       (buffer-local-value 'buffer-file-name (awc-project-source-buffer))
       (not (file-remote-p default-directory))))

(defun awc-r--selection ()
  "Return session identity, root, R executable and optional Python."
  (let* ((root (awc-project-root))
         (python (awc-project-python nil t))
         (program (or (executable-find inferior-ess-r-program)
                      (user-error "Cannot find R: %s" inferior-ess-r-program))))
    (list (list root program python) root program python)))

(defun awc-r-process (&optional start)
  "Return this project's live R process; START permits starting it.
An inferior or inspection buffer always keeps its own original session."
  (let ((owner (or awc-r--owner
                   (and (derived-mode-p 'inferior-ess-r-mode)
                        (get-buffer-process (current-buffer))))))
    (if owner
        (if (process-live-p owner) owner (user-error "This R session has ended"))
      (let ((proc (gethash (car (awc-r--selection)) awc-r--sessions)))
        (cond ((process-live-p proc)
               (setq-local ess-local-process-name (process-name proc)) proc)
              (start (awc-r-run))
              (t nil))))))

(defun awc-r-run (&optional restart)
  "Display this project's R session, starting it if needed.
With prefix RESTART, discard this session's workspace and start afresh.
Use normal R profiles (including renv), without restoring .RData."
  (interactive "P")
  (let* ((owner (or awc-r--owner
                    (and (derived-mode-p 'inferior-ess-r-mode)
                         (get-buffer-process (current-buffer)))))
         (selection (or (and owner (process-get owner 'awc-r-selection))
                        (awc-r--selection)))
         (key (nth 0 selection)) (root (nth 1 selection))
         (program (nth 2 selection)) (python (nth 3 selection))
         (proc (gethash key awc-r--sessions)))
    (when (and restart (process-live-p proc)) (delete-process proc))
    (unless (process-live-p proc)
      (let* ((default-directory root)
             (awc-r--starting t)
             (process-environment (append (awc-project-environment python) process-environment))
             (exec-path (append (split-string (getenv "PATH") path-separator t) (list exec-directory)))
             (inferior-ess-r-program program)
             (ess-startup-directory root)
             (ess-ask-for-ess-directory nil)
             (ess-ask-about-transfile nil)
             (ess-history-file nil)
             (name (format "*%s*" (awc-project-process-name "R" (concat program "\0" python))))
             (ess-gen-proc-buffer-name-function (lambda (_) (generate-new-buffer-name name)))
             (buffer (save-window-excursion
                       ;; A neutral buffer prevents ESS reusing an ended REPL.
                       (with-temp-buffer
                         (setq default-directory root)
                         (run-ess-r "--no-save --no-restore")))))
        (setq proc (get-buffer-process buffer))
        (process-put proc 'awc-r-selection selection)
        (puthash key proc awc-r--sessions)
        (with-current-buffer buffer
          (setq-local awc-project-python python)
          (setq-local header-line-format
                      (format " R project: %s | Python: %s" root (or python "none"))))))
    (setq-local ess-local-process-name (process-name proc))
    (display-buffer (process-buffer proc))
    proc))

(defun awc-r-interrupt ()
  "Interrupt only this project's existing R session."
  (interactive)
  (let ((proc (or (awc-r-process) (user-error "No running R session for this project"))))
    (interrupt-process proc)))

(defun awc-r--force (original &optional prompt force no-autostart ask-if-1)
  "Route native ESS evaluation in project sources to their owned session."
  (if (awc-r--source-p)
      (process-buffer (or (awc-r-process (not no-autostart))
                          (user-error "Start this project's R session with C-c C-z")))
    (funcall original prompt force no-autostart ask-if-1)))

(defun awc-r--get (original &optional name use-another)
  "Prevent ESS background queries from selecting another project's R."
  (if (and (awc-r--source-p) (or (null name) (equal name ess-local-process-name)))
      (or (awc-r-process) (user-error "No R session for this project; use C-c C-z"))
    (funcall original name use-another)))

(defun awc-r--current (original)
  "Look up a project process without starting R while editing."
  (if (awc-r--source-p)
      (condition-case nil (awc-r-process) (user-error nil))
    (funcall original)))

(defun awc-r--make-current (original)
  "Keep completion association current without starting a process."
  (if (awc-r--source-p)
      (setq-local ess-local-process-name
                  (when-let* ((proc (condition-case nil (awc-r-process) (user-error nil))))
                    (process-name proc)))
    (funcall original)))

(advice-add 'ess-force-buffer-current :around #'awc-r--force)
(advice-add 'ess-get-process :around #'awc-r--get)
(advice-add 'ess-get-current-process :around #'awc-r--current)
(advice-add 'ess-make-buffer-current :around #'awc-r--make-current)

(defun awc-r-eval-region (start end)
  "Send START through END to this project's R session."
  (awc-r-run)
  (ess-eval-region start end nil))

(defun awc-r-async-command (code buffer proc callback)
  "Run CODE into BUFFER on PROC, calling CALLBACK on completion.
This ESS version leaves running-async? set after delimited output; clear it
only after our own command's completion, without altering unrelated jobs."
  (ess-async-command
   code buffer proc
   (lambda (process output)
     (process-put process 'running-async? nil)
     (funcall callback process output))))

(defun awc-r--query (code kind &optional package)
  "Show asynchronous CODE output from this project's R in a KIND buffer.
If PACKAGE is missing, explain it without installing anything."
  (let* ((proc (awc-r-process t))
         (buffer (get-buffer-create (format "*%s: %s*" kind (buffer-name (process-buffer proc)))))
         (guard (if package
                    (format "if (!requireNamespace(%S, quietly=TRUE)) stop(%S); "
                            package (format "Install %s explicitly in this project's R library" package)) "")))
    (when (or (process-get proc 'busy) (process-get proc 'running-async?))
      (user-error "R is busy; interrupt with C-c C-k or wait for the prompt"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq buffer-read-only nil))
    (awc-r-async-command
     (format "local({ old <- options(error=NULL); on.exit(options(old)); tryCatch({ %s%s }, error=function(e) cat('R: ', conditionMessage(e), '\\n', sep='')) })\n" guard code)
     buffer proc
     (lambda (_proc _output)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (goto-char (point-min)) (special-mode)
           (setq-local awc-r--owner proc))
         (display-buffer buffer))))
    buffer))

(defun awc/r-symbol-at-point () (thing-at-point 'symbol t))
(defun awc/r--ensure-symbol (sym)
  (or sym (user-error "No symbol at point")))

(defun awc-r-inspect (symbol function &optional package)
  "Inspect the named R SYMBOL using FUNCTION from optional PACKAGE."
  (awc-r--query (format "%s(get(%S, envir=.GlobalEnv, inherits=TRUE))"
                        function symbol) "R Inspect" package))

(dolist (spec '((awc/r-inspect-str "str") (awc/r-inspect-head "head")
                (awc/r-inspect-summary "summary")
                (awc/r-inspect-glimpse "dplyr::glimpse" "dplyr")
                (awc/r-inspect-skim "skimr::skim" "skimr")))
  (let ((function (nth 1 spec)) (package (nth 2 spec)))
    (defalias (car spec)
      (lambda (symbol)
        (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
        (awc-r-inspect symbol function package))
      (format "Inspect the object at point with R's %s." function))))

(defun awc/r-env-list ()
  "List objects in this session's workspace, including an empty workspace."
  (interactive)
  (awc-r--query
   "n <- ls(envir=.GlobalEnv); if (!length(n)) cat('Empty workspace\\n') else print(do.call(rbind, lapply(n, function(nm) { x <- get(nm, envir=.GlobalEnv); data.frame(name=nm, class=paste(class(x), collapse=','), type=typeof(x), length=length(x), row.names=NULL) })), row.names=FALSE)"
   "R Env"))

(defun awc-r-info ()
  "Inspect the actual session's interpreter, libraries and Python selection."
  (interactive)
  (awc-r--query
   "cat(R.version.string, '\\nWorking directory:', getwd(), '\\nLibraries:\\n'); writeLines(.libPaths()); cat('RETICULATE_PYTHON:', Sys.getenv('RETICULATE_PYTHON'), '\\n'); print(vapply(c('httpgd','reticulate','languageserver'), requireNamespace, logical(1), quietly=TRUE))"
   "R Environment"))

(provide 'awc-r)
;;; awc-r.el ends here

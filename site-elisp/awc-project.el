;;; awc-project.el --- Shared local project environments -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'compile)
(require 'subr-x)
(defvar projectile-project-root-functions)
(declare-function vterm "vterm" (&optional arg))

(defgroup awc-project nil "Local research project workflows." :group 'tools)

(defcustom awc-project-tool-directories
  (list (expand-file-name "tools/pyright/node_modules/.bin" user-emacs-directory))
  "Optional configuration-local tool directories, ahead of the system PATH.
These tools are installed explicitly; opening a file never downloads them."
  :type '(repeat directory) :group 'awc-project)

;; Keep existing Quarto directory locals working, now for every consumer.
(defvaralias 'quarto-preview-python 'awc-project-python)
(defcustom awc-project-python nil
  "Project Python executable, or nil to find the nearest .venv or venv.
Relative paths start at the project root.  For Poetry, conda or another
externally managed environment, set its existing bin/python here using
.dir-locals.el.  Nothing is installed or activated globally."
  :type '(choice (const :tag "Detect" nil) file) :group 'awc-project)
(make-variable-buffer-local 'awc-project-python)

(defun awc-project-split-arguments (text)
  "Split TEXT using shell quoting, treating all expansions/operators literally.
Unlike `split-string-shell-command', retain arguments before a semicolon.
Reject unmatched quotes rather than silently discarding arguments."
  (let ((index 0) (length (length text)) quote started chars result)
    (while (< index length)
      (let ((char (aref text index)))
        (cond
         ((and (not quote) (memq char '(?\s ?\t ?\n)))
          (when started
            (push (apply #'string (nreverse chars)) result)
            (setq chars nil started nil)))
         ((and (not (eq quote ?\')) (eq char ?\\))
          (cl-incf index)
          (when (= index length) (user-error "Trailing backslash in arguments"))
          (let ((next (aref text index)))
            (when (and (eq quote ?\") (not (memq next '(?\" ?\\ ?$ ?` ?\n))))
              (push ?\\ chars))
            (unless (eq next ?\n) (push next chars)))
          (setq started t))
         ((and quote (eq char quote)) (setq quote nil))
         ((and (not quote) (memq char '(?\' ?\"))) (setq quote char started t))
         (t (push char chars) (setq started t))))
      (cl-incf index))
    (when quote (user-error "Unclosed quote in arguments"))
    (when started (push (apply #'string (nreverse chars)) result))
    (nreverse result)))

(defun awc-project-source-buffer ()
  "Return the file buffer underlying the current indirect buffer."
  (or (buffer-base-buffer) (current-buffer)))

(defun awc-project-find-root (directory)
  "Find a local project boundary above DIRECTORY, or nil.
Repository and language project markers precede nested Quarto projects.
Never search above a nested repository or language project."
  (unless (file-remote-p directory)
    (let ((root
           (or (locate-dominating-file
                directory
                (lambda (dir)
                  (or (cl-some (lambda (name) (file-exists-p (expand-file-name name dir)))
                               '(".git" ".hg" "pyproject.toml" ".projectile" "renv.lock"))
                      ;; New files can have parents that do not exist yet.
                      ;; Keep searching upward until an existing directory.
                      (and (file-directory-p dir)
                           (directory-files dir nil "\\.Rproj\\'" t)))))
               (locate-dominating-file
                directory
                (lambda (dir)
                  (cl-some (lambda (name) (file-exists-p (expand-file-name name dir)))
                           '("_quarto.yml" "_quarto.yaml" ".venv" "venv")))))))
      (when root (file-name-as-directory (file-truename root))))))

(defun awc-project-root (&optional directory)
  "Return the project root for DIRECTORY, falling back to DIRECTORY."
  (let ((directory (or directory
                       (with-current-buffer (awc-project-source-buffer)
                         (if buffer-file-name (file-name-directory buffer-file-name)
                           default-directory)))))
    (when (file-remote-p directory)
      (user-error "These project environment commands currently require local files"))
    (or (awc-project-find-root directory)
        (file-name-as-directory (file-truename directory)))))

(defun awc-project-try (directory)
  "Connect project.el to the same boundaries used by Projectile."
  (when-let* ((root (awc-project-find-root directory)))
    (let ((vc (project-try-vc directory)))
      ;; Retain VC's efficient tracked-file and ignore handling when possible.
      (if (and vc (equal root (file-truename (project-root vc)))) vc
        (cons 'awc-project root)))))

(cl-defmethod project-root ((project (head awc-project))) (cdr project))
(add-hook 'project-find-functions #'awc-project-try)
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-functions #'awc-project-find-root))

(defun awc-project-python (&optional directory optional)
  "Return the selected Python for DIRECTORY without resolving its symlink.
With OPTIONAL return nil when no environment exists; broken environments
and explicit invalid settings always signal an error."
  (when (file-remote-p (or directory default-directory))
    (user-error "These project environment commands currently require local files"))
  (let* ((directory (file-name-as-directory
                     (file-truename
                      (or directory
                          (with-current-buffer (awc-project-source-buffer)
                            (if buffer-file-name (file-name-directory buffer-file-name)
                              default-directory))))))
         (root (awc-project-root directory))
         (explicit (buffer-local-value 'awc-project-python (awc-project-source-buffer)))
         (python (and explicit (expand-file-name explicit root)))
         environment)
    (unless python
      (while directory
        (setq environment
              (cl-find-if (lambda (path) (or (file-exists-p path) (file-symlink-p path)))
                          (mapcar (lambda (name) (expand-file-name name directory))
                                  '(".venv" "venv"))))
        (if environment
            (setq python (expand-file-name "bin/python" environment) directory nil)
          (setq directory
                (unless (equal directory root)
                  (file-name-directory (directory-file-name directory)))))))
    (when (and (not python) (not optional))
      (user-error "No project Python; create .venv with your package manager or set awc-project-python"))
    (when (and python (or (file-remote-p python)
                          (not (file-regular-p python)) (not (file-executable-p python))))
      (user-error "Project Python is not executable: %s" python))
    python))

(defun awc-project-environment (python)
  "Return subprocess overrides for PYTHON, leaving Emacs unchanged.
Strip an inherited venv/conda bin directory before prepending the selected
one.  Nil PYTHON gives an unactivated environment for non-Python projects.
Keep project R settings (including renv startup) intact."
  (let* ((process-environment (append compilation-environment process-environment))
         (old-bins (delq nil
                         (mapcar (lambda (key)
                                   (when-let* ((prefix (getenv key)))
                                     (directory-file-name (expand-file-name "bin" prefix))))
                                 '("VIRTUAL_ENV" "CONDA_PREFIX"))))
         (paths (cl-remove-if
                 (lambda (path) (member (directory-file-name path) old-bins))
                 (split-string (or (getenv "PATH") "") path-separator t)))
         (paths (append (cl-remove-if-not #'file-directory-p awc-project-tool-directories) paths))
         (bin (and python (directory-file-name (file-name-directory python))))
         (prefix (and bin (file-name-directory bin))))
    (append
     (when python (list (concat "QUARTO_PYTHON=" python)
                        (concat "RETICULATE_PYTHON=" python)
                        "JUPYTER_PREFER_ENV_PATH=1"))
     (unless python '("QUARTO_PYTHON" "RETICULATE_PYTHON" "JUPYTER_PREFER_ENV_PATH"))
     (list (concat "PATH=" (mapconcat #'identity
                                     (delete-dups (if bin (cons bin paths) paths))
                                     path-separator))
           (if (and prefix (file-exists-p (expand-file-name "pyvenv.cfg" prefix)))
               (concat "VIRTUAL_ENV=" (directory-file-name prefix)) "VIRTUAL_ENV")
           "PYTHONHOME" "CONDA_PREFIX" "CONDA_DEFAULT_ENV" "CONDA_PROMPT_MODIFIER"))))

(defun awc-project-process-name (kind python)
  "Return a readable process name for KIND, project and PYTHON."
  (let ((root (awc-project-root)))
    (format "%s: %s:%s" kind (file-name-nondirectory (directory-file-name root))
            (substring (secure-hash 'sha256 (concat root "\0" python)) 0 12))))

(defun awc-project-info ()
  "Show project selection and missing capabilities without executing code."
  (interactive)
  (let* ((root (awc-project-root))
         (resolution (condition-case err (list :python (awc-project-python))
                       (user-error (list :error (error-message-string err)))))
         (python (plist-get resolution :python))
         (selection (or python (plist-get resolution :error)))
         (process-environment (append (awc-project-environment python) process-environment))
         (exec-path (append (split-string (getenv "PATH") path-separator t) (list exec-directory)))
         (source (buffer-name (awc-project-source-buffer))))
    (with-help-window "*Project environment*"
      (princ (format "Source: %s\nRoot: %s\nPython: %s\n\n" source root selection))
      (dolist (tool '("quarto" "pyright-langserver" "R" "rg" "git"))
        (princ (format "%s: %s\n" tool (or (executable-find tool) "MISSING"))))
      (princ "\nSet awc-project-python in .dir-locals.el for an external environment.\n")
      (princ "Existing REPLs and terminals keep their original environment.\n")
      (princ "After changing selection: normal-mode; restart LSP and Quarto preview.\n"))))

(defun awc-project-compile (command &optional require-python)
  "Run shell COMMAND at the project root with its selected environment.
REQUIRE-PYTHON rejects missing Python environments.  This is an explicitly
invoked shell command, so the usual shell syntax is supported."
  (interactive (list (read-shell-command "Project command: " compile-command)))
  (let* ((python (awc-project-python nil (not require-python)))
         (name (format "*%s*" (awc-project-process-name "Project command" python)))
         (root (awc-project-root))
         (default-directory root)
         (compilation-environment (append (awc-project-environment python) compilation-environment)))
    (save-some-buffers nil (lambda () (and buffer-file-name
                                          (file-in-directory-p buffer-file-name root))))
    (compilation-start command 'compilation-mode (lambda (_) name))))

(defun awc-project-terminal ()
  "Open or reuse a vterm per project and selected environment."
  (interactive)
  (require 'vterm)
  (let* ((python (awc-project-python nil t))
         (name (format "*%s*" (awc-project-process-name "Terminal" python)))
         (default-directory (awc-project-root))
         (process-environment (append (awc-project-environment python) process-environment)))
    (if-let* ((buffer (get-buffer name))
              (_ (process-live-p (get-buffer-process buffer))))
        (pop-to-buffer buffer)
      (when-let* ((buffer (get-buffer name)))
        (with-current-buffer buffer (rename-buffer (generate-new-buffer-name (concat name " finished")))))
      (let ((buffer (vterm name)))
        (with-current-buffer buffer
          (setq-local awc-project-python python)
          (setq-local header-line-format (concat " Project: " default-directory " Python: " (or python "none"))))
        buffer))))

(provide 'awc-project)
;;; awc-project.el ends here

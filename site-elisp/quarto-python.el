;;; quarto-python.el --- Project Python for Quarto previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Choose an interpreter per document without activating a global pyvenv.
;; Pass it to both Quarto's Jupyter engine and knitr's reticulate bridge.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)

(defcustom quarto-preview-python nil
  "Python executable for this project's Quarto previews, or nil to detect it.
Detection searches for .venv then venv, from the document directory up to
the project root.  A relative explicit path is resolved from that root.
Set this in .dir-locals.el for environments stored outside the project.
Use C-u C-c C-p to restart an existing preview after changing it."
  :group 'quarto
  :type '(choice (const :tag "Detect project virtual environment" nil) file))
(make-variable-buffer-local 'quarto-preview-python)

(defun quarto-preview--project-root (directory)
  "Find the project boundary for DIRECTORY.
Prefer a repository or Python project over a nested Quarto project,
such as reports/_quarto.yml with .venv at the repository root."
  (file-name-as-directory
   (or (locate-dominating-file
        directory (lambda (dir)
                    (cl-some (lambda (name) (file-exists-p (expand-file-name name dir)))
                             '(".git" ".hg" "pyproject.toml" ".projectile"))))
       (locate-dominating-file
        directory (lambda (dir) (or (file-exists-p (expand-file-name "_quarto.yml" dir))
                                   (file-exists-p (expand-file-name "_quarto.yaml" dir)))))
       ;; A virtual environment can itself mark a small standalone project.
       (locate-dominating-file
        directory (lambda (dir) (or (file-directory-p (expand-file-name ".venv" dir))
                                   (file-directory-p (expand-file-name "venv" dir)))))
       directory)))

(defun quarto-preview--python-chunks-p ()
  "Whether the current source contains executable Python chunks."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (re-search-forward "^[ \t]*\\(?:```+\\|~~~+\\){python\\(?:[ ,}]\\)" nil t)))))

(defun quarto-preview--python (file)
  "Select a Python executable for FILE in the current source buffer.
R-only documents need no Python environment.  Python documents require
a project environment or an explicit `quarto-preview-python' setting."
  (when (or quarto-preview-python (quarto-preview--python-chunks-p))
    (let* ((directory (file-name-directory (expand-file-name file)))
           (root (quarto-preview--project-root directory))
           (python (when quarto-preview-python
                     (expand-file-name quarto-preview-python root)))
           environment)
      (unless python
        (while (and directory (not environment))
          (setq environment
                (cl-find-if #'file-directory-p
                            (mapcar (lambda (name) (expand-file-name name directory))
                                    '(".venv" "venv"))))
          (setq directory
                (unless (equal directory root)
                  (let ((parent (file-name-directory (directory-file-name directory))))
                    (unless (equal parent directory) parent)))))
        (unless environment
          (user-error "No project Python environment; create .venv or set quarto-preview-python"))
        (setq python (expand-file-name "bin/python" environment)))
      ;; Do not resolve symlinks: .venv/bin/python often links to the system
      ;; binary, but its invoked path is what makes Python load the venv.
      (unless (and (not (file-remote-p python))
                   (file-regular-p python) (file-executable-p python))
        (user-error "Project Python is not executable: %s" python))
      python)))

(defun quarto-preview--python-environment (python)
  "Return subprocess environment overrides for PYTHON, or nil.
This does not change Emacs's global environment or other previews."
  (when python
    (let* ((bin (file-name-directory python))
           (prefix (directory-file-name (file-name-directory (directory-file-name bin))))
           (process-environment (append compilation-environment process-environment)))
      (list (concat "QUARTO_PYTHON=" python)
            (concat "RETICULATE_PYTHON=" python)
            (if (file-exists-p (expand-file-name "pyvenv.cfg" prefix))
                (concat "VIRTUAL_ENV=" prefix)
              "VIRTUAL_ENV")
            (concat "PATH=" (directory-file-name bin) path-separator (or (getenv "PATH") ""))
            ;; Prefer the selected environment's python3 kernelspec over a
            ;; user-wide installation, without registering a global kernel.
            "JUPYTER_PREFER_ENV_PATH=1"
            "PYTHONHOME"))))

(provide 'quarto-python)
;;; quarto-python.el ends here

;;; quarto-graphical-setup.el --- Private graphical startup state -*- lexical-binding: t; -*-
(defvar awc-gui-root (getenv "AWC_GUI_ROOT"))
(unless (and awc-gui-root (file-in-directory-p awc-gui-root "/tmp/"))
  (error "Graphical validation requires a disposable root"))
(defvar awc-gui-warnings nil)
(setq straight-safe-mode t org-roam-db-location (expand-file-name "notes.db" awc-gui-root))
(with-eval-after-load 'org-roam
  (setq org-roam-directory (expand-file-name "notes/" awc-gui-root))
  (make-directory org-roam-directory t))
(require 'server)
(setq server-name "disposable-gui" server-socket-dir (expand-file-name "server/" awc-gui-root))
(advice-add 'display-warning :before
            (lambda (type message &optional level &rest _)
              (push (list :type (format "%S" type) :message (format "%s" message)
                          :level (format "%S" level)) awc-gui-warnings)))

;;; init-ess.el --- Customisations for the Emacs Speaks Statistics package -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  ESS customisation for R:
;;  - Keybindings and font-lock
;;  - Better REPL display
;;  - On-demand object inspection / environment listing
;;  - httpgd-based plot viewer
;;  - Plays nicely with lsp-mode (configured in init-lsp.el)

;;; Code:

(require 'transient)  ;; used for the object inspection menu

;;;; Basic helpers

(defun awc/insert-r-pipe-operator ()
  "Insert the R native pipe operator `|>` and start a new properly indented line."
  (interactive)
  (insert " |> ")
  (reindent-then-newline-and-indent))

(defun awc/setup-ess-r-mode ()
  "Buffer-local keybindings and behaviour for `ess-r-mode`."
  ;; Assignment and pipes
  (local-set-key (kbd ";")   #'ess-insert-assign)
  (local-set-key (kbd "C-;") #'awc/insert-r-pipe-operator)

  ;; Evaluation commands, RStudio-style
  (local-set-key (kbd "C-j")   #'ess-eval-line-and-step)
  (local-set-key (kbd "C-M-j") #'ess-eval-region-and-go)

  ;; Object inspection / environment listing / plots
  (local-set-key (kbd "C-c i") #'awc/r-object-menu)           ; overrides consult-info in R buffers
  (local-set-key (kbd "C-c e") #'awc/r-env-list)
  (local-set-key (kbd "C-c v") #'awc/r-httpgd-init-and-open)  ; v = view plots

  ;; Quick toggle of ess-rdired on F9
  (local-set-key (kbd "<f9>") #'ess-rdired))

(defun awc/setup-ess-rdired-mode ()
  "Keybindings for `ess-rdired-mode`."
  ;; Use F9 to close the R dired buffer/window.
  (local-set-key (kbd "<f9>") #'kill-buffer-and-window))

(defun awc/fix-ess-repl-display ()
  "Fix display issues in the ESS REPL (strip stray carriage returns)."
  (setq-local comint-preoutput-filter-functions
              (cons (lambda (output)
                      (replace-regexp-in-string "\r" "" output))
                    comint-preoutput-filter-functions)))

(defun awc/close-ess-view-buffer ()
  "Close the `*R view*` buffer and its window, if present."
  (interactive)
  (let ((view-buffer-name "*R view*"))
    (when-let ((buf (get-buffer view-buffer-name)))
      (when-let ((win (get-buffer-window buf)))
        (quit-window nil win)))))

;;;; Object inspection and environment listing

(defun awc/r-symbol-at-point ()
  "Return the R symbol at point as a string, or nil if none."
  (when-let ((sym (symbol-at-point)))
    (symbol-name sym)))

(defun awc/r--ensure-symbol (sym)
  "Ensure SYM is non-nil; otherwise signal an error."
  (unless sym
    (user-error "No symbol at point"))
  sym)

(defun awc/r-inspect-str (sym)
  "Call `str()` on SYM in the R process."
  (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
  (ess-command (format "str(%s)\n" sym) "*R Inspect*"))

(defun awc/r-inspect-glimpse (sym)
  "Call `dplyr::glimpse()` on SYM in the R process."
  (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
  (ess-command (format "dplyr::glimpse(%s)\n" sym) "*R Inspect*"))

(defun awc/r-inspect-skim (sym)
  "Call `skimr::skim()` on SYM in the R process."
  (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
  (ess-command (format "skimr::skim(%s)\n" sym) "*R Inspect*"))

(defun awc/r-inspect-head (sym)
  "Call `head()` on SYM in the R process."
  (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
  (ess-command (format "head(%s)\n" sym) "*R Inspect*"))

(defun awc/r-inspect-summary (sym)
  "Call `summary()` on SYM in the R process."
  (interactive (list (awc/r--ensure-symbol (awc/r-symbol-at-point))))
  (ess-command (format "summary(%s)\n" sym) "*R Inspect*"))

(transient-define-prefix awc/r-object-menu ()
  "R object inspection menu."
  ["Inspect object at point"
   ("s" "str"     awc/r-inspect-str)
   ("g" "glimpse" awc/r-inspect-glimpse)
   ("k" "skim"    awc/r-inspect-skim)
   ("h" "head"    awc/r-inspect-head)
   ("m" "summary" awc/r-inspect-summary)])

(defun awc/r-env-list ()
  "List objects in the global environment with class and size in a buffer."
  (interactive)
  (let* ((cmd "as.data.frame(sapply(ls(envir = .GlobalEnv), function(x) {
  obj <- get(x, envir = .GlobalEnv)
  c(class = paste(class(obj), collapse = ','),
    type = typeof(obj),
    length = length(obj))
}), stringsAsFactors = FALSE)\n")
         (buf (get-buffer-create "*R Env*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "Objects in .GlobalEnv:\n\n"))
    (ess-command cmd buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (ess-mode)
      (read-only-mode 1))
    (display-buffer buf)))

;;;; httpgd plot viewer

(defun awc/r-httpgd-init ()
  "Start an httpgd device in the current R session."
  (interactive)
  (ess-command "if (!requireNamespace('httpgd', quietly = TRUE)) install.packages('httpgd')\n")
  (ess-command "httpgd::hgd()\n"))

(defun awc/r-httpgd-open ()
  "Open the httpgd plot viewer in xwidget if available, else in the browser."
  (interactive)
  (let ((url "http://127.0.0.1:14191"))
    (cond
     ((featurep 'xwidget-internal)
      (xwidget-webkit-browse-url url))
     (t
      (browse-url url)))))

(defun awc/r-httpgd-init-and-open ()
  "Start an httpgd device (if needed) and open the viewer."
  (interactive)
  (awc/r-httpgd-init)
  (run-with-timer 0.5 nil #'awc/r-httpgd-open))

;;;; ESS core configuration

(use-package ess
  :defer t
  :init
  (require 'ess-site)
  :hook
  ((ess-r-mode      . awc/setup-ess-r-mode)
   (ess-rdired-mode . awc/setup-ess-rdired-mode)
   (inferior-ess-mode . awc/fix-ess-repl-display))
  :custom
  (ess-eval-visibly 'nowait)
  (inferior-ess-fix-misaligned-output t)
  (ess-use-flymake nil)
  (comint-process-echoes nil)
  (comint-scroll-to-bottom-on-input t)
  (comint-move-point-for-output t)
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers  . t)
     (ess-R-fl-keyword:fun-defs   . t)
     (ess-R-fl-keyword:keywords   . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants  . t)
     (ess-fl-keyword:fun-calls    . t)
     (ess-fl-keyword:numbers      . t)
     (ess-fl-keyword:operators    . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T        . t)))
  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt     . t)
     (ess-R-fl-keyword:messages   . t)
     (ess-R-fl-keyword:modifiers  . t)
     (ess-R-fl-keyword:fun-defs   . t)
     (ess-R-fl-keyword:keywords   . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants  . t)
     (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls    . t)
     (ess-fl-keyword:numbers      . t)
     (ess-fl-keyword:operators    . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T        . t)))
  (ess-help-own-frame 'one)
  (ess-tab-complete-in-script t)
  (ess-first-tab-never-complete 'symbol-or-paren-or-punct))

;;;; Tree-sitter and polymode

(use-package tree-sitter-ess-r
  :after ess
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate))

(use-package polymode :defer t)
(use-package poly-markdown :defer t)
(use-package poly-R :defer t)

;;;; LSP for R (languageserver via eglot - currently unused)

;; You are using lsp-mode via init-lsp.el.
;; If you ever switch to eglot, this is a starting point:
;;
;; (use-package eglot
;;   :defer t
;;   :hook (ess-r-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))))

(provide 'init-ess)

;;; init-ess.el ends here

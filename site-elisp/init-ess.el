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

(require 'transient)

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

  (local-set-key (kbd "C-c C-z") #'awc-r-run)
  (local-set-key (kbd "C-c C-k") #'awc-r-interrupt)
  (local-set-key [remap R] #'awc-r-run)

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
    (when-let* ((buf (get-buffer view-buffer-name)))
      (when-let* ((win (get-buffer-window buf)))
        (quit-window nil win)))))

(transient-define-prefix awc/r-object-menu ()
  "R object inspection menu."
  ["Inspect object at point"
   ("s" "str"     awc/r-inspect-str)
   ("g" "glimpse" awc/r-inspect-glimpse)
   ("k" "skim"    awc/r-inspect-skim)
   ("h" "head"    awc/r-inspect-head)
   ("m" "summary" awc/r-inspect-summary)])

;;;; ESS core configuration

(use-package ess
  :defer t
  :init
  (require 'ess-site)
  ;; straight activates ESS before :init.  These helpers require ess-site,
  ;; so loading them above the package declaration breaks a fresh startup.
  (require 'awc-r)
  (require 'awc-r-httpgd)
  (require 'awc-r-lsp)
  :hook
  ((ess-r-mode      . awc/setup-ess-r-mode)
   (ess-r-mode      . awc-r-lsp-prepare-buffer)
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

(provide 'init-ess)

;;; init-ess.el ends here

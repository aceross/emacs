;;; Init-ess.el --- Customisations for the Emacs Speaks Statistics package
;;
;;; Commentary:
;;
;;  Mostly font and readability stuff.

;;; Code:

(defun insert-r-pipe-operator()
  "R - |> operator or 'then' pipe operator."
  (interactive
   (insert "|>")
   (reindent-then-newline-and-indent)))

  ;; Define hook functions
(defun awc/setup-ess-r-mode ()
  (local-set-key (kbd "<f9>") #'ess-rdired))

(defun awc/setup-ess-rdired-mode ()
  (local-set-key (kbd "<f9>") #'kill-buffer-and-window))

(defun my-close-ess-view-buffer ()
  "Close the *R view* buffer and its associated window."
  (interactive)
  (let ((view-buffer-name "*R view*")) ;; Adjust buffer name as needed
    (when (get-buffer view-buffer-name)
      (let ((window (get-buffer-window view-buffer-name)))
        (when window
          (quit-window nil window))))))

(use-package ess
  :defer t
  :init
  (require 'ess-site)
  :bind
  (:map ess-mode-map
        (";"   . ess-insert-assign)
        ("C-;" . insert-r-pipe-operator))
  (:map inferior-ess-mode-map
        (";"   . ess-insert-assign)
        ("C-;" . insert-r-pipe-operator))
  :custom
  (ess-eval-visibly 'nowait)
  (inferior-ess-fix-misaligned-output t)
  (ess-use-flymake nil)
  (comint-process-echoes nil)
  (comint-scroll-to-bottom-on-input t)
  (comint-move-point-for-output t)
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)))
  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t)
     (ess-R-fl-keyword:messages . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)))
  (ess-help-own-frame 'one)
  (ess-tab-complete-in-script t)
  (ess-first-tab-never-complete 'symbol-or-paren-or-punct)
  :config
  (global-set-key (kbd "C-j") 'ess-eval-line-and-step)
  (global-set-key (kbd "C-M-j") 'ess-eval-region-and-go)
    :hook
  ((ess-r-mode . awc/setup-ess-r-mode)
   (ess-rdired-mode . awc/setup-ess-rdired-mode))
  )

(use-package tree-sitter-ess-r
  :ensure t
  :after (ess)
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate))

(defun fix-ess-repl-display ()
  "Fix display issues in the ESS REPL."
  (setq-local comint-preoutput-filter-functions
              (cons (lambda (output)
                      (replace-regexp-in-string "\r" "\n" output))
                    comint-preoutput-filter-functions)))

(add-hook 'inferior-ess-mode-hook 'fix-ess-repl-display)

(use-package polymode
  :ensure t)

(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

(provide 'init-ess)

;;; init-ess.el ends here

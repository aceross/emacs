;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine).

(unless (daemonp)
  (defvar my--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun my-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'my--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist my--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'my-reset-file-handler-alist-h)
  (add-hook 'after-init-hook
            (lambda ()
              ;; restore after startup
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1))))

;; Ensure Emacs is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

;; Add my customisations files path
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))

;; Load the customisation modules with relaxed GC and file-name handlers.
(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))

  ;; ---- Core: straight.el, use-package, global tools ----
  (require 'init-core)

  ;; ---- UI: theme, fonts, modeline, tree-sitter, etc. ----
  (require 'init-ui)

  ;; ---- Text-level tools: markdown, spellcheck, citations, etc. ----
  (require 'init-text)

  ;; ---- Org: agenda, capture, org-roam, etc. ----
  (require 'init-org)

  ;; ---- LSP: lsp-mode, lsp-ui, flycheck, etc. ----
  ;; Load before language-specific modules that hook into it.
  (require 'init-lsp)

  ;; ---- Languages ----
  ;; R / ESS (uses LSP hooks defined above)
  (require 'init-ess)

  ;; Python (REPL tweaks, pyvenv, projectile integration + LSP)
  (require 'init-python)

  ;; Common Lisp (SLY, etc.)
  (require 'init-lisp)

  ;; (require 'init-julia)  ; still optional / commented

  ;; ---- Quarto polymode (R + Python chunks, render/preview helpers) ----
  (require 'quarto-mode)

  ;; ---- GPT / AI helpers ----
  (require 'init-gptel))

;;; init.el ends here

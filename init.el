;;;; init.el

(setq user-full-name "Aaron Ceross")
(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
       '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar my-packages '(ac-slime
                      airline-themes
                      ag
                      aggressive-indent
                      auto-complete
                      autopair
                      browse-kill-ring
                      cider
                      clojure-mode
                      clojure-mode-extra-font-locking
                      company-auctex
                      company-ghci
                      company-inf-ruby
                      company-irony
                      company-irony-c-headers
                      company-jedi
                      company-tern
                      company-web
                      deft
                      diminish
                      ein
                      elpy
                      fill-column-indicator
                      flx-ido
                      flycheck
                      flycheck-flow
                      flycheck-google-cpplint
                      git-gutter-fringe
                      haskell-mode
                      js2-mode
                      highlight-indentation
                      ido-ubiquitous
                      ido-vertical-mode
                      indent-guide
                      irony
                      latex-preview-pane
                      magit
                      markdown-mode
                      marmalade
                      material-theme
                      nodejs-repl
                      org
                      org-bullets
                      org-pomodoro
                      pandoc-mode
                      paredit
                      powerline
                      projectile
                      py-autopep8
                      rainbow-delimiters
                      seq
                      smex
                      sml-mode
                      smooth-scrolling
                      web-mode
                      writegood-mode
                      ws-butler
                      yaml-mode
                      ztree)
  "default packages")

(defun my-packages-installed-p ()
  (loop for pkg in my-packages
  when (not (package-installed-p pkg)) do (return nil)
  finally (return t)))

(unless (my-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;; UI Customisations ----------------------------------------------------------

;; load themes
(load-theme 'material t)

;; powerline
(require 'powerline)

;; airline modeline theme
(require 'airline-themes)
(load-theme 'airline-molokai t)
(setq airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

;; remove the splash screen
(setq inhibit-splash-screen t)

;; remove the scroll bar
(scroll-bar-mode -1)

;; remove the tool bar
(tool-bar-mode -1)

;; remove the menu
(menu-bar-mode -1)

;; set number lines globally
(global-linum-mode t)

;; provide column numbers
(setq column-number-mode t)

;; default size of the emacs window
(setq initial-frame-alist '((top . 0) (left . 0) (width . 110) (height . 40)))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; indent guide, shows a line to indicate which block currently editing
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char "|")
(set-face-foreground 'indent-guide-face "pink1")

;; highlight indentation
(require 'highlight-indentation)
(add-hook 'c-mode-hook 'highlight-indentation-mode)
(add-hook 'c++-mode-hook 'highlight-indentation-mode)
(add-hook 'emacs-lisp-mode 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'java-mode-hook 'highlight-indentation-mode)
(add-hook 'clojure-mode-hook 'highlight-indentation-mode)
(add-hook 'lisp-mode-hook 'highlight-indentation-mode)
(add-hook 'js-mode-hook 'highlight-indentation-mode)

;; delimiters in different colours
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ido vertical mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; show git diff in the buffer
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; kill blinking cursor
(blink-cursor-mode 0)

;; smooth scroll
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; show the 80-column line
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)
;; (defun auto-fci-mode (&optional unused)
;;   (if (> (window-width) fci-rule-column)
;;       (fci-mode 1)
;;    (fci-mode 0))
;;   )
;; (add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;; (add-hook 'window-configuration-change-hook 'auto-fci-mode)

;;; Editing customisations -----------------------------------------------------

;; aggressive indent mode
(global-aggressive-indent-mode 1)

;; ws-butler
(ws-butler-global-mode 1)

;; ensure EOF newline on save
(setq require-final-newline t)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; highlight current line
(global-hl-line-mode 1)

;; highlight matching parenthesis
(show-paren-mode 1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(setq tab-width 2
      indent-tabs-mode nil)

;; turn tabs to spaces on save
(add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))

;; turn off creation of backup files
(setq make-backup-files nil)

;; browse-kill-ring key binding
;; use M-y to browse kill ring
(browse-kill-ring-default-keybindings)

;; change all the yes-or-no answers to simple y or p
(defalias 'yes-or-no-p 'y-or-n-p)

;; filterable list of possible commands in minibuffer
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido
(ido-mode t)
(ido-ubiquitous-mode 1)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
                                        ; only try to match within the work directory
(setq ido-auto-merge-work-directories-length -1)


;; see all the buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile
;; (projectile-global-mode)
;; (setq projectile-enable-caching t)
;; ;; Press Super-p for fuzzy find in project
;; (global-set-key (kbd "C-x p") 'projectile-find-file)
;; Press Super-b for fuzzy switch buffer
;; (global-set-key (kbd "C-x bb") 'projectile-switch-to-buffer)

;; close delimiters automatically
(require 'autopair)
(autopair-global-mode)

;; Windmove configuration
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
  (funcall fn)))))

(global-set-key [s-left] (ignore-error-wrapper 'windmove-left))
(global-set-key [s-right] (ignore-error-wrapper 'windmove-right))
(global-set-key [s-up] (ignore-error-wrapper 'windmove-up))
(global-set-key [s-down] (ignore-error-wrapper 'windmove-down))

;; YASnippets
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-c yi") 'yas-insert-snippet)

;;;; org-mode settings ---------------------------------------------------------

(require 'org)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-odd-level-only nil)
(setq org-completion-use-ido t)
(setq org-indent-mode t)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)
(setq auto-fill-mode -1)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "IN PROGRESS" "OVERDUE" "DONE"))
      org-todo-keyword-faces
      (quote
       (("IN PROGRESS" . (:foreground "deep sky blue" :background "blue" :weight bold))
        ("OVERDUE" . (:foreground "yellow2" :background "goldenrod3" :weight bold)))))

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
    (lambda ()
      (writegood-mode)))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

;;;; Language settings ---------------------------------------------------------

;;; company-mode autocomplete
(defvar company-backends)
(require 'company-irony-c-headers)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-c-headers
                                    company-irony
                                    company-tern
                                    company-jedi
                                    company-inf-ruby
                                    company-web-html)))

;;; flycheck linting
(require 'flycheck)
(global-flycheck-mode)
(eval-after-load 'flycheck
   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  ;; '(progn
  ;;    (require 'flycheck-google-cpplint)
  ;;    ;; Add Google C++ Style checker.
  ;;    ;; In default, syntax checked by Clang and Cppcheck.
  ;;    (flycheck-add-next-checker 'c/c++-clang
  ;;                               'c/c++-googlelint 'append)))

(require 'flycheck-flow)
(add-hook 'javascript-mode-hook 'flycheck-mode)
(flycheck-add-next-checker 'javascript-gjslint 'javascript-flow)

;;; Lisp and clojure
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; SLIME
(require 'auto-complete)
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "sbcl")
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval
     (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval
     (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;;; C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (require 'company-irony-c-headers)

;;; Python
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(elpy-use-ipython)

;; use flycheck and not flymake
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;; Javascript
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;;; Haskell
(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)
;;; To get completions in the REPL
(add-hook 'haskell-interactive-mode-hook 'company-mode)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;;; LaTex
(require 'company-auctex)
(company-auctex-init)
(setq TeX-parse-self t) ; enable parse on load.
(setq TeX-auto-save t)  ; enable parse on save.

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

; add the tilde when using \cite
(setq reftex-format-cite-function
      '(lambda (key fmt)
         (let ((cite (replace-regexp-in-string "%l" key fmt)))
           (if (or (= ?~ (string-to-char fmt))
                   (member (preceding-char) '(?\ ?\t ?\n ?~)))
               cite
             (concat "~" cite)))))

;;; Custom set variables -------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(airline-display-directory (quote airline-directory-shortened))
 '(airline-shortened-directory-length 20)
 '(minimap-major-modes (quote (prog-mode)))
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (minimap
     yaml-mode
     writegood-mode
     web-mode
     sml-mode
     smex
     paredit
     nodejs-repl
     material-theme
     marmalade
     markdown-mode
     magit
     haskell-mode
     flycheck
     elpy
     deft
     clojure-mode
     autopair
     ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "dark slate gray")))))

;;; diminish -------------------------------------------------------------------

;; remove cruft from modeline
(require 'diminish)
(diminish 'projectile-mode)
(diminish 'aggressive-indent-mode)
(diminish 'flycheck-mode)
(diminish 'paredit-mode)
(diminish 'autopair-mode)
(diminish 'indent-guide-mode)
(diminish 'git-gutter-mode)
(diminish 'abbrev-mode)
(diminish 'highlight-indentation-mode)
(diminish 'org-indent-mode)
(diminish 'flyspell-mode)

;;; init.el ends here

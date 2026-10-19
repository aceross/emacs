;;; init-workflow.el --- Discoverable research commands -*- lexical-binding: t; -*-
(require 'awc-project)
(require 'transient)

;; Pyright does not autoload this command, but Transient validates every
;; suffix when opening the menu, even before a Python LSP session exists.
(autoload 'lsp-pyright-organize-imports "lsp-pyright" nil t)

(autoload 'awc-research-import-zotero "awc-research" nil t)
(autoload 'awc-research-open-note "awc-research" nil t)
(autoload 'awc-research-open-zotero "awc-research" nil t)
(autoload 'awc-research-open-pdf "awc-research" nil t)
(autoload 'awc-research-open-in-obsidian "awc-research" nil t)
(autoload 'gptel "gptel" nil t)
(autoload 'writegood-grade-level "writegood-mode" nil t)
(autoload 'writegood-reading-ease "writegood-mode" nil t)
(autoload 'writegood-mode "writegood-mode" nil t)

(transient-define-prefix awc/writing ()
  "Writing checks and contextual Org templates."
  [["Writing"
    ("g" "Grade level" writegood-grade-level)
    ("e" "Reading ease" writegood-reading-ease)
    ("w" "Toggle Writegood" writegood-mode)]
   ["Org templates"
    ("h" "Document header" org-skeleton-header :if (lambda () (derived-mode-p 'org-mode)))
    ("r" "R source block" org-skeleton-R-src :if (lambda () (derived-mode-p 'org-mode)))
    ("i" "Inline R" org-inline-R-src :if (lambda () (derived-mode-p 'org-mode)))]])

(transient-define-prefix awc/research ()
  "References, literature notes and source documents."
  [["References"
    ("z" "Find/import one Zotero item" awc-research-import-zotero)
    ("c" "Insert citation (document bibliography)" citar-insert-citation)
    ("e" "Bibliography entry" citar-open-entry)]
   ["Read / write"
    ("n" "Obsidian literature note" awc-research-open-note)
    ("N" "Markdown / legacy Org notes" citar-open-notes)
    ("p" "Local PDF" awc-research-open-pdf)
    ("Z" "Open Zotero item" awc-research-open-zotero)
    ("o" "Open current note in Obsidian" awc-research-open-in-obsidian)]])

(transient-define-prefix awc/workflow ()
  "Research project commands."
  [["Project"
    ("p" "Switch (Projectile)" projectile-switch-project)
    ("f" "Find file" project-find-file)
    ("s" "Search" consult-ripgrep)
    ("b" "Buffers" consult-project-buffer)
    ("g" "Git" magit-status)]
   ["Environment / execution"
    ("e" "Inspect environment" awc-project-info)
    ("t" "Project terminal" awc-project-terminal)
    ("c" "Project shell command" awc-project-compile)
    ("r" "Python REPL" awc-python-run)
    ("i" "Inspect Python expression" awc-python-inspect)
    ("T" "Python tests" awc-python-test)]
   ["R"
    ("L" "Project R language server" awc-r-start-lsp)
    ("R" "Project R REPL / restart" awc-r-run)
    ("E" "R session environment" awc-r-info)
    ("V" "R plots (httpgd)" awc/r-httpgd-init-and-open)
    ("I" "R objects" awc/r-env-list)]
   ["Documents / IDE"
    ("W" "Writing / Org templates" awc/writing)
    ("a" "Assistant (gptel)" gptel)
    ("n" "References / literature notes" awc/research)
    ("B" "Quarto render" quarto-build-document)
    ("v" "Quarto preview" quarto-preview-document)
    ("k" "Quarto Python / kernels" quarto-check-python)
    ("d" "Diagnostics" flycheck-list-errors)
    ("o" "Organize Python imports" lsp-pyright-organize-imports)
    ("l" "Python LSP" awc-python-start-lsp)]])

(global-set-key (kbd "C-c w") #'awc/workflow)

;; Reuse auxiliary windows without selecting them during evaluation/builds.
(add-to-list 'display-buffer-alist
             '("\\*\\(?:Python: \\|R: \\|R Inspect: \\|R Env: \\|R Environment: \\|Project command: \\|Quarto render: \\)"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . below) (window-height . 0.3)
               (inhibit-same-window . t)))

(provide 'init-workflow)
;;; init-workflow.el ends here

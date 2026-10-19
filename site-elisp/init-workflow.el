;;; init-workflow.el --- Discoverable research commands -*- lexical-binding: t; -*-
(require 'awc-project)
(require 'transient)

;; The pinned Transient compiles this availability test with static-if.
;; A prebuilt copy can assume a variable absent from the running PGTK build.
(defun awc-workflow--text-conversion-available-p (&rest _)
  (boundp 'overriding-text-conversion-style))
(when (fboundp 'transient--suspend-text-conversion-style)
  (advice-add 'transient--suspend-text-conversion-style :before-while
              #'awc-workflow--text-conversion-available-p))

;; Pyright does not autoload this command, but Transient validates every
;; suffix when opening the menu, even before a Python LSP session exists.
(autoload 'lsp-pyright-organize-imports "lsp-pyright" nil t)

(autoload 'awc-research-import-zotero "awc-research" nil t)
(autoload 'awc-research-open-note "awc-research" nil t)
(autoload 'awc-research-open-zotero "awc-research" nil t)
(autoload 'awc-research-open-pdf "awc-research" nil t)
(autoload 'awc-research-open-in-obsidian "awc-research" nil t)
(autoload 'awc-research-export-associations "awc-research-portable" nil t)
(autoload 'awc-research-import-associations "awc-research-portable" nil t)
(autoload 'awc-research-resolve-pdfs "awc-research-portable" nil t)
(autoload 'awc-research-link-local-pdf "awc-research-portable" nil t)
(autoload 'awc-bibliography-prepare "awc-bibliography" nil t)
(autoload 'awc-bibliography-refresh "awc-bibliography" nil t)
(autoload 'gptel "gptel" nil t)
(autoload 'writegood-grade-level "writegood-mode" nil t)
(autoload 'writegood-reading-ease "writegood-mode" nil t)
(autoload 'writegood-mode "writegood-mode" nil t)
(autoload 'awc-writing-check "awc-writing" nil t)
(autoload 'awc-writing-show-results "awc-writing" nil t)
(autoload 'awc-writing-open-config "awc-writing" nil t)
(autoload 'awc-writing-cancel "awc-writing" nil t)
(autoload 'awc-paper-status "awc-paper" nil t)
(autoload 'awc-paper-check "awc-paper" nil t)
(autoload 'awc-paper-verify "awc-paper" nil t)
(autoload 'awc-paper-freeze "awc-paper" nil t)
(autoload 'awc-paper-cancel "awc-paper" nil t)

(transient-define-prefix awc/manuscript ()
  "Review manuscript validation and freeze explicit checkpoints."
  [["Manuscript"
    ("s" "Status / source hashes" awc-paper-status)
    ("c" "Review and run checks" awc-paper-check)
    ("m" "Record manual verification" awc-paper-verify)
    ("f" "Review and freeze checkpoint" awc-paper-freeze)
    ("k" "Cancel checks" awc-paper-cancel)]])

(transient-define-prefix awc/writing ()
  "Writing checks and contextual Org templates."
  [["Project prose QA (Vale)"
    ("c" "Check buffer" awc-writing-check)
    ("l" "Last report / findings" awc-writing-show-results)
    ("v" "Review project rules" awc-writing-open-config)
    ("k" "Cancel check" awc-writing-cancel)]
   ["Writing"
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
   ["Project bibliography"
    ("b" "Review add/update from local snapshot" awc-bibliography-prepare)
    ("r" "Review refresh from Zotero" awc-bibliography-refresh)]
   ["Read / write"
    ("n" "Obsidian literature note" awc-research-open-note)
    ("N" "Markdown / legacy Org notes" citar-open-notes)
    ("p" "Local PDF" awc-research-open-pdf)
    ("Z" "Open Zotero item" awc-research-open-zotero)
    ("o" "Open current note in Obsidian" awc-research-open-in-obsidian)]
   ["Portable identities / device PDFs"
    ("E" "Review export associations" awc-research-export-associations)
    ("I" "Review import associations" awc-research-import-associations)
    ("f" "Resolve PDFs from local Zotero" awc-research-resolve-pdfs)
    ("P" "Link a local PDF explicitly" awc-research-link-local-pdf)]])

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
    ("M" "Manuscript / submission checkpoint" awc/manuscript)
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

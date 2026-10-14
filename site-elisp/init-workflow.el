;;; init-workflow.el --- Discoverable research commands -*- lexical-binding: t; -*-
(require 'awc-project)
(require 'transient)

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

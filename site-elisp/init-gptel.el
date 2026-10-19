;;; init-gptel.el --- AI integrations for Emacs -*- lexical-binding: nil; -*-
(use-package gptel
  :straight t
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-model "gpt-4")
  (setq gptel-default-mode 'org-mode))

(provide 'init-gptel)

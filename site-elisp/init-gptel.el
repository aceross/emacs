;; init-ai.el -- AI integrations for Emacs
(use-package gptel
  :straight t
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-model "gpt-4")
  (setq gptel-default-mode 'org-mode))

(global-set-key (kbd "C-c g") #'gptel)

(provide 'init-gptel)

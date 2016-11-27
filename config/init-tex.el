
;;;; init-tex.el
;;; For LaTex editing

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-parse-self t) ; enable parse on load.
  (setq TeX-auto-save t)  ; enable parse on save.
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  ;; add the tilde when using \cite
  (setq reftex-format-cite-function
     '(lambda (key fmt)
	(let ((cite (replace-regexp-in-string "%l" key fmt)))
	  (if (or (= ?~ (string-to-char fmt))
		  (member (preceding-char) '(?\ ?\t ?\n ?~)))
	      cite (concat "~" cite)))))
  )

(use-package company-auctex
  :defer t
  :ensure t
  :config (company-auctex-init))

(provide 'init-tex)

;;; init-tex.el ends here.

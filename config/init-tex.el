;;; init-tex.el --- Module for LaTex editing
;;
;;; Commentary:
;;

;;; Code:

(use-package reftex
  :ensure t
  :config
  ;; Enable RefTeX to find the central bibliography
  (setq reftex-default-bibliography '("~/Documents/dphil/bibliography/references.bib"))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  )

(use-package tex
  :defer t
  :ensure auctex
  :config
  (TeX-global-PDF-mode t)
  (setq TeX-parse-self t
	TeX-auto-save t
	TeX-math-close-double-dollar t)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
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
  :config
  (progn
    (push 'company-auctex-labels company-backends-LaTeX-mode)
    (push 'company-auctex-bibs company-backends-LaTeX-mode)
    (push '(company-auctex-macros
	    company-auctex-symbols
	    company-auctex-environments) company-backends-LaTeX-mode))
  (company-auctex-init)
  )

(use-package company-math
  :ensure t)


(provide 'init-tex)

;;; init-tex.el ends here

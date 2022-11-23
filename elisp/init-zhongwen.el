;;; init-chinese.el --- Chinese fonts
;;
;;; Commentary:
;;
;; The customisations in this file relate to the use of Chinese fonts
;; in my setup.
;;

;;; Code:

;; (use-package cnfonts
;;   :config (cnfonts-mode 1)
;;   )

;; Chinese fontset
(set-fontset-font t 'han (font-spec :name "Hiragino Sans GB" :size 14))

;; (if (display-graphic-p)
;;     (dolist (charset '(kana han cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset (font-spec :family "Noto Sans Mono CJK SC" :size 15))))

(provide 'init-zhongwen)

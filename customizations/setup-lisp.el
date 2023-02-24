;;; package -- Setup Lisp

;;; Commentary:
;; Things common to all lisps

;;; Code:

;; paredit
(use-package paredit
  :ensure t)

;; rainbow parens
(use-package rainbow-delimiters
  :ensure t)


;; elisp
(load "setup-elisp.el")

;; clojure
(load "setup-clojure.el")

(provide 'setup-lisp)
;;; setup-lisp.el ends here

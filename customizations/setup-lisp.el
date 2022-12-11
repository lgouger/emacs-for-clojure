;;; package -- Setup Lisp

;;; Commentary:
;; Things common to all lisps

;;; Code:

;; paredit
(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
         ("C-j" . nil)
         ("C-<return>" . paredit-newline)))

;; rainbow parens
(use-package rainbow-delimiters
  :ensure t)


;; elisp
(load "setup-elisp.el")

;; clojure
(load "setup-clojure.el")

(provide 'setup-lisp)
;;; setup-lisp.el ends here

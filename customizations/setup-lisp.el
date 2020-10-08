
;; Things common to all lisps

;; paredit
(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
         ("C-j" . nil)
         ("C-<return>" . paredit-newline)))


;; rainbow parens
(use-package rainbow-delimiters
  :ensure t)

;; ielm -- Interactive Emacs Lisp Mode
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)


;; elisp
(load "setup-elisp.el")

;; clojure
(load "setup-clojure.el")

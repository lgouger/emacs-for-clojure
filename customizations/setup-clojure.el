;;;;
;; Clojure
;;;;
(use-package clojure-mode
  :ensure t
  :mode ("\\.edn$" "\\.boot$")
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(load "setup-cider.el")

;; (use-package inf-clojure
;;   :ensure t
;;   :pin melpa-stable
;;   :hook (clojure-mode . inf-clojure-minor-mode))


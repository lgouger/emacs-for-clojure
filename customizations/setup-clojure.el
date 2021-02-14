;;; package -- Setup Clojure

;;; Commentary:

;;; Code:
(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj$" "\\.cljc$" "\\.cljs$" "\\.edn$")
  :hook
  ((clojure-mode . eldoc-mode)
   (clojure-mode . yas-minor-mode)
   (clojure-mode . paredit-mode)
   (clojure-mode . rainbow-delimiters-mode))
  :config
  (require 'flycheck-clj-kondo))

(load "setup-cider.el")

(provide 'setup-clojure)
;;; setup-clojure.el ends here

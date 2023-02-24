;;; package -- Setup Clojure

;;; Commentary:

;;; Code:
;; (use-package flycheck-clj-kondo
;;   :ensure t)

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj$" "\\.cljc$" "\\.cljs$" "\\.edn$")
  :hook
  ((clojure-mode . eldoc-mode)
   (clojure-mode . enable-paredit-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . lsp))
;;  :config
;;  (require 'clojure-mode-extra-font-locking)
  )

(use-package clj-refactor
  :ensure t
  :hook
  (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

(load "setup-cider.el")

(provide 'setup-clojure)
;;; setup-clojure.el ends here

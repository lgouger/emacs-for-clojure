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
   (clojure-mode . paredit-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (clojure-mode . lsp))
  ;; :config
  ;; (require 'flycheck-clj-kondo)
  )

;; (use-package clj-refactor
;;   :ensure t
;;   :hook
;;   (clojure-mode . clj-refactor-mode)
;;   :config
;;   (cljr-add-keybindings-with-prefix "C-c .")
;;   :diminish clj-refactor-mode)

(load "setup-cider.el")

(provide 'setup-clojure)
;;; setup-clojure.el ends here

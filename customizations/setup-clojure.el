;;; package -- Setup Clojure

;;; Commentary:

;;; Code:

;; (use-package clojure-mode
;;   :ensure t
;;   :mode ("\\.clj$" "\\.cljc$" "\\.cljs$" "\\.edn$")
;;   :hook
;;   ((clojure-mode . eldoc-mode)
;;    (clojure-mode . enable-paredit-mode)
;;    (clojure-mode . rainbow-delimiters-mode)
;;    (clojure-mode . subword-mode)
;;    (clojure-mode . eglot-ensure))
;; ;;  :config
;; ;;  (require 'clojure-mode-extra-font-locking)
;;   :bind (:map clojure-mode-map
;;               ("M-<return>" . clerk-show)))

(use-package clojure-ts-mode
  :ensure t)


(use-package clj-refactor
  :ensure t
  :hook
  (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

(load "setup-cider.el")

;; clerk related

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))


(provide 'setup-clojure)
;;; setup-clojure.el ends here

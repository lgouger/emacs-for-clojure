
(use-package clj-refactor
  :ensure t)


(use-package cider
  :ensure t
  :pin melpa-stable
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :config
  (setq nrepl-log-messages t)
  :bind (:map clojure-mode-map
              ("C-`" . 'cider-eval-expression-at-point-in-repl))
  :hook
  ((cider-repl-mode . eldoc-mode) 
   (cider-repl-mode . paredit-mode) 
   (cider-repl-mode . rainbow-delimiters-mode))

  :config
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        ))

;; A little more syntax highlighting
;; (require 'clojure-mode-extra-font-locking)


(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-sexp-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (save-selected-window
      (cider-switch-to-repl-buffer)
      (goto-char (point-max))
      (insert form)
      (cider-repl-return))))

;; Change which repl to run while editing Clojurescript files
;; (setq cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")

;; provides minibuffer documentation for the code you're typing into the repl
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ;; go right to the REPL buffer when it's finished connecting
;;   (setq cider-repl-pop-to-buffer-on-connect t)

;;   ;; When there's a cider error, show its buffer and switch to it
;;   (setq cider-show-error-buffer t)
;;   (setq cider-auto-select-error-buffer t)

;;   ;; Where to store the cider history.
;;   (setq cider-repl-history-file "~/.emacs.d/cider-history")

;;   ;; Wrap when navigating history.
;;   (setq cider-repl-wrap-history t)


;; key bindings
;; these help me out with the way I usually develop web apps
;; (defun cider-start-http-server ()
;;   (interactive)
;;   (cider-load-current-buffer)
;;   (let ((ns (cider-current-ns)))
;;     (cider-repl-set-ns ns)
;;     (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
;;     (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


;; (defun cider-refresh ()
;;   (interactive)
;;   (cider-interactive-eval (format "(user/reset)")))

;; (defun cider-user-ns ()
;;   (interactive)
;;   (cider-repl-set-ns "user"))


;;; package -- Setup Clojure

;;; Commentary:

;;; Code:

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj$" "\\.cljc$" "\\.cljs$" "\\.edn$")
  :hook
  ((clojure-ts-mode . eldoc-mode)
   (clojure-ts-mode . enable-paredit-mode)
   (clojure-ts-mode . rainbow-delimiters-mode)
   (clojure-ts-mode . subword-mode)
   (clojure-ts-mode . eglot-ensure)
   (clojure-ts-mode . company-mode))
  )


(use-package clj-refactor
  :ensure t
  :hook
  (clojure-ts-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

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

(use-package cider
  :ensure t
  ;; :pin melpa-stable
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :bind
  (:map clojure-ts-mode-map
        ("C-`" . 'cider-eval-expression-at-point-in-repl)
        ("C-~" . 'clojure-toggle-keyword-string)
        ;;        ("M-<return>" . 'cider-clerk-show)
        )
  :hook
  ((cider-repl-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)
   (cider-repl-mode . rainbow-delimiters-mode))

  :config
  (setq nrepl-log-messages t
        cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        lsp-enable-xref nil))



;; clerk related

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(provide 'setup-clojure)
;;; setup-clojure.el ends here

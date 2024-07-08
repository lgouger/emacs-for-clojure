
;; commented out on 1/4/23
;; (use-package clj-refactor
;;   :ensure t)

(use-package cider
  :ensure t
  ;; :pin melpa-stable
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :bind
  (:map clojure-mode-map
        ("C-`" . 'cider-eval-expression-at-point-in-repl)
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

;; commentted out on 1/4/23
;; (defun cider-clerk-show ()
;;   (interactive)
;;   (when-let ((filename (buffer-file-name)))
;;     (save-buffer)
;;     (cider-interactive-eval (concat "(nextjournal/show! \"" filename "\")"))))
;;
;;
;; (define-key clojure-mode-map (kbd "<M-return>") 'cider-clerk-show)


(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))


;;; package -- Setup emacs for rust development
;; 

;;; Code:

(use-package rust-ts-mode
  :ensure t
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . company-mode))
  :mode (("\\.rs\\'" . rust-ts-mode))
  :config
  (add-to-list 'exec-path "~/.cargo/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/Users/larry.gouger/.cargo/bin")))

(provide 'setup-rust)
;;; setup-rust.el ends here

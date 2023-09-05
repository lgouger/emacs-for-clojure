;;; package -- Setup emacs for rust development
;; 

;;; Code:

(use-package rust-mode
  :ensure t
  :hook (rust-mode . eglot-ensure)
  :mode ("\\.rs\\'" . rust-mode)
  :interpreter ("rust" . rust-mode)
  :config
  (setq rust-format-on-save t
        indent-tabs-mode nil)
  (prettify-symbols-mode))

(provide 'setup-rust)
;;; setup-rust.el ends here

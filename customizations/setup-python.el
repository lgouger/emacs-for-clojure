;;; package -- Setup emacs for python development

;;; Code:

(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . company-mode))
  :mode (("\\.py\\'" . python-ts-mode))
  :bind (:map python-mode-map
              ("C-<return>" . electric-newline-and-maybe-indent)
              ("M-<return>" . electric-newline-and-maybe-indent)
              ("C-/" . comment-or-uncomment-region))
  )

(use-package highlight-indent-guides
  :ensure t
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(provide 'setup-python)
;;; setup-python.el ends here

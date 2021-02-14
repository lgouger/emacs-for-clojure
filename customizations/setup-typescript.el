;;; setup-typescript -- Configuration for Typescript language

;;; Commentary -- misc

;;; Code:
(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode compant flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'setup-typescript)
;;; setup-typescript.el ends here


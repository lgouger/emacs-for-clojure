;;; package -- Setup Emacs Lisp Stuff

;;; Commentary:
;; Setup Emacs Lisp Stuff

;;; Code:
;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;; paredit for elisp
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)

(add-hook 'lisp-mode-hook #'eglot-ensure)
(add-hook 'emacs-lisp-mode-hook #'eglot-ensure)
(add-hook 'lisp-interaction-mode-hook #'eglot-ensure)
(add-hook 'eval-expression-minibuffer-setup-hook #'eglot-ensure)
(add-hook 'ielm-mode-hook #'eglot-ensure)

(add-hook 'lisp-mode-hook #'company-mode)
(add-hook 'emacs-lisp-mode-hook #'company-mode)
(add-hook 'lisp-interaction-mode-hook #'company-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'company-mode)
(add-hook 'ielm-mode-hook #'company-mode)


(provide 'setup-elisp)
;;; setup-elisp.el ends here

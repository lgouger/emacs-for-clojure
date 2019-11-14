;; python, elpy

;; (require 'elpy)
;; (require 'py-autopep8)

(add-hook 'python-mode-hook 'my-python-mode-hook t)

(defun my-python-mode-hook ()
  (elpy-enable)
  (define-key elpy-mode-map (kbd "C-<return>") 'electric-newline-and-maybe-indent)
  (define-key python-mode-map (kbd "C-<return>") 'electric-newline-and-maybe-indent)
  (define-key python-mode-map (kbd "M-<return>") 'electric-newline-and-maybe-indent)
  (define-key python-mode-map (kbd "C-/") 'comment-or-uncomment-region))

(defun flycheck-in-elpy-hook ()
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))
(add-hook 'elpy-mode-hook 'flycheck-in-elpy-hook)

;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")

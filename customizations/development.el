;;; package -- Setup emacs for software development
;;

;;; Code:

;; comment-and-uncomment region on C-/
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read)
  )

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '(("~/src/python" . 1) ("~/src/clojure" . 1)
                                         ("~/git/ghe" . 2) ("~/git/github" . 2)))
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  ;; projectile everywhere!
  (projectile-mode +1))

(use-package diminish :ensure t)
(use-package delight :ensure t)

;; company mode -- Company is a text completion framework for Emacs
(use-package company
  :ensure t
  :diminish
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.25)
  (setq company-minimum-prefix-length 2)
  (setq company-show-quick-access t)
  (setq company-show-quick-access 'left)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(require 'hi-lock)
(defun toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(global-set-key (kbd "s-.") 'toggle-mark-word-at-point)


;; For editing lisps (eg. emacs-lisp, clojure)
(load "setup-lisp.el")

;; (load "setup-js.el") ;; needs updating to use-package
;; (load "setup-typescript.el")
;; (load "setup-json.el")
;; (load "setup-perl.el")
(load "setup-python.el")

(load "setup-rust.el")

(provide 'development)
;;; development.el ends here

;; Changes all yes/no questions to y/n type
;;; (fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read))

;; (global-set-key (kbd "C-x g") 'magit-status)
;; (setq magit-completing-read-function 'magit-ido-completing-read)

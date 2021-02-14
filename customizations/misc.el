;; Changes all yes/no questions to y/n type
;;; (fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(use-package terraform-mode
  :ensure t)


(defun list-disabled-commands ()
  "Enable all commands, reporting on which were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that are disabled*"
    (mapatoms
     (function
      (lambda (symbol)
        (when (get symbol 'disabled)
          ;; (put symbol 'disabled nil)
          (prin1 symbol)
          (princ "\n")))))))


(use-package lorem-ipsum
  :ensure t
  :config
  (define-prefix-command 'lorem-ipsum-map)
  (global-set-key (kbd "C-c l") 'lorem-ipsum-map)
  (define-key lorem-ipsum-map (kbd "s") 'lorem-ipsum-insert-sentences)
  (define-key lorem-ipsum-map (kbd "p") 'lorem-ipsum-insert-paragraphs)
  (define-key lorem-ipsum-map (kbd "l") 'lorem-ipsum-insert-list))

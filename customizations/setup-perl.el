;;; cperl-mode - the best Perl mode for Emacs
;;; This file is part of the Emacs Dev Kit

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)


(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)

(defun my-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 8)
  ;; cperl-hairy affects all those variables, but I prefer
  ;; a more fine-grained approach as far as they are concerned
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens t)
  (setq cperl-electric-linefeed t)
  (setq cperl-electric-keywords t)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)

  ;; if you want all the bells and whistles
  ;; (setq cperl-hairy)

  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)
  (setq cperl-invalid-face nil)

  (set (make-local-variable 'eldoc-documentation-function)
                     'my-cperl-eldoc-documentation-function))

(defun my-cperl-eldoc-documentation-function ()
      "Return meaningful doc string for `eldoc-mode'."
      (car
       (let ((cperl-message-on-help-error nil))
         (cperl-get-help))))
    

(provide 'perl-config)

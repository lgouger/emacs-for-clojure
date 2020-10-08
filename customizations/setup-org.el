
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . ord-bullets-mode))

(setq org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-completion-use-ido t
      org-return-follows-link t
      org-outline-path-complete-in-steps nil)


(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;; (setq org-link-elisp-confirm-function 'y-or-n-p)
;; (setq org-log-done 'time)
 
;; (setq org-ellipsis "⤵")
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'ox-md)

(when (version<= "9.2" (org-version))
    (require 'org-tempo))

(setq org-todo-keywords
       '((sequence "TODO(t)" "IN PROGRESS(i)" "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("IN PROGRESS" . org-target)
        ("DONE" . org-done)
        ("BLOCKED" . org-block)
        ("CANCELED" . org-warning)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; (setq org-confirm-babel-evaluate nil)

(setq org-log-into-drawer t)
(setq org-log-done 'time)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
(setq org-archive-location "~/org/archive")



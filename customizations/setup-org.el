
(require 'org-bullets)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)))

(setq org-hide-leading-stars t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; (setq org-ellipsis "⤵")

(require 'ox-md)

(setq org-html-postamble nil)

(setq org-todo-keywords
       '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-log-into-drawer t)
(setq org-log-done 'time)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))




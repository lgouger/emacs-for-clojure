
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-hide-leading-stars t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-ellipsis "⤵")


(require 'ox-md)

(setq org-html-postamble nil)

(setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-log-into-drawer t)
(setq org-log-done 'time)

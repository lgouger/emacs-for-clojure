
(defun mb/org-narrow-to-parent ()
  "Narrow buffer to the current subtree."
  (interactive)
  (widen)
  (org-up-element)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
        (progn
          (org-back-to-heading t) (point))
        (progn (org-end-of-subtree t t)
               (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
               (point)))))))

(use-package org
  :ensure t
  :bind
  ("C-x n p" . mb/org-narrow-to-parent)
  ("C-c C-l" . org-store-link)
  :init
  (setq org-adapt-indentation t)
  ;; (setq org-adapt-indentation 'headline-data)
  :config
  (setq org-completion-use-ido t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-src-preserve-indentation t)
  (setq line-move-visual nil))

(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t
      org-outline-path-complete-in-steps nil
      org-edit-src-content-indentation 0)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-link-elisp-confirm-function 'y-or-n-p)
(setq org-log-done 'time)
 
;; (require ox-md)
;; (require ox-reveal) 

(eval-after-load 'org
  (lambda()
    ;; Clojure in orgmode stuff
    (require 'org)
    (require 'ob-async)

    (require 'ob-restclient)
    (require 'ob-sql)

    (require 'org-tempo)

    (require 'ox-gfm)
    ;; (require 'flycheck)
    ;; (require 'ob-clojure)

    ;; (require 'ob-clojure-literate)
    ;; (setq org-babel-clojure-backend 'cider)

    ;; (setq ob-clojure-literate-auto-jackin-p t)
    ;; (setq ob-clojure-literate-project-location
    ;;       (expand-file-name (concat user-emacs-directory "Org-mode/")))
    ;; (setq ob-clojure-literate-default-session "*cider-repl ob-clojure*")
    
    ;; (define-key org-babel-map (kbd "M-c") 'ob-clojure-literate-mode)
    
    ;; General config
    (setq org-startup-indented t)
    ;; Disable "ask to execute code block"  because it's annoying
    (setq org-confirm-babel-evaluate nil)))

(with-eval-after-load
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                             (sql . t)
                                                             (python . t)
                                                             (clojure . t)
                                                             (restclient . t)
                                                             (java . t)
                                                             (shell . t))))

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



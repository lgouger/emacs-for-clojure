;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

(setq default-directory (getenv "HOME"))

(use-package xref
  :ensure t)

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :init
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-menu-items 40)
  :config
  (recentf-mode 1))

(use-package ibuffer
  :init
  (setq ibuffer-saved-filter-groups
        '(("home"
	   ("emacs-config" (or (filename . ".emacs.d")
			       (filename . "emacs-config")))
	   ("Org" (mode . org-mode))
           ("code" (or (mode . python-mode)
                       (mode . java-mode)
                       (mode . clojure-mode)))
	   ("Web Dev" (or (mode . html-mode)
			  (mode . css-mode)))
	   ("Magit" (name . "\*magit"))
	   ("Help" (or (name . "\*Help\*")
		       (name . "\*Apropos\*")
		       (name . "\*info\*"))))))
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       `(ibuffer-auto-mode 1)
	       (ibuffer-switch-to-saved-filter-groups "home")))
  :bind
  ("C-x C-b" . ibuffer))

;; Prevent Control-Z from hiding the window, use C-x C-z
(global-unset-key (kbd "C-z"))

;; comment-and-uncomment region on C-/
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)


(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read))


;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '(("~/src/python" . 1) ("~/src/clojure" . 1)
                                         ("~/git/ghe" . 2) ("~/git/github" . 2)))
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  ;; projectile everywhere!
  (projectile-mode +1))

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 10)
                            (projects . 5)
                            (agenda . 10)
                            (bookmarks . 5)))
    (setq dashboard-center-content nil)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    ;; (dashboard-modify-heading-icons '((recents . "file-text")
    ;;                               (bookmarks . "book")))
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face)))
  :config
  (dashboard-setup-startup-hook))


;; from https://clojure-lsp.io/clients/
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;;(setq lsp-lens-enable t)
  (setq
   lsp-ui-doc-enable nil       ;; disable all doc popups
   lsp-ui-sideline-enable nil  ;; disable sideline bar for less distraction
   treemacs-space-between-root-nodes nil) ;; no spacing in treemacs views


  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init (progn
          (setq lsp-ui-sideline-show-code-actions nil))
  :config
  (progn
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :after (:all treemacs lsp-mode)
  :commands lsp-treemacs-errors-list)


(defun keypad-recenter (arg)
  (interactive "P")
  (if (null arg)
      (setq arg (cons nil nil)))
  (recenter arg))

(defvar numnav-mode-keymap (make-keymap) "numnav-mode keymap.")
(define-key numnav-mode-keymap (kbd "<kp-1>") 'end-of-buffer)
(define-key numnav-mode-keymap (kbd "<kp-2>") 'next-line)
(define-key numnav-mode-keymap (kbd "<kp-3>") 'scroll-up)
(define-key numnav-mode-keymap (kbd "<kp-4>") 'left-char)
(define-key numnav-mode-keymap (kbd "<C-kp-4>") 'move-beginning-of-line)
(define-key numnav-mode-keymap (kbd "<kp-5>") 'recenter-top-bottom)
(define-key numnav-mode-keymap (kbd "<kp-6>") 'right-char)
(define-key numnav-mode-keymap (kbd "<C-kp-6>") 'move-end-of-line)
(define-key numnav-mode-keymap (kbd "<kp-7>") 'beginning-of-buffer)
(define-key numnav-mode-keymap (kbd "<kp-8>") 'previous-line)
(define-key numnav-mode-keymap (kbd "<kp-9>") 'scroll-down)
(define-key numnav-mode-keymap (kbd "<kp-add>") 'compilation-next-error)
(define-key numnav-mode-keymap (kbd "<kp-subtract>") 'compilation-previous-error)

;; Custom Minor Mode
(define-minor-mode numnav-mode
  "Doc description, yada yada yada."
  ;; The initial value - Set to 1 to enable by default
  1
  ;; The indicator for the mode line.
  " #nav"
  ;; The minor mode keymap
  numnav-mode-keymap
  ;; Make mode global rather than buffer local
  :global 1
  )

(global-set-key (kbd "<clear>") 'numnav-mode)
(global-set-key (kbd "<s-mouse-1>") 'xref-find-definitions)

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package treemacs
  :ensure t
  :init
  (progn
    (setq treemacs-space-between-root-nodes        t
          treemacs-wide-toggle-width               60
          treemacs-width                           40
          treemacs-project-follow-mode             t)
    ;; (with-eval-after-load 'winum
    ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    )
  :config
  (progn
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 22)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]        . treemacs)
        ("C-<f8>"    . treemacs-select-window)
        ;; ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

(defun add-or-switch-project-dwim (dir)
  "Let elisp do a few chores & set my hands free!"
  (interactive (list (read-directory-name "Add to known projects: ")))
  (projectile-add-known-project dir)
  (find-file dir)
  (treemacs-add-and-display-current-project))

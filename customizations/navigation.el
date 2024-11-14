;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

(setq default-directory (getenv "HOME"))

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


;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


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
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ([f8]        . treemacs)
        ("C-<f8>"    . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


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

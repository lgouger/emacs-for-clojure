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

;; comment-and-uncomment region on C-/
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)


(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read))


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; (use-package smex
;;   :ensure t
;;   :init
;;   (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;   :bind ("M-x" . smex)
;;   :config (smex-initialize))

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package projectile
  :ensure t
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

(global-set-key (kbd "<S-clear>") 'numnav-mode)

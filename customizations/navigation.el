;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode 1)
(ido-everywhere 1)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers nil)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Prevent Control-Z from hiding the window, use C-x C-z
(global-unset-key (kbd "C-z"))


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; which-key on
(which-key-mode)

;; projectile everywhere!
(projectile-global-mode)

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

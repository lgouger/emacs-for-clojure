;; Customizations relating to editing a buffer.


;; TABs are the devil's whitespace
(setq-default indent-tabs-mode nil)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :init
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  ;; (setq ido-enable-flex-matching t)
  (setq ido-enable-prefix t)

  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point 'guess)

  (setq ido-file-extensions-order '(".org" ".txt" ".clj" ".py" ".java" ".json" ".el"))

  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  ;; (setq ido-auto-merge-work-directories-length -1)

  ;; Includes buffer names of recently open files, even if they're not
  ;; open now
  (setq ido-use-virtual-buffers t)

  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package crm-custom
  :ensure t
  :config
  (crm-custom-mode 1))

;; company mode -- Company is a text completion framework for Emacs
(use-package company
  :ensure t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode t))


(require 'hi-lock)
(defun toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(global-set-key (kbd "s-.") 'toggle-mark-word-at-point)


;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; undo
(global-set-key (kbd "<C-backspace>") 'undo)

;; newline and indent
(global-set-key (kbd "<s-return>") 'electric-newline-and-maybe-indent)
;; (global-set-key (kbd "<M-return>") 'electric-newline-and-maybe-indent)

;; Meta-SPACE used by Alfred app
;; (global-set-key (kbd "<M-SPC>") 'cycle-spacing)

;; The following works if Spotlight key moved elsewhere
(global-set-key (kbd "<s-SPC>") 'cycle-spacing) 

;; multiple-cursor support
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

; When you want to add multiple cursors not based on continuous lines, but based on
;; keywords in the buffer, use:

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; multi-cursor mouse
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(global-set-key (kbd "M-C-q") 'indent-sexp)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode t)

(global-set-key (kbd "<C-enter>") 'kmacro-end-and-call-macro)


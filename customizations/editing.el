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

;; load the desired interactive mode
(load "editing-vertico.el")

;; company mode -- Company is a text completion framework for Emacs
(use-package company
  :ensure t
  :diminish
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

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
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)

;; undo
(global-set-key (kbd "<C-backspace>") 'undo)

;; newline and indent
(global-set-key (kbd "<s-return>") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "<M-return>") 'electric-newline-and-maybe-indent)

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
;; (defun toggle-comment-on-line ()
;;   "comment or uncomment current line"
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; (global-set-key (kbd "C-;") 'toggle-comment-on-line)

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

(use-package string-inflection
  :ensure t
  :bind
  (:map global-map
        ("C-c i" . string-inflection-all-cycle)
        ("C-c p" . string-inflection-python-style-cycle)
        ("C-c c" . string-inflection-camelcase)
        ("C-c k" . string-inflection-kebab-case)
        ("C-c u" . string-inflection-underscore)
        ("C-c j" . string-inflection-java-style-cycle)))

(defun my-string-inflection-cycle-auto ()
  "switch inflection-cycling based on major-mode"
    (interactive)
    (cond
     ;; for ruby-mode
     ;; (foo_bar => FOO_BAR => FooBar => foo_bar)
     ((eq major-mode 'ruby-mode)
      (string-inflection-ruby-style-cycle))
     ;; for java
     ;; (fooBar  => FOO_BAR => FooBar => fooBar)
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     ;; for python
     ;; (foo_bar => FOO_BAR => FooBar => foo_bar)
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for elixir
     ;; (foo_bar => FooBar => foo_bar)
     ((eq major-mode 'elixir-mode)
      (string-inflection-elixir-style-cycle))
     (t
      ;; default
      ;; (foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar)
      (string-inflection-all-cycle))))

;; embrace -- a mode to assist with wrapping expressions
(use-package embrace
  :ensure t
  :bind
  (:map global-map
        ("C-," . embrace-commander)))


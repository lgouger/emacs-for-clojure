
;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)               ;
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :init
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t)
;;   (setq ido-enable-prefix t)

  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point 'guess)

  (setq ido-file-extensions-order '(".org" ".txt" ".clj" ".py" ".java" ".json" ".el"))

  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  (setq ido-auto-merge-work-directories-length -1)

  (setq ido-create-new-buffer 'always)
  ;; Includes buffer names of recently open files, even if they're not
  ;; open now
  (setq ido-use-virtual-buffers nil)

  :config
  (ido-mode 1)
  (ido-everywhere 1))

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; (use-package smex
;;   :ensure t
;;   :init
;;   (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;   :bind ("M-x" . smex)
;;   :config (smex-initialize))

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

;; (use-package ido-yes-or-no
;;   :ensure t
;;   :config (ido-yes-or-no-mode 1))


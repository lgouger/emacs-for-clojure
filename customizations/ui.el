;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Show line numbers
(global-display-line-numbers-mode)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; Highlight current line
(global-hl-line-mode 1)

;; Turn off the menu bar at the top of each frame because it's distracting
;; (when (fboundp 'menu-bar-mode)
;;   (menu-bar-mode))

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
   (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; swap the default modifier keys
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 200)
  ;; (set-face-attribute 'default nil :family "Hasklug Nerd Font" :height 200)
  )

(setq scroll-preserve-screen-position t)


;; (custom-set-variables
;;  '(mouse-wheel-scroll-amount (quote (1 ((shift) . 4) ((control))))))
;; (mouse-wheel-mode 1)

;; scroll one line at a time (less "jumpy" than defaults)
(when (fboundp 'mouse-wheel-mode)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-factor '(1 ((double) . 2) ((triple) . 3))) ;; comment this line too
  (setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (mouse-wheel-mode 1))

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; (load-theme 'tomorrow-night t)
;; (load-theme 'material t)
;; (load-theme 'monokai t)
;; (load-theme 'hybrid t)
;; (load-theme 'base16-monokai-dark t)
;; (load-theme 'darktooth t)
;; (load-theme `seti t)
;; (load-theme `warm-night t)

;; (use-package jetbrains-darcula-theme
;;   :config
;;   (load-theme 'jetbrains-darcula t))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-default-dark t))


;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 20) (left . 40) (width . 140) (height . 54)))
(setq initial-frame-alist '((width . 140) (height . 54)))

;; powerline setup
(use-package spaceline-config
  :config
  (setq powerline-default-separator 'utf-8)
  (spaceline-emacs-theme))

;; (use-package keycast
;;   :init
;;   (add-to-list 'global-mode-string '("" mode-line-keycast))
;;   (keycast-mode))


;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; force horizontal split for temp buffers  (top an bottom)
 ;; change to zero (0) for vertical split.
 split-width-threshold 99999

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font global
;; (menu-set-key (kbd "s-t") '(lambda () (no)))

;; interactive bell
(setq ring-bell-function 'ignore)

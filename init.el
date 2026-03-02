;;;pac.el -- setup my emacs environment

;;; Commentary:


;;; Code:
(eval-and-compile
  (setq
   package-enable-at-startup nil
   package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa"        . "https://melpa.org/packages/")))

  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ())))

(defvar byte-compile-warnings nil)

(set-language-environment "UTF-8")

(require 'package)

;; (when (< emacs-major-version 27)
;;   (package-initialize))

(defvar predicate nil)
(defvar inherit-input-method nil)



;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(use-package

     org

     ;; used by use-package
     diminish
     delight

     ;; project navigation
     projectile

     ;; Modular in-buffer completion framework for Emacs. http://company-mode.github.io
     company

     ;; completion:  tried ivy, ido, and helm
     vertico

     ;; testing related
     buttercup

     ;; linting of languages
     flycheck
     
     ;; makes handling lisp expressions much, much easier
     ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
     paredit

     ;; colorful parenthesis matching
     rainbow-delimiters

     ;; key bindings and code colorization for Clojure
     ;; https://github.com/clojure-emacs/clojure-mode
     clojure-mode

     ;; integration with a Clojure REPL
     ;; https://github.com/clojure-emacs/cider
     cider

     ;; Python mode
     elpy
     py-autopep8
     ;; pipenv

     ;; AI stuff
     eca

     ;; edit html tags like sexps
     ;; tagedit

     ;; git integration
     magit

     ;; json editing
     json-mode

     ;; yaml editing
     yaml-mode

     ;; kotlin editing
     kotlin-mode

     ;; groovy editing
     groovy-mode

     ;; Terraform and HCL
     hcl-mode
     terraform-mode

     ;; ai agent-shell mode stuff
     ;; agent-shell

     ;; REST client
     restclient
     ob-restclient

     ;; misc
     which-key
     spaceline
     ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

;; (if (eq system-type 'darwin)
;;     (add-to-list 'my-packages 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defun my-packages-installed-p ()

  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; -----

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(message "adding vendor to load-path")
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;
(use-package all-the-icons
  :ensure t)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(message "adding customizations to load-path")
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
;; (load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Hard-to-categorize customizations
(load "misc.el")

(load "setup-restclient.el")

;; for org-mode
(load "setup-org.el")

;; setup for varios programming languages
(load "development.el")

(load "ai.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
  '(org-agenda-files
   '("~/org/notes.org"
     "/Users/larry.gouger/org/work.org"
     "/Users/larry.gouger/org/home.org"))
 '(package-selected-packages
   '(agent-shell all-the-icons amx base16-theme buttercup clj-refactor
                 clojure-ts-mode company-box dashboard delight diff-hl
                 diminish eca elpy embark embrace exec-path-from-shell
                 flx-ido flycheck groovy-mode highlight-indent-guides
                 json-mode kotlin-mode lorem-ipsum marginalia
                 nerd-icons-completion ob-async ob-restclient
                 orderless org-bullets ox-gfm ox-reveal pipenv poetry
                 py-autopep8 python-black rainbow-delimiters rust-mode
                 spaceline string-inflection tagedit terraform-mode
                 treemacs-icons-dired treemacs-magit
                 treemacs-projectile treesit-auto vertico which-key
                 winum yaml-mode))
 '(package-vc-selected-packages
   '((eca :url "http://github.com/editor-code-assistant/eca-emacs"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'init)
;;; init.el ends here

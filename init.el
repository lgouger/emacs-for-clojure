;;; init.el -- setup my emacs environment

;;; Commentary:


;;; Code:
(eval-and-compile
  (setq
   package-enable-at-startup nil
   package-archives
   '(("org"          . "https://orgmode.org/elpa/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa"        . "http://melpa.org/packages/")
     ("gnu"          . "http://elpa.gnu.org/packages/") ))

  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ())))

(defvar byte-compile-warnings nil)

(require 'package)

(when (< emacs-major-version 27)
  (package-initialize))

(defvar predicate nil)
(defvar inherit-input-method nil)

(defvar lg/interactive-mode :ido)
;; (defvar lg/interactive-mode :ivy)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
      '(use-package

        org

        ;; used by use-package
        diminish
        delight

	;; Modular in-buffer completion framework for Emacs. http://company-mode.github.io
        company
    
        ;; linting of languages
        flycheck
        
	;; Python mode
        elpy
        py-autopep8
        pipenv

	;; makes handling lisp expressions much, much easier
	;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
        paredit

	;; colorful parenthesis matching
        rainbow-delimiters

	;; key bindings and code colorization for Clojure
	;; https://github.com/clojure-emacs/clojure-mode
        clojure-mode
        
	;; extra syntax highlighting for clojure
	;; clojure-mode-extra-font-locking

	;; snippets for clojure
        ;; clojure-snippets

	;; integration with a Clojure REPL
	;; https://github.com/clojure-emacs/cider
        cider

	;; Enhances M-x to allow easier execution of commands. Provides
	;; a filterable list of possible commands in the minibuffer
	;; http://www.emacswiki.org/emacs/Smex
        ;; smex

	;; project navigation
        projectile

	;; edit html tags like sexps
        tagedit

	;; git integration
        magit

	;; json editing
        json-mode

	;; yaml editing
        yaml-mode

	;; kotlin editing
        kotlin-mode

	;; Terraform and HCL
        hcl-mode
        terraform-mode

        ;; REST client
        restclient
        ob-restclient
        
        ;; trying out Language Server Protocol mode
        lsp-mode

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

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))


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
(load "shell-integration.el")

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

;; For editing lisps
(load "setup-lisp.el")

;; for org-mode
(load "setup-org.el")

(load "setup-js.el")
(load "setup-typescript.el")
;; (load "setup-json.el")
(load "setup-perl.el")
(load "setup-python.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   '(cider ob-async lsp-ui lsp-python-ms ob-graphql ob-rust ob-typescript typescript tide clj-refactor flycheck-clj-kondo ox-reveal ox-md lorem-ipsum ido-yes-or-no rg counsel mysql-to-org ob-sql-mode company-restclient ob-restclient restclient crm-custom amx ivy jetbrains-darcula-theme yaml-mode which-key use-package terraform-mode tagedit spinner spaceline-all-the-icons smex sesman rainbow-delimiters py-autopep8 projectile pipenv paredit org-bullets ob-kotlin ob-http ob-go magit kotlin-mode json-mode inf-clojure helm flycheck exec-path-from-shell elpy diminish delight base16-theme))
 '(safe-local-variable-values
   '((lsp-python-ms-python-executable concat
                                      (find-pipenv-venv-at default-directory)
                                      "/bin/python"))))
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

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


;;; PATHS

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp/" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)


;;; LIBRARIES

(require 'cl)
(require 'appearance)
(require 'behavior)
(require 'libraries)
(require 'key-bindings)
(require 'hacks)
(require 'setup-ido)
(require 'setup-apache-mode)
(require 'setup-slime)
(require 'setup-paredit)
(require 'setup-redshank)
(require 'setup-package)


;;; HOOKS

(add-hook 'makefile-mode-hook (lambda ()
                                (setq indent-tabs-mode t)))

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (enable-paredit-mode)
                                  (hs-minor-mode t)))

(add-hook 'lisp-mode-hook (lambda ()
                            (enable-paredit-mode)
                            (hs-minor-mode t)))

(add-hook 'slime-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)


;; (lambda ()
;;   (paredit-mode +1)
;;   ;; (gnp-slime-key-bindings)
;;   ;; (gnp-paredit-key-bindings)
;;   ;; (gnp-repl-key-bindings)
;;   (override-slime-repl-bindings-with-paredit))



;; ;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; ;; Lets start with a smattering of sanity
;; (require 'sane-defaults)

;; ;; Are we on a mac?
;; (setq is-mac (equal system-type 'darwin))

;; ;; Setup extensions
;; (require 'setup-ido)
;; (require 'setup-yasnippet)
;; (require 'setup-dired)
;; (require 'setup-magit)
;; (require 'setup-rgrep)
;; (require 'setup-hippie)
;; (require 'setup-ace-jump-mode)
;; (require 'setup-perspective)
;; (require 'setup-shell)
;; (require 'setup-wrap-region)
;; (require 'setup-ffip)

;; ;; Map files to modes
;; (require 'mode-mappings)

;; ;; Annoying arrows mode
;; (require 'annoying-arrows-mode)
;; ;;(global-annoying-arrows-mode)

;; ;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))
;; (require 'expand-region)
;; (require 'mark-more-like-this)
;; (require 'inline-string-rectangle)
;; (require 'multiple-cursors)
;; (require 'delsel)
;; (require 'jump-char)
;; (require 'eproject)
;; (require 'wgrep)
;; (require 'smart-forward)

;; ;; Predictive abbreviations while typing - an experiment (tab to complete)
;; (require 'pabbrev)
;; (pabbrev-mode 1)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; ;; Browse kill ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; ;; Smart M-x is smart
;; (require 'smex)
;; (smex-initialize)

;; ;; Setup key bindings
;; (require 'key-bindings)

;; ;; Misc
;; (require 'appearance)
;; (require 'misc)
;; (when is-mac (require 'mac))

;; ;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; ;; Run at full power please
;; (put 'downcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;; ;; Diminish modeline clutter
;; (require 'diminish)
;; (diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)

;; ;; Setup slime-js if it is installed
;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (when (locate-library "slime-js")
;;                 (require 'setup-slime-js))))

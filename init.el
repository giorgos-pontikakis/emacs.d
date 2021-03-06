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
(setq custom-file (expand-file-name "custom.el" site-lisp-dir))
(load custom-file)


;;; LIBRARIES

(require 'appearance)
(require 'behavior)
(require 'key-bindings)

(require 'libraries)
(require 'white)
(require 'hacks)

(require 'setup-paredit)
(require 'setup-slime)
(require 'setup-redshank)
(require 'setup-dired)
(require 'setup-org)


;;; HOOKS

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (enable-paredit-mode)
                                  (hs-minor-mode t)))

(add-hook 'lisp-mode-hook (lambda ()
                            (make-local-variable 'paredit-space-for-delimiter-predicates)
                            (add-to-list 'paredit-space-for-delimiter-predicates
                                         'paredit-space-for-delimiter-predicates-common-lisp)
                            (enable-paredit-mode)
                            (hs-minor-mode t)))

(add-hook 'js2-mode-hook (lambda ()
                           (make-local-variable 'paredit-space-for-delimiter-predicates)
                           (add-to-list 'paredit-space-for-delimiter-predicates
                                        (lambda (endp delimiter)
                                          nil))
                           (enable-paredit-mode)
                           (electric-case-mode 1)))

(add-hook 'slime-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(add-hook 'dired-mode-hook '(lambda ()
                              (turn-on-gnus-dired-mode)
                              (auto-revert-mode 1)))

(add-hook 'message-mode-hook 'turn-on-orgstruct++)


(defun gnp-maybe-tramp-cleanup-all-buffers ()
  (when (fboundp 'tramp-cleanup-all-buffers)
    (tramp-cleanup-all-buffers)))

(add-to-list 'kill-emacs-hook 'gnp-maybe-tramp-cleanup-all-buffers)



;; EMACS SERVER

(when (and (display-graphic-p)
           (not (eq system-type 'windows-nt)))
  (require 'server)
  (unless (server-running-p)
    (server-start)))




;; ;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; ;; Setup extensions
;; (require 'setup-perspective)
;; (require 'setup-shell)
;; (require 'setup-wrap-region)
;; (require 'setup-ffip)

;; ;; Map files to modes
;; (require 'mode-mappings)

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
;; (require 'smart-forward)

;; ;; Predictive abbreviations while typing - an experiment (tab to complete)
;; (require 'pabbrev)
;; (pabbrev-mode 1)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; ;; Setup slime-js if it is installed
;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (when (locate-library "slime-js")
;;                 (require 'setup-slime-js))))

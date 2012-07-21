
;;; BUILT-IN LIBRARIES OR PACKAGES FROM REPOS

;;; package module
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; common lisp extensions
(require 'cl)

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-strip-common-suffix nil)

;;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; tramp
(setq tramp-default-method "ssh")
(setq tramp-copy-size-limit 1024)

;;; remember
(require 'remember)

;;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; wgrep
(require 'wgrep)

(provide 'libraries)

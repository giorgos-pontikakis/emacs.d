
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

;;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; Apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("vhosts\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;; Misc from ELPA
(require 'remember)
(require 'wgrep)
(autoload 'regex-tool "regex-tool" t)
(require 'redo+)
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)




(provide 'libraries)

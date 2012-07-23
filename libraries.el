
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

;;; yasnippet
(require 'yasnippet)
(setq yas/root-directory '("~/.emacs.d/snippets" "~/.emacs.d/elpa/yasnippet-0.6.1/snippets"))
(mapc #'yas/load-directory yas/root-directory)
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt
                             yas/no-prompt))
(add-hook 'snippet-mode-hook (lambda ()
                               ;; disable refill mode inherited from text-mode-hook
                               (refill-mode 0)))


;;; Apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("vhosts\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; ido-ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x M-x") 'execute-extended-command)


;;; Misc from ELPA
(require 'remember)
(require 'wgrep)
(autoload 'regex-tool "regex-tool" t)
(require 'redo+)
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)


(provide 'libraries)

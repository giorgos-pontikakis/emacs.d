;;; BUILT-IN LIBRARIES OR PACKAGES FROM REPOS

;;; package module
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; common lisp extensions
(require 'cl-lib)

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
(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-copy-size-limit 1024
      ido-enable-tramp-completion nil
      tramp-auto-save-directory "~/.emacs.d/auto-save-list")

;;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; yasnippet
(require 'yasnippet)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc #'yas/load-directory yas/root-directory)
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt
                             yas/no-prompt))

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
(define-key global-map (kbd "M-i") 'ace-jump-mode)

;; ido-ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x M-x") 'execute-extended-command)
(setq smex-key-advice-ignore-menu-bar t)

;;; wgrep
(require 'wgrep)
(setq wgrep-enable-key "")

;;; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; Misc from ELPA
(autoload 'regex-tool "regex-tool" t)
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)

;;; Google Translate
(require 'google-translate)
(global-set-key "\C-ct" 'google-translate-query-translate)

;;; Deft mode
(require 'deft)
(setq deft-extension "org")
(setq deft-directory "~/Dropbox/org/")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title nil)
(setq deft-auto-save-interval 0)

;;; browse-kill-ring
(require 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-quit-action 'save-and-restore)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;;; which-func
(require 'which-func)
(add-to-list 'which-func-modes 'org-mode)
(which-func-mode 1)

;;; spelling
;;; For greek, aspell-el must be present in the system
(setq-default ispell-program-name "aspell")

;;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-<right>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)


;;; js2-mode
(require 'js2-mode)
(define-key js2-mode-map (kbd "C-RET") 'js2-line-break)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(provide 'libraries)

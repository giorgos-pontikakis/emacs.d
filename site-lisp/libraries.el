;;; BUILT-IN LIBRARIES OR PACKAGES FROM REPOS

;;; package module
(require 'package)

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar gnp-theme-list '(gruber-darker-theme
                         color-theme-sanityinc-solarized
                         twilight-anti-bright-theme
                         zen-and-art-theme))

(defvar gnp-package-list '(ace-jump-mode
                           apache-mode
                           auto-indent-mode
                           browse-kill-ring
                           deft
                           erlang
                           expand-region
                           find-file-in-project
                           google-translate
                           goto-last-change
                           ido-ubiquitous
                           js2-mode
                           jump-char
                           magit
                           multiple-cursors
                           regex-tool
                           paredit
                           redshank
                           smartparens
                           dash
                           smex
                           undo-tree
                           wgrep
                           yasnippet
                           tagedit
                           less-css-mode
                           angular-snippets
                           zencoding-mode
                           electric-case))

(when (not package-archive-contents)
  (package-refresh-contents))

(mapc (lambda (package)
        (unless (package-installed-p package)
          (package-install package)))
      (append gnp-theme-list gnp-package-list))



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
;; bind M-S-<return> directly because M-S-RET gets translated to M-RET
(define-key global-map (kbd "C-M-<return>") 'er/expand-region)

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
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(setq ace-jump-mode-scope 'frame)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "M-i") 'ace-jump-word-mode)
(define-key global-map (kbd "M-I") 'ace-jump-char-mode)
(define-key global-map (kbd "C-x x") 'ace-jump-mode-pop-mark)

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

;;; Regex tool
(autoload 'regex-tool "regex-tool" t)

;;; Goto last change
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
(global-set-key (kbd "C-M-<kp-0>") 'mc/mark-all-like-this)


;;; js2-mode
(require 'js2-mode)
(define-key js2-mode-map (kbd "C-<return>") 'js2-line-break)
(define-key js2-mode-map (kbd "M-j") nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq js2-bounce-indent-p nil
      js2-mode-indent-inhibit-undo nil
      js2-mode-indent-ignore-first-tab nil)

;;; subword mode
(subword-mode 1)

;;; HTML stuff
;;; tagedit mode and angular snippets (magnars)
;;; zencoding mode
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
     (define-key html-mode-map (kbd "C-(") 'tagedit-forward-barf-tag)
     (define-key html-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
     (define-key html-mode-map (kbd "M-<delete>") 'tagedit-kill)
     (require 'angular-snippets)
     (define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)
     (require 'zencoding-mode)
     (setq zencoding-indentation 2)
     (define-key html-mode-map (kbd "<backtab>") 'zencoding-expand-line)))


(provide 'libraries)

;;; -*- Mode: Emacs-Lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "snow")
 '(cua-overwrite-cursor-color "red")
 '(cua-read-only-cursor-color "snow")
 '(grep-command "grep -nH -E -e ")
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -E -e <R> {} +")
 '(hippie-expand-ignore-buffers (quote ("^ \\*.*\\*$" dired-mode REPL)))
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-cannot-complete-command (quote ido-next-match))
 '(ido-case-fold t)
 '(ido-completion-buffer "nil")
 '(ido-confirm-unique-completion t)
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history t)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".lx32fsl$" ".fasl$")))
 '(ido-max-window-height 3)
 '(ido-mode (quote both) nil (ido))
 '(ido-record-commands t)
 '(ido-show-dot-for-dired t)
 '(js-indent-level 3)
 '(js2-basic-offset 3)
 '(org-disputed-keys (quote (([(shift up)] . [(control meta up)]) ([(shift down)] . [(control meta down)]) ([(shift left)] . [(control meta left)]) ([(shift right)] . [(control meta right)]) ([(control shift right)] . [(meta n)]) ([(control shift left)] . [(meta p)]))))
 '(org-replace-disputed-keys t)
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(safe-local-variable-values (quote ((Package . CL-PPCRE) (Package ITERATE :use "COMMON-LISP" :colon-mode :external) (syntax . COMMON-LISP) (Package . DRAKMA) (Package . CL-WHO) (Syntax . ANSI-Common-Lisp) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP) (Package . CL-USER) (Base . 10))))
 '(wdired-allow-to-change-permissions (quote advanced))
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(wgrep-enable-key "")
 '(yas/global-mode nil nil (yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-first-match ((t (:foreground "tomato"))))
 '(ido-only-match ((((class color)) (:foreground "tomato"))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "gold2"))))
 '(slime-repl-input-face ((t (:foreground "lime green"))))
 '(slime-repl-output-face ((t (:foreground "light slate gray"))))
 '(slime-repl-output-mouseover-face ((t (:inherit slime-repl-inputed-output-face))))
 '(slime-repl-prompt-face ((t (:foreground "cyan3"))))
 '(wgrep-delete-face ((t (:background "SlateGray1" :foreground "red" :weight bold))))
 '(wgrep-face ((t (:background "SlateGray1" :foreground "Black"))))
 '(wgrep-file-face ((t (:background "gray30" :foreground "white"))))
 '(wgrep-reject-face ((t (:foreground "hot pink")))))

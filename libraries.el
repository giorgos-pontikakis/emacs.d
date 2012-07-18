


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-strip-common-suffix nil)


;; ;;; ORG-mode
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (setq org-startup-indented t)

;; ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
;; (setq org-replace-disputed-keys t)

;; ;; Fontify org-mode code blocks
;; (setq org-src-fontify-natively t)



;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;; tramp setup
(setq tramp-default-method "ssh")
(setq tramp-copy-size-limit 1024)


;;; remember mode
(require 'remember)


(provide 'libraries)

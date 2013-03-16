;;; GENERAL KEY BINDINGS

(defun backward-up-list-mark ()
  (interactive)
  (backward-up-list)
  (mark-sexp))

(defun forward-sexp-kill-ring-save ()
  (interactive)
  (let ((beg (point)))
    (activate-mark)
    (forward-sexp)
    (kill-ring-save beg (point))
    (backward-sexp)))

(defun bracket-wrap-sexp (&optional n)
  "Wrap the following S-expression in a list.
If a prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression. As a special
case, if the point is at the end of a list, simply insert a pair
of parentheses, rather than insert a lone opening parenthesis and
then signal an error, in the interest of preserving structure."
  (interactive "P")
  (paredit-handle-sexp-errors
      (paredit-insert-pair (or n
                               (and (not (paredit-region-active-p))
                                    1))
                           ?\[ ?\]
                           'goto-char)
    (insert ?\] )
    (backward-char))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun other-window-previous ()
  (interactive)
  (other-window -1))

(defun global-key-bindings ()
  (interactive)

  ;; I don't need to kill emacs that easily -- the mnemonic is C-x REALLY QUIT
  (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-x C-c") 'delete-frame)

  ;; Reserved for the OS key binding
  (global-unset-key (kbd "M-SPC"))

  ;; Text visibility
  (define-key global-map (kbd "C-<kp-add>") 'text-scale-increase)
  (define-key global-map (kbd "C-<kp-subtract>") 'text-scale-decrease)
  (define-key global-map (kbd "M-<prior>") 'hs-hide-block)
  (define-key global-map (kbd "M-<next>") 'hs-show-block)
  (define-key global-map (kbd "M-<home>") 'hs-hide-all)
  (define-key global-map (kbd "M-<end>") 'hs-show-all)

  ;; Undo, Redo, goto-last-change
  (define-key global-map (kbd "C-z") 'undo-tree-undo)
  (define-key global-map (kbd "M-z") 'undo-tree-redo)
  (define-key global-map (kbd "C-M-z") 'undo-tree-visualize)
  (define-key global-map (kbd "C-x l") 'goto-last-change)
  (define-key global-map (kbd "C-x M-f") 'view-file)

  ;; Toggle input method
  (define-key global-map (kbd "M-j") 'toggle-input-method)

  (define-key isearch-mode-map (kbd "M-j") 'isearch-toggle-input-method)
  (define-key isearch-mode-map (kbd "S-<insert>") 'isearch-yank-kill)

  ;; Miscellaneous commands
  (define-key global-map (kbd "M-/") 'hippie-expand)
  (define-key global-map (kbd "C-x 9") 'delete-other-windows-vertically)
  (define-key global-map (kbd "C-x M-u") 'upcase-region-gr)
  (define-key global-map (kbd "C-t") 'transpose-sexps)
  (define-key global-map (kbd "C-M-t") 'transpose-chars)

  ;; Movement and region handling
  (define-key global-map (kbd "S-<backspace>") 'join-line)
  (define-key global-map (kbd "C-<tab>") 'other-window)
  (define-key global-map (kbd "M-o") 'other-window)
  (define-key global-map (kbd "C-S-<iso-lefttab>") 'other-window-previous)
  (define-key global-map (kbd "M-O") 'other-window-previous)

  (define-key global-map (kbd "C-<delete>") 'kill-word)
  (define-key global-map (kbd "M-<delete>") 'kill-sexp)
  (define-key global-map (kbd "C-<backspace>") 'backward-kill-word)
  (define-key global-map (kbd "M-<backspace>") 'backward-kill-sexp)

  (define-key global-map (kbd "C-M-<insert>") 'append-next-kill)
  (define-key global-map (kbd "M-<insert>") 'forward-sexp-kill-ring-save)
  (define-key global-map (kbd "C-S-<insert>") 'yank-pop)

  (define-key global-map (kbd "M-<left>") 'backward-sexp)
  (define-key global-map (kbd "M-<right>") 'forward-sexp)
  (define-key global-map (kbd "C-M-<left>") 'backward-sentence)
  (define-key global-map (kbd "C-M-<right>") 'forward-sentence)

  (define-key global-map (kbd "M-<up>") 'backward-up-list)
  (define-key global-map (kbd "M-<down>") 'down-list)
  (define-key global-map (kbd "C-M-<up>") 'backward-paragraph)
  (define-key global-map (kbd "C-M-<down>") 'forward-paragraph)

  (define-key global-map (kbd "S-M-<up>") 'backward-up-list-mark)
  (define-key global-map (kbd "C-x <backspace>") (lambda ()
                                                   (interactive)
                                                   (just-one-space -1)))
  (define-key global-map (kbd "C-x <delete>") 'delete-blank-lines)

  ;; Brackets
  (when (display-graphic-p)
    (define-key global-map (kbd "M-[") 'bracket-wrap-sexp))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

  ;; f1-f4: general
  (define-key global-map (kbd "<f1>") 'slime-selector)
  (define-key global-map (kbd "<f2>") 'occur)
  (define-key global-map (kbd "<f3>") 'query-replace)
  (define-key global-map (kbd "<f4>") 'isearch-forward-word)

  ;; f5-f8: find/grep
  (define-key global-map (kbd "<f5>") 'rgrep)
  (define-key global-map (kbd "<f6>") 'find-grep-dired)
  (define-key global-map (kbd "<f7>") 'find-name-dired)
  (define-key global-map (kbd "<f8>") 'find-dired)

  ;; C-<f5-f8>: programs
  (define-key global-map (kbd "M-<f5>") 'calc)
  (define-key global-map (kbd "M-<f6>") 'calendar)
  (define-key global-map (kbd "M-<f7>") 'gnus-other-frame)
  (define-key global-map (kbd "M-<f8>") 'magit-status)

  ;; C-<f5-f8>: appearance
  (define-key global-map (kbd "C-<f5>") 'refill-mode)
  (define-key global-map (kbd "C-<f6>") 'menu-bar-mode)
  (define-key global-map (kbd "C-<f7>") 'toggle-truncate-lines)
  (define-key global-map (kbd "C-<f8>") 'visual-line-mode)

  ;; f12 for buffers. f9-f11 reserved for mode-specific stuff
  (define-key global-map (kbd "<f12>") 'ido-switch-buffer)
  (define-key global-map (kbd "C-<f12>") 'kill-this-buffer)
  (define-key global-map (kbd "M-<f12>") 'revert-buffer)

  ;;  Fonts
  (define-key global-map (kbd "<C-kp-0>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-7")))

  (define-key global-map (kbd "<C-kp-1>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "-*-fixed-medium-r-semicondensed-*-13-*-*-*-*-*-*-7")))

  (define-key global-map (kbd "<C-kp-2>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*-7")))

  (define-key global-map (kbd "<C-kp-3>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "-*-fixed-medium-r-normal-*-14-*-*-*-*-*-*-7")))

  (define-key global-map (kbd "<C-kp-4>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "Droid Sans Mono-9")))

  (define-key global-map (kbd "<C-kp-5>")
    '(lambda ()
       (interactive)
       (set-face-attribute 'default nil
                           :background (face-background 'default)
                           :foreground (face-foreground 'default)
                           :font "Inconsolata-8")))

  (define-key global-map (kbd "<C-kp-8>")
    '(lambda ()
       (interactive)
       (gnp-light-colors)))

  (define-key global-map (kbd "<C-kp-9>")
    '(lambda ()
       (interactive)
       (gnp-dark-colors)))

  ;; ORG-related, global map
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c b") 'org-iswitchb)
  (define-key global-map (kbd "S-<f10>") 'org-capture)
  (define-key global-map (kbd "S-<f11>") 'org-agenda)
  (define-key global-map (kbd "S-<f12>") 'org-iswitchb)

  ;; Emacs Lisp
  (define-key lisp-interaction-mode-map (kbd "C-c <return>") 'eval-print-last-sexp)
  (define-key lisp-interaction-mode-map (kbd "<backtab>") 'completion-at-point)
  (define-key emacs-lisp-mode-map (kbd "<backtab>") 'completion-at-point))

;;; Finally, bind the keys
(global-key-bindings)

;;; These should be useful in Windows,
;;; where there is no xmodmap equivalent
(when (string-equal window-system "w32")
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

(provide 'key-bindings)

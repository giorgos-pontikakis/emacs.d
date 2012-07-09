;;; GENERAL KEY BINDINGS

(defun ensure-mark ()
  ;; make sure mark is active
  ;; test if it is active, if it isn't, set it and activate it
  (if mark-active
      nil
    (set-mark-command nil)))

(defun backward-sexp-mark ()
  (interactive)
  (ensure-mark)
  (backward-sexp))

(defun forward-sexp-mark ()
  (interactive)
  (ensure-mark)
  (forward-sexp))

(defun backward-kill-sexp ()
  (interactive)
  (backward-sexp)
  (kill-sexp))

(defun backward-up-list-mark ()
  (interactive)
  (backward-up-list)
  (mark-sexp))

(defun forward-sexp-kill-ring-save ()
  (interactive)
  (let ((beg (point)))
    (forward-sexp-mark)
    (kill-ring-save beg (point))))

(defun gnp-global-key-bindings ()
  (interactive)
  ;; --- Window Manipulation ---
  (define-key global-map (kbd "C-<kp-add>") 'text-scale-increase)
  (define-key global-map (kbd "C-<kp-subtract>") 'text-scale-decrease)
  (define-key global-map (kbd "M-<prior>") 'hs-hide-block)
  (define-key global-map (kbd "M-<next>") 'hs-show-block)
  (define-key global-map (kbd "M-<home>") 'hs-hide-all)
  (define-key global-map (kbd "M-<end>") 'hs-show-all)

  (define-key global-map (kbd "C-1") (lambda ()
                                       (interactive)
                                       (slime 'sbcl)))
  (define-key global-map (kbd "C-2") (lambda ()
                                       (interactive)
                                       (slime 'slime)))
  (define-key global-map (kbd "C-3") (lambda ()
                                       (interactive)
                                       (slime 'web)))

  ;; --- Undo, Redo, goto-last-change ---
  (define-key global-map (kbd "C-z") 'undo)
  (define-key global-map (kbd "M-z") 'redo)
  (define-key global-map (kbd "C-<") 'goto-last-change)

  ;; --- Letters --
  (define-key global-map (kbd "M-g") 'goto-line)
  (define-key global-map (kbd "M-s") 'search-forward-regexp)
  (define-key global-map (kbd "C-M-q") 'indent-region)
  (define-key global-map (kbd "M-/") 'hippie-expand)
  (define-key global-map (kbd "M-j") 'toggle-input-method)

  ;; --- Special Keys ---
  (define-key global-map (kbd "S-<backspace>") 'join-line)
  (define-key global-map (kbd "C-<tab>") 'other-window)
  (define-key global-map (kbd "C-S-<lefttab>") 'previous-multiframe-window)

  (define-key global-map (kbd "C-<delete>") 'kill-word)
  (define-key global-map (kbd "M-<delete>") 'kill-sexp)
  (define-key global-map (kbd "C-<backspace>") 'backward-kill-word)
  (define-key global-map (kbd "M-<backspace>") 'backward-kill-sexp)

  (define-key global-map (kbd "C-M-<insert>") 'append-next-kill)
  (define-key global-map (kbd "M-<insert>") 'forward-sexp-kill-ring-save)
  (define-key global-map (kbd "C-S-<insert>") 'yank-pop)

  (define-key global-map (kbd "M-<left>") 'backward-sexp)
  (define-key global-map (kbd "M-<right>") 'forward-sexp)
  (define-key global-map (kbd "M-S-<left>") 'backward-sexp-mark)
  (define-key global-map (kbd "M-S-<right>") 'forward-sexp-mark)

  (define-key global-map (kbd "M-<up>") 'backward-up-list)
  (define-key global-map (kbd "M-<down>") 'down-list)

  (define-key global-map (kbd "S-M-<up>") 'backward-up-list-mark)

  ;; --- Function Keys. ---
  ;; Warning: Combinations of F1-F12 with C and M interfere with OS shortcuts

  ;; f1-f4: general
  (define-key global-map (kbd "<f1>") 'slime-selector)
  (define-key global-map (kbd "<f2>") 'list-matching-lines)
  (define-key global-map (kbd "<f3>") 'query-replace)
  (define-key global-map (kbd "<f4>") 'make-frame)

  ;; f5-f8
  (define-key global-map (kbd "<f5>") 'calc)
  (define-key global-map (kbd "C-<f5>") 'toggle-read-only)
  (define-key global-map (kbd "<f6>") 'menu-bar-mode)
  (define-key global-map (kbd "<f7>") 'toggle-truncate-lines)
  (define-key global-map (kbd "<f8>") 'visual-line-mode)

  ;; f12 for files. f9-f11 reserved for mode-specific stuff
  (define-key global-map (kbd "<f12>") 'ido-switch-buffer)
  (define-key global-map (kbd "C-<f12>") 'kill-this-buffer)
  (define-key global-map (kbd "M-<f12>") 'revert-buffer)

  ;; --- Keypad ---
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

  (define-key global-map (kbd "<C-kp-8>")
    '(lambda ()
       (interactive)
       (gnp-light-colors)))

  (define-key global-map (kbd "<C-kp-9>")
    '(lambda ()
       (interactive)
       (gnp-dark-colors))))


(defun bracket-wrap-sexp (&optional n)
  "Wrap the following S-expression in a list.
If a prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
  a pair of parentheses, rather than insert a lone opening parenthesis
  and then signal an error, in the interest of preserving structure."
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


;;; Bind the keys
(gnp-global-key-bindings)
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
(define-key global-map (kbd "M-[") 'bracket-wrap-sexp)

;;; These should be useful in Windows,
;;; where there is no xmodmap equivalent
(when (string-equal window-system "x")
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

(provide 'key-bindings)
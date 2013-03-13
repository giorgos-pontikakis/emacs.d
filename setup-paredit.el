;;; Paredit mode setup
(autoload 'enable-paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(defun paredit-backward-up-mark ()
  (interactive)
  (paredit-backward-up)
  (mark-sexp))

(defun paredit-forward-down-mark ()
  (interactive)
  (paredit-forward-down)
  (paredit-backward-up)
  (mark-sexp))

(defun gnp-paredit-key-bindings ()
  (interactive)

  (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round)
  (define-key paredit-mode-map (kbd ")") 'paredit-close-round-and-newline)

  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)
  (define-key paredit-mode-map (kbd "M-<RET>") 'newline-and-indent)

  (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-<delete>") 'paredit-forward-kill-word)

  (define-key paredit-mode-map (kbd "M-<backspace>") 'backward-kill-sexp)
  (define-key paredit-mode-map (kbd "M-<delete>") 'kill-sexp) ;; NOT paredit-kill

  (define-key paredit-mode-map (kbd "C-M-<backspace>") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "C-M-<delete>") 'paredit-splice-sexp-killing-forward)

  ;; Extra to paredit defaults for forward/backward (C-M-f and C-M-b)
  (define-key paredit-mode-map (kbd "M-<left>") 'paredit-backward)
  (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward)
  (define-key paredit-mode-map (kbd "C-M-<left>") 'beginning-of-defun)
  (define-key paredit-mode-map (kbd "C-M-<right>") 'end-of-defun)
  ;; (define-key global-map (kbd "M-S-<left>") 'backward-sexp) ;; revert to native sexp commands
  ;; (define-key global-map (kbd "M-S-<right>") 'forward-sexp) ;; so that shift-mode-selection works

  ;; Take back M-<down> and M-<up>
  ;; Extra to paredit defaults C-M-u and C-M-d, rely on C-M-( and C-M-) for barfage
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "M-<down>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "C-M-<up>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "C-M-<down>") 'paredit-backward-down)
  (define-key paredit-mode-map (kbd "S-M-<up>") 'paredit-backward-up-mark)
  (define-key paredit-mode-map (kbd "S-M-<down>") 'paredit-forward-down-mark)

  ;; Take back C-<left> and C-<right>, rely on C-( and C-) for slurpage
  (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
  (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)

  ;; Extra to paredit defaults for barfage (C-{ and C-})
  (define-key paredit-mode-map (kbd "C-M-(") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-)") 'paredit-forward-barf-sexp))

(eval-after-load 'paredit
  '(gnp-paredit-key-bindings))

(provide 'setup-paredit)

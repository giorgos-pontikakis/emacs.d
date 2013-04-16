;;; -*- Mode: Emacs-Lisp -*-

;; Don't break lines
(setq-default truncate-lines t)

;; Parentheses
(show-paren-mode 1)
(setq blink-matching-paren nil)

;;; Global visual line mode (emacs 23), 0 for off, 1 for on
;;; I prefer off globally, it messes with the minibuffer
(global-visual-line-mode 0)

;; window appearance
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "emacs: %f" ("%b")))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode t)
  (mouse-avoidance-mode 'exile))

;; Always display line and column numbers in mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;; Display time in the mode-line
(setq display-time-24hr-format t)
(display-time-mode 1)


(defun gnp-light-colors ()
  (interactive)
  (set-face-attribute 'show-paren-match-face nil :background "grey88" :foreground "red" :weight 'bold)
  (set-face-attribute 'default nil
                      :background "grey90"
                      :foreground "black")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "blue1")
  (set-face-attribute 'font-lock-comment-face nil :foreground "orange4")
  (set-face-attribute 'font-lock-constant-face nil :foreground "brown")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "red1")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "blue1")
  (set-face-attribute 'font-lock-string-face nil :foreground "green4")
  (set-face-attribute 'font-lock-type-face nil :foreground "brown3")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "PaleVioletRed1")

  (set-face-attribute 'fringe nil :background "grey92" :foreground "grey10")
  (set-face-attribute 'header-line nil :inherit 'mode-line :background "grey10" :foreground "black")
  (set-face-attribute 'region nil :background "gray" :foreground "black"))


(setq custom-theme-directory (expand-file-name "themes" dotfiles-dir))
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;;; Set default frame properties
(setq default-frame-alist
      '((width . 100)
        (height . 60)
        (cursor-type . box)
        (vertical-scroll-bars . nil)))

(load-theme 'gnp-pastel t)


(provide 'appearance)

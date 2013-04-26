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

;;; Fonts and Themes
(defvar gnp-console-font-family
  (if (eq system-type 'windows-nt)
      "Consolas"
    "Inconsolata"))
(set-face-attribute 'default nil :family gnp-console-font-family :height 80)
(setq custom-theme-directory (expand-file-name "themes" dotfiles-dir))
(load-theme 'gnp-dark t)
;; (load-theme 'gnp-light t)


;;; Set default frame properties
(setq default-frame-alist
      '((width . 100)
        (height . 60)
        (cursor-type . box)
        (vertical-scroll-bars . nil)))


(provide 'appearance)

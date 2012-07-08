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
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode t)
  (mouse-avoidance-mode 'exile))

;; Always display line and column numbers in mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;; Display time in the mode-line
(display-time-mode t)
(setq display-time-24hr-format t)




;; (setq visible-bell t
;;       font-lock-maximum-decoration t
;;       color-theme-is-global t
;;       truncate-partial-width-windows nil)

;; (set-face-background 'region "#464740")

;; ;; Highlight current line
;; (global-hl-line-mode 1)

;; ;; Customize background color of lighlighted line
;; (set-face-background 'hl-line "#222222")

;; ;; Highlight in yasnippet
;; (set-face-background 'yas/field-highlight-face "#333399")

;; (set-face-foreground 'font-lock-warning-face "#ff6666")

;; ;; org-mode colors
;; (setq org-todo-keyword-faces
;;       '(
;;         ("INPR" . (:foreground "yellow" :weight bold))
;;         ("DONE" . (:foreground "green" :weight bold))
;;         ("IMPEDED" . (:foreground "red" :weight bold))
;;         ))


;; ;; Make zooming affect frame instead of buffers
;; (require 'zoom-frm)

(defun gnp-dark-colors ()
  (interactive)
  (set-face-attribute 'default nil
                      :background "black"
                      :foreground "white")
  (set-face-attribute 'cursor nil :background "white")
  (set-face-attribute 'show-paren-match-face nil
                      :background 'unspecified
                      :foreground "red"
                      :weight 'bold
                      :underline nil)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#637EA3")
  (set-face-attribute 'font-lock-comment-face nil :foreground "IndianRed3") ;; dimgrey
  (set-face-attribute 'font-lock-constant-face nil :foreground "MediumPurple1")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "deepskyblue")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "lightgreen")
  (set-face-attribute 'font-lock-string-face nil :foreground "LightGoldenrod2") ;; LightGoldenrod"
  (set-face-attribute 'font-lock-type-face nil :foreground "plum1")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "PaleVioletRed1")

  (set-face-attribute 'fringe nil :background "grey8" :foreground "grey 20")
  (set-face-attribute 'header-line nil :inherit 'mode-line :background "grey" :foreground "black")
  (set-face-attribute 'region nil :background "grey50" :foreground "black"))



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


;;; Set colors and font

(add-to-list 'default-frame-alist '(font . "6x13"))
;; (add-to-list 'default-frame-alist '(font . "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-7"))
(gnp-dark-colors)

(provide 'appearance)

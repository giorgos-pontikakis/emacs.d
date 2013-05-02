;;; ------------------------------------------------------------
;;; BASIC FUNCTIONALITY
;;; ------------------------------------------------------------

;;; Don't kill emacs by mistake
(setq confirm-kill-emacs 'y-or-n-p)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; ;; Auto refresh buffers
;; (global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Searches and matches should ignore case.
(setq case-fold-search t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;;; Allow scroll when searching
(setq isearch-allow-scroll t)


;;; ------------------------------------------------------------
;;; ENCODING
;;; ------------------------------------------------------------

;;; Use UTF-8 for everything
;; (setq locale-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)

(if (eq system-type (not 'windows-nt))
    (set-file-name-coding-system 'utf-8)
  (set-file-name-coding-system 'windows-1253))

(set-language-environment "Greek")
(prefer-coding-system 'utf-8)


;;; ------------------------------------------------------------
;;; CURSOR
;;; ------------------------------------------------------------

;;; Scrolling
(setq scroll-conservatively 1)

;;; Keeps the cursor in the same relative row during pgups and dwns.
(setq scroll-preserve-screen-position t)

;;; Don't hscroll unless needed
(setq hscroll-margin 1)

;;; Make cursor stay in the same column when scrolling using pgup/dn.
(defadvice scroll-up (around ewd-scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(defadvice scroll-down (around ewd-scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

;;; Don't bother entering search and replace args if the buffer is read-only.
(defadvice query-replace-read-args (before barf-if-buffer-read-only
                                           activate)
  "Signal a `buffer-read-only' error if the current buffer is
read-only."
  (barf-if-buffer-read-only))



;;; ------------------------------------------------------------
;;; MINIBUFFER
;;; ------------------------------------------------------------

;;; Ignore some extensions in find-file
(add-to-list 'completion-ignored-extensions  ".lx32fsl" ".fasl")

;;; Ignore case when completing filenames/buffer names
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; Save minibuffer history
(savehist-mode 1)


;;; ------------------------------------------------------------
;;; EDITING
;;; ------------------------------------------------------------

;;; Change pasting behavior. Normally, it pastes where the mouse
;;; is at, which is not necessarily where the cursor is. This changes
;;; things so all pastes, whether they be middle-click or C-y or menu,
;;; all paste at the cursor.
(setq mouse-yank-at-point t)

;;; Enable some commands which are disabled by default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)



;;; ------------------------------------------------------------
;;; MESSAGE
;;; ------------------------------------------------------------

(setq message-signature-file "~/.emacs.d/mail-signature")
(setq message-send-mail-function 'smtpmail-send-it)



;;; ------------------------------------------------------------
;;; W32
;;; ------------------------------------------------------------
;; (when (eq system-type 'windows-nt)
;;   (w32-register-hot-key (kbd "M-S"))
;;   (w32-register-hot-key (kbd "S-M")))



;;; ------------------------------------------------------------
;;; MODES
;;; ------------------------------------------------------------

;; Save a list of recent files visited.
;; (recentf-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
;; (winner-mode 1)



(provide 'behavior)


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

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)



;;; ------------------------------------------------------------
;;; ENCODING
;;; ------------------------------------------------------------

;;; Use UTF-8 for everything
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)
(setq current-language-environment "UTF-8")
(setq default-input-method "greek")
(prefer-coding-system 'utf-8)



;;; ------------------------------------------------------------
;;; REGION
;;; ------------------------------------------------------------

;;; Enable CUA mode for rectangle editing
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Restore shift-select-mode to T, after loading CUA mode which sets
;; this to NIL
(setq shift-select-mode t)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)



;;; ------------------------------------------------------------
;;; CURSOR
;;; ------------------------------------------------------------

;; ;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)

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

;;; indent-tabs-mode becomes buffer-local when set in any way.
(set-default 'indent-tabs-mode nil)

;;; Change pasting behavior. Normally, it pastes where the mouse
;;; is at, which is not necessarily where the cursor is. This changes
;;; things so all pastes, whether they be middle-click or C-y or menu,
;;; all paste at the cursor.
(setq mouse-yank-at-point t)

;;; Enable some commands which are disabled by default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)



;;; ------------------------------------------------------------
;;; MODES
;;; ------------------------------------------------------------

;; Save a list of recent files visited.
;; (recentf-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
;; (winner-mode 1)

;; ;; Represent undo-history as an actual tree (visualize with C-x u)
;; (setq undo-tree-mode-lighter "")
;; (require 'undo-tree)
;; (global-undo-tree-mode)

;; ;; Add marmalade to package repos
;; (eval-after-load "package"
;;   '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))


(provide 'behavior)

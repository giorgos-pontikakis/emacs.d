(require 'dired)
(require 'dired-x)


;; Reload dired after creating a directory or creating af file
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

(defadvice dired-delete-file (after revert-buffer-after-delete-file activate)
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  (revert-buffer))

;; Move to the fourth line - straight under the ".."
(defun dired-jump-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

;; Move to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))

;;; Bind the keys
;;; Extend key bindings to mimic wgrep
(define-key dired-mode-map (kbd "C-<up>") 'dired-jump-to-top)
(define-key dired-mode-map (kbd "C-<down>") 'dired-jump-to-bottom)
(define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-<up>") 'dired-jump-to-top)
     (define-key wdired-mode-map (kbd "C-<down>") 'dired-jump-to-bottom)
     (define-key wdired-mode-map (kbd "C-c C-k") 'wdired-abort-changes)
     (define-key wdired-mode-map (kbd "C-x C-s") 'wdired-finish-changes)))

(defun dired-key-bindings ()
  (interactive)
  (define-key dired-mode-map [remap toggle-read-only] nil)
  (define-key dired-mode-map (kbd "C-x M-q") 'dired-toggle-read-only))

(eval-after-load "dired"
  '(dired-key-bindings))



(provide 'setup-dired)

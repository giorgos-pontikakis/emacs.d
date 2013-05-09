(require 'dired)

;; Reload dired after creating a directory or creating af file
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

(defadvice dired-delete-file (after revert-buffer-after-delete-file activate)
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  (revert-buffer))

;; Move to the fourth line - straight under the ".."
(defun gnp-dired-jump-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

;; Move to the last file
(defun gnp-dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))

;;; Bind some keys
(define-key dired-mode-map (kbd "C-<up>") 'gnp-dired-jump-to-top)
(define-key dired-mode-map (kbd "C-<down>") 'gnp-dired-jump-to-bottom)
(define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)
(define-key dired-mode-map (kbd "M-o") nil)

;;; Extend key bindings to mimic wgrep and discard remapping of
;;; read-only-mode command to dired-toggle-read-only (see dired.el)
(define-key dired-mode-map (kbd "C-c C-w") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map [remap read-only-mode] nil)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-<up>") 'gnp-dired-jump-to-top)
     (define-key wdired-mode-map (kbd "C-<down>") 'gnp-dired-jump-to-bottom)
     ;; mimic wgrep keys
     (define-key wdired-mode-map (kbd "C-c C-k") 'wdired-abort-changes)
     (define-key wdired-mode-map (kbd "C-x C-s") 'wdired-finish-edit)
     (define-key wdired-mode-map (kbd "C-c ESC") nil)
     (define-key wdired-mode-map (kbd "C-c C-c") nil)))

(provide 'setup-dired)

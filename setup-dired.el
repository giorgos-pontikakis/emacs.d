(require 'dired)


;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
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
(define-key dired-mode-map (kbd "M-<up>") 'dired-jump-to-top)
(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "M-<up>") 'dired-jump-to-top)
     (define-key wdired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)))


(provide 'setup-dired)

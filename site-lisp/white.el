
;;; indent-tabs-mode becomes buffer-local when set in any way.
(set-default 'indent-tabs-mode nil)
(add-hook 'makefile-mode-hook (lambda ()
                                (setq indent-tabs-mode t)))

;;; BUGFIX FOR WHITESPACE-CLEANUP; SEE
;;; http://stackoverflow.com/questions/7349487/emacs-different-tab-indent-settings-in-different-modes

;; I've had the same problem, and it seems that this is a bug in Emacs
;; (as of 24.2). Try this, using the following .emacs:

;; (setq-default indent-tabs-mode nil)
;; (add-hook 'after-save-hook 'whitespace-cleanup)

;; If you open a file, save it, and then open a Makefile, you'll have
;; the problem you described. But if you open a Makefile first, save
;; it, and then open another type of file, you'll have the opposite
;; problem: 8 spaces will be replaced by tabs.

;; The problem is that indent-tabs-mode is buffer-local, but in
;; whitespace.el it is set to a regular variable called
;; whitespace-indent-tabs-mode. Hence, the first value that's seen is
;; the one that counts.

;; Here's another workaround that solves some other problems too. Add this to your .emacs:
(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
                                      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'white)

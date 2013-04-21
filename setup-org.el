;;; capture
(setq org-capture-templates
      '(("s" "Scrooge")

        ("sb" "Bugs" entry (file+headline "~/Dropbox/org/scrooge.org" "Bugs")
         "** TODO %?
%t" :empty-lines 1)

        ("sc" "Code" entry (file+headline "~/Dropbox/org/scrooge.org" "Code")
         "** TODO %?
" :empty-lines 1)

        ("si" "Ideas"
         entry (file+headline "~/Dropbox/org/scrooge.org" "Ideas")
         "** %?
" :empty-lines 1)

        ("a" "System Administration" entry (file+headline "~/Dropbox/org/sysadmin.org" "Misc")
         "** %?
" :empty-lines 1)

        ("p" "Personal" entry (file+headline "~/Dropbox/org/personal.org" "Notes")
         "** %?
" :empty-lines 1)

        ("e" "Extherm" entry (file+headline "~/Dropbox/org/extherm.org" "Tasks")
         "** TODO %?
%t" :empty-lines 1)))

;;; Disputed keys (conflicts with CUA mode)
(setq org-replace-disputed-keys t)
(setq org-disputed-keys
      '(([(shift up)]            . [(control meta up)])
        ([(shift down)]          . [(control meta down)])
        ([(shift left)]          . [(control meta left)])
        ([(shift right)]         . [(control meta right)])
        ([(control shift right)] . [(meta n)])
        ([(control shift left)]  . [(meta p)])))

;;; Org Appearance
(setq org-src-fontify-natively t
      org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
      org-completion-use-ido t
      org-return-follows-link t
      org-cycle-separator-lines 1)

;;; Org Structure
(setq org-indent-mode nil
      org-adapt-indentation t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-yank-adjusted-subtrees t
      org-yank-folded-subtrees nil
      org-catch-invisible-edits 'show
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-M-RET-may-split-line '((headline . nil)
                                 (item     . t)
                                 (table    . t)
                                 (default  . t)))

;;; Agenda and Refile
(setq org-agenda-files '("~/Dropbox/org/"))
(setq org-refile-targets '((org-agenda-files . (:level . 1)))
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path 'file)

;;; Timestamps
(setq org-display-custom-times nil)
(setq org-time-stamp-custom-formats '("<%a %d/%m/%y>" . "<%a %d/%m/%y %H:%M>"))

;;; Misc
(setq org-archive-location ".%s_archive::"
      org-default-notes-file "~/Dropbox/org/personal.org"
      org-enforce-todo-dependencies t
      org-insert-heading-respect-content t
      org-use-speed-commands t)

(setq org-todo-keywords
      '((sequence "URGENT" "TODO" "DELAYED" "|" "DONE" "CANCELED")))

(defun gnp-org-key-bindings ()
  (interactive)
  (define-key org-mode-map (kbd "C-M-<tab>") 'org-force-cycle-archived)
  (define-key org-mode-map (kbd "C-<tab>") nil))

(eval-after-load "org"
  '(gnp-org-key-bindings))



(provide 'setup-org)

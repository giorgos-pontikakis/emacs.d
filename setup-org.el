
;;; capture
(setq org-capture-templates
      '(("s" "Scrooge")

        ("sb" "Bugs" entry (file+headline "/Dropbox/org/scrooge.org" "Bugs")
         "** %?
%t - %f" :empty-lines 1)

        ("sc" "Code" entry (file+headline "~/Dropbox/org/scrooge.org" "Code")
         "** %?
%t - %f" :empty-lines 1)

        ("si" "Ideas"
         entry (file+headline "~/Dropbox/org/scrooge.org" "Ideas")
         "** %?
%t - %f" :empty-lines 1)

        ("a" "System Administration" entry (file+headline "~/Dropbox/org/sysadmin.org" "Misc")
         "** %?
%t" :empty-lines 1)

        ("n" " Notes" entry (file+headline "~/Dropbox/org/notes.org" "Random")
         "** %?
%t" :empty-lines 1)

        ("e" "Extherm" entry (file+headline "~/Dropbox/org/extherm.org" "Tasks")
         "** %?
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
(setq org-indent-mode t
      org-startup-indented t
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

;;; Misc
(setq org-archive-location ".%s_archive::"
      org-default-notes-file "~/Dropbox/org/notes.org"
      org-enforce-todo-dependencies t
      org-insert-heading-respect-content t)



(provide 'setup-org)

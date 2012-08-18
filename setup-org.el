
;;; capture
(setq org-capture-templates
      '(("s" "Scrooge")

        ("sb" "Bugs" entry (file+headline "~/www/scrooge/documentation.org" "Bugs")
         "** %?
%t - %f" :empty-lines 1)

        ("sc" "Code" entry (file+headline "~/www/scrooge/documentation.org" "Code")
         "** %?
%t - %f" :empty-lines 1)

        ("si" "Ideas"
         entry (file+headline "~/www/scrooge/documentation.org" "Ideas")
         "** %?
%t - %f" :empty-lines 1)

        ("g" "General Notes" entry (file+headline "~/Dropbox/notes.org" "Random")
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

;;; Misc
(setq org-archive-location ".%s_archive::"
      org-default-notes-file "~/.notes"
      org-enforce-todo-dependencies t
      org-insert-heading-respect-content t)



(provide 'setup-org)

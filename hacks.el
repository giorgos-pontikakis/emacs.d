;;; Extend font-lock for lisp mode
(font-lock-add-keywords 'lisp-mode
                        '(("(\\(defpage\\(\\s_\\|\\w\\)*\\)"
                           1 font-lock-keyword-face)))



;;; Better indentation of html keywords (for CL-WHO)

(defun gnp-cl-indent (symbol indent)
  "Set the indentation of SYMBOL to INDENT."
  (put symbol 'common-lisp-indent-function
       (if (symbolp indent)
           (get indent 'common-lisp-indent-function)
         indent)))

(defvar *gnp-cl-html-symbols*
  (list :a :abbr :acronym :address :applet :area :article :aside :audio :b
        :base :basefont :bdi :bdo :big :blockquote :body :br :button :canvas
        :caption :center :cite :code :col :colgroup :command :datalist :dd
        :del :details :dfn :dir :div :dl :dt :em :embed :fieldset :figcaption
        :figure :font :footer :form :frame :frameset :h1 :h2 :h3 :h4 :h5 :h6
        :head :header :hgroup :hr :html :i :iframe :img :input :ins :keygen
        :kbd :label :legend :li :link :map :mark :menu :meta :meter :nav
        :noframes :noscript :object :ol :optgroup :option :output :p :param
        :pre :progress :q :rp :rt :ruby :s :samp :script :section ; :select
        :small :source :span :strike :strong :style :sub :summary :sup :table
        :tbody :td :textarea :tfoot :th :thead :time :title :tr :track :tt :u
        :ul :var :video :wbr))

(dolist (symbol *gnp-cl-html-symbols*)
  (gnp-cl-indent symbol '(&body)))

(defun gnp-upcase-gr (string)
  "Upcase a string and converted accented characters to
non-accented. Also take care of final sigma."
  (let ((result-string (upcase string)))
    (mapc #'(lambda (pair)
              (nsubstitute (cdr pair)
                           (car pair)
                           result-string))
          '((?\Ά . ?\Α)
            (?\Έ . ?\Ε)
            (?\Ή . ?\Η)
            (?\Ί . ?\Ι)
            (?\ΐ . ?\Ϊ)
            (?\Ό . ?\Ο)
            (?\ς . ?\Σ)
            (?\Ύ . ?\Υ)
            (?\Ώ . ?\Ω)
            (?\ΰ . ?\Ϋ)))
    result-string))

(defun gnp-upcase-region-gr (beg end)
  "Like upcase-region, but respecting capitalization rules for
the Greek language. Converts accented characters to non-accented
and takes care of the final sigma."
  (interactive "r")
  (barf-if-buffer-read-only)
  (let ((result (gnp-upcase-gr (delete-and-extract-region beg end))))
    (save-excursion (insert result))))

(defun gnp-find-alternate-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (let ((user (read-from-minibuffer "User: " "root")))
        (if (string-match (concat "^/sudo:" user "@localhost:") fname)
            (setq fname (replace-regexp-in-string
                         (concat "^/sudo:" user "@localhost:") ""
                         fname))
          (setq fname (concat "/sudo:root@localhost:" fname))))
      (find-alternate-file fname))))

(define-key global-map (kbd "C-x M-v") 'gnp-find-alternate-file-with-sudo)



;;; Window splitting

(defun gnp-split-window-sensibly (&optional window)
  "This is a copy of split-window-sensibly, modified so that it
tries to split windows first horizontally then vertically"
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'gnp-split-window-sensibly)
(setq split-width-threshold 150)


(provide 'hacks)

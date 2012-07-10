;;; Extend font-lock for lisp mode
(font-lock-add-keywords 'lisp-mode
                        '(("(\\(defpage\\(\\s_\\|\\w\\)*\\)"
                           1 font-lock-keyword-face)))

;;; Better indentation of html keywords (for CL-WHO)
(defun nm-cl-indent (symbol indent)
  "Set the indentation of SYMBOL to INDENT."
  (put symbol 'common-lisp-indent-function
       (if (symbolp indent)
           (get indent 'common-lisp-indent-function)
         indent)))
(defvar *nm-cl-html-symbols*
  (list :a :abbr :acronym :address :applet :area :article :aside :audio :b
        :base :basefont :bdi :bdo :big :blockquote :body :br :button :canvas
        :caption :center :cite :code :col :colgroup :command :datalist :dd
        :del :details :dfn :dir :div :dl :dt :em :embed :fieldset :figcaption
        :figure :font :footer :form :frame :frameset :h1 :h2 :h3 :h4 :h5 :h6
        :head :header :hgroup :hr :html :i :iframe :img :input :ins :keygen
        :kbd :label :legend :li :link :map :mark :menu :meta :meter :nav
        :noframes :noscript :object :ol :optgroup :option :output :p :param
        :pre :progress :q :rp :rt :ruby :s :samp :script :section :select
        :small :source :span :strike :strong :style :sub :summary :sup :table
        :tbody :td :textarea :tfoot :th :thead :time :title :tr :track :tt :u
        :ul :var :video :wbr))

(dolist (symbol *nm-cl-html-symbols*)
  (nm-cl-indent symbol '(&body)))


(provide 'hacks)

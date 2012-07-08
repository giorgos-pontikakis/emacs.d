;;; Using cursor color to indicate some modes. If you sometimes
;;; find yourself inadvertently overwriting some text because you
;;; are in overwrite mode but you didn't expect so, this might prove
;;; as useful to you as it is for me. It changes cursor color to
;;; indicate read-only, insert and overwrite modes:
(setq set-cursor-color-color "")
(setq set-cursor-color-buffer "")
(defun set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;;set-cursor-color is somewhat costly, so we only call it when
  ;;needed:
  (let ((color
         (if buffer-read-only (face-foreground 'default)
           (if overwrite-mode
               "red"
             (face-foreground 'default)))))
    (unless (and
             (string= color set-cursor-color-color)
             (string= (buffer-name) set-cursor-color-buffer))
      (set-cursor-color (setq set-cursor-color-color color))
      (setq set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'set-cursor-color-according-to-mode)


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
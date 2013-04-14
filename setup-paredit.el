;;; Paredit mode setup
(autoload 'enable-paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(defun gnp-paredit-key-bindings ()
  (interactive)

  ;; Paredit movement commants should mark when shifted
  (put 'paredit-forward 'CUA 'move)
  (put 'paredit-backward 'CUA 'move)
  (put 'paredit-backward-up 'CUA 'move)
  (put 'paredit-backward-down 'CUA 'move)
  (put 'paredit-forward-up 'CUA 'move)
  (put 'paredit-forward-down 'CUA 'move)

  ;; Basic keys
  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map (kbd "<") 'paredit-open-angled)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square)
  (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly)
  (define-key paredit-mode-map (kbd "M->") 'paredit-close-angled)
  (define-key paredit-mode-map (kbd ")") 'paredit-close-round-and-newline)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd "}") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd ">") 'paredit-close-angled-and-newline)

  (define-key paredit-mode-map (kbd "<return>") nil)
  (define-key lisp-mode-shared-map (kbd "<return>") 'paredit-newline)

  (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-<delete>") 'paredit-forward-kill-word)

  (define-key paredit-mode-map (kbd "M-<backspace>") 'backward-kill-sexp)
  (define-key paredit-mode-map (kbd "M-<delete>") 'kill-sexp) ;; NOT paredit-kill

  (define-key paredit-mode-map (kbd "C-M-<backspace>") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "C-M-<delete>") 'paredit-splice-sexp-killing-forward)

  ;; Extra to paredit defaults for forward/backward (C-M-f and C-M-b)
  (define-key paredit-mode-map (kbd "M-<left>") 'paredit-backward)
  (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward)
  (define-key paredit-mode-map (kbd "C-M-<left>") (lambda ()
                                                    (interactive)
                                                    (deactivate-mark)
                                                    (paredit-backward)
                                                    (paredit-backward)
                                                    (paredit-forward)))
  (define-key paredit-mode-map (kbd "C-M-<right>") (lambda ()
                                                    (interactive)
                                                    (deactivate-mark)
                                                    (paredit-forward)
                                                    (paredit-forward)
                                                    (paredit-backward)))
  ;; Take back M-<down> and M-<up>
  ;; Extra to paredit defaults C-M-u and C-M-d, rely on C-M-( and C-M-) for barfage
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "M-<down>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "C-M-<up>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "C-M-<down>") 'paredit-backward-down)

  ;; Take back C-<left> and C-<right>, rely on C-( and C-) for slurpage
  (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
  (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)

  ;; Extra to paredit defaults for barfage (C-{ and C-})
  (define-key paredit-mode-map (kbd "C-M-(") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-)") 'paredit-forward-barf-sexp))

(defvar common-lisp-octothorpe-quotation-characters '(?P))
(defvar common-lisp-octothorpe-parameter-parenthesis-characters '(?A))
(defvar common-lisp-octothorpe-parenthesis-characters '(?+ ?- ?C))

(defun paredit-space-for-delimiter-predicate-common-lisp (endp delimiter)
  (or endp
      (let ((case-fold-search t)
            (look
             (lambda (prefix characters n)
               (looking-back
                (concat prefix (regexp-opt (mapcar 'string characters)))
                (min n (point))))))
        (let ((oq common-lisp-octothorpe-quotation-characters)
              (op common-lisp-octothorpe-parenthesis-characters)
              (opp common-lisp-octothorpe-parameter-parenthesis-characters))
          (cond ((eq (char-syntax delimiter) ?\()
                 (and (not (funcall look "#" op 2))
                      (not (funcall look "#[0-9]*" opp 20))))
                ((eq (char-syntax delimiter) ?\")
                 (not (funcall look "#" oq 2)))
                (else t))))))

(eval-after-load 'paredit
  '(gnp-paredit-key-bindings))

(provide 'setup-paredit)

;;; Slime core functionality
;;; (add-to-list 'load-path "/home/gnp/lib/emacs/slime")
(setq slime-lisp-implementations
      `((sbcl ("/usr/bin/sbcl"))
        (slime ("/usr/bin/sbcl" "--core" "/home/gnp/sbcl-slime.core"))
        (web ("/usr/bin/sbcl" "--core" "/home/gnp/sbcl-web.core"))
        (clisp ("/usr/bin/clisp"))))


;;; slime via quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")

;;; Load Slime with contrib functionality
(require 'slime)
(slime-setup '(slime-fancy
               slime-asdf
               slime-banner
               slime-indentation
               slime-xref-browser))
(setq slime-complete-symbol*-fancy t)
(setq slime-display-compilation-output nil)
(setq slime-enable-evaluate-in-emacs t)
(setq slime-fuzzy-completion-in-place nil)
(setq slime-kill-without-query-p t)
(setq slime-truncate-lines t)
(setq slime-multiprocessing t)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(make-variable-buffer-local 'lisp-indent-function)
(setq lisp-indent-function 'common-lisp-indent-function)

(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 6)

(setq lisp-lambda-list-keyword-parameter-alignment t)
(setq lisp-lambda-list-keyword-alignment t)
(setq slime-header-line-p t)

(defun slime-key-bindings ()
  (interactive)
  (define-key slime-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-mode-map (kbd "<f9>") 'slime-compile-defun)
  (define-key slime-mode-map (kbd "<f10>") 'slime-compile-and-load-file)
  (define-key slime-mode-map (kbd "<f11>") 'slime-eval-last-expression)

  (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
  (define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
  (define-key slime-mode-map (kbd "C-;") 'slime-insert-balanced-comments) ;
  (define-key slime-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments))

(defun repl-key-bindings ()
  (interactive)
  (define-key slime-repl-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-repl-mode-map (kbd "M-r") nil)
  (define-key slime-repl-mode-map (kbd "M-s") nil)
  (define-key slime-repl-mode-map (kbd "M-<RET>") 'newline-and-indent)

  (define-key slime-repl-mode-map (kbd "C-;") 'slime-insert-balanced-comments)
  (define-key slime-repl-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments)

  (define-key slime-repl-mode-map (kbd "C-M-S-<up>") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "C-M-S-<down>") 'slime-repl-next-prompt))



(defun slime-faces ()
  (interactive)
  (set-face-attribute 'slime-highlight-face nil :underline "lightgreen")
  (set-face-attribute 'slime-note-face nil :underline t))


(eval-after-load 'slime
  '(progn
     (slime-key-bindings)
     (repl-key-bindings)
     (slime-faces)))

(provide 'setup-slime)
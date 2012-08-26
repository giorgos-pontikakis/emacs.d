;;; Slime core functionality
(setq slime-lisp-implementations
      `((sbcl ("/usr/bin/sbcl"))
        (slime ("/usr/bin/sbcl" "--core" "/home/gnp/sbcl-slime.core"))
        (web ("/usr/bin/sbcl" "--core" "/home/gnp/sbcl-web.core"))
        (clisp ("/usr/bin/clisp"))))

;;; Slime via quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))


;;; Load Slime with contrib functionality

(require 'slime)
(slime-setup '(slime-fancy
               slime-asdf
               slime-banner
               slime-indentation
               slime-xref-browser
               slime-compiler-notes-tree))

(setq slime-complete-symbol*-fancy t
      slime-display-compilation-output nil
      slime-enable-evaluate-in-emacs t
      slime-fuzzy-completion-in-place nil
      slime-kill-without-query-p t
      slime-truncate-lines nil
      slime-multiprocessing t
      slime-net-coding-system 'utf-8-unix
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil
      slime-header-line-p t
      slime-load-failed-fasl 'always
      slime-repl-history-remove-duplicates nil
      slime-repl-history-trim-whitespaces t
      slime-setup-autodoc-use-multiline-p t)


;;; Isolate fasl files generated from slime-compile-file

(make-directory "/tmp/slime-fasls/" t)
(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))


;;; Indentation

(make-variable-buffer-local 'lisp-indent-function)
(setq lisp-indent-function 'common-lisp-indent-function)

(setq common-lisp-style-default "modern"
      lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-parameter-indentation 4)


;;; Key bindings and faces

(defun slime-key-bindings ()
  (interactive)
  (define-key slime-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-mode-map (kbd "<f9>") 'slime-compile-defun)
  (define-key slime-mode-map (kbd "<f10>") 'slime-compile-and-load-file)
  (define-key slime-mode-map (kbd "<f11>") 'slime-eval-last-expression)

  (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
  (define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
  (define-key slime-mode-map (kbd "C-;") 'slime-insert-balanced-comments)
  (define-key slime-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments)

  (define-key sldb-mode-map (kbd "C-M-<left>") 'slime-previous-presentation)
  (define-key sldb-mode-map (kbd "C-M-<right>") 'slime-next-presentation)

  (define-key global-map (kbd "C-c 1") (lambda ()
                                         (interactive)
                                         (slime 'sbcl)))
  (define-key global-map (kbd "C-c 2") (lambda ()
                                         (interactive)
                                         (slime 'slime)))
  (define-key global-map (kbd "C-c 3") (lambda ()
                                         (interactive)
                                         (slime 'web))))

(defun repl-key-bindings ()
  (interactive)
  (define-key slime-repl-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-repl-mode-map (kbd "M-r") nil)
  (define-key slime-repl-mode-map (kbd "M-s") nil)
  (define-key slime-repl-mode-map (kbd "M-<RET>") 'newline-and-indent)

  (define-key slime-repl-mode-map (kbd "C-;") 'slime-insert-balanced-comments) ;
  (define-key slime-repl-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments)

  (define-key slime-repl-mode-map (kbd "C-M-S-<up>") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "C-M-S-<down>") 'slime-repl-next-prompt))

(defun slime-faces ()
  (interactive)
  (set-face-attribute 'slime-highlight-face nil :underline "lightgreen")
  (set-face-attribute 'slime-note-face nil :underline t))


;;; Finally load customizations

(eval-after-load 'slime
  '(progn
     (slime-key-bindings)
     (repl-key-bindings)
     (slime-faces)))

(provide 'setup-slime)

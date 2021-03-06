;;; Slime core functionality

(let ((user-homedir (if (eq system-type 'windows-nt)
                        (getenv "USERPROFILE")
                      (getenv "HOME")))
      (sbcl-bin "sbcl")
      (clisp-bin "clisp")
      (ccl-bin (if (eq system-type 'windows-nt)
                   "wx86cl64" "ccl")))
  ;; Implementations
  (setq slime-lisp-implementations
        `((sbcl (,sbcl-bin))
          (sbcl-web (,sbcl-bin "--core" ,(expand-file-name "sbcl-web.core" user-homedir)))
          (ccl (,ccl-bin))
          (ccl-web (,ccl-bin "--image-name" ,(expand-file-name "ccl-web.core" user-homedir)))
          (clisp (,clisp-bin))))

  ;; Slime via quicklisp
  (load (expand-file-name "quicklisp/slime-helper.el"
                          user-homedir)))

;;; default lisp
(setq slime-default-lisp (if (eq system-type 'windows-nt)
                             'ccl
                           'sbcl))



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

(let ((tmp-dir (expand-file-name "slime-fasls/"
                                 (if (eq system-type 'windows-nt)
                                     (getenv "TEMP")
                                     "/tmp/slime-fasls/"))))
  (make-directory tmp-dir t)
  (setq slime-compile-file-options `(:fasl-directory ,tmp-dir)))


;;; Indentation

(define-common-lisp-style "gnp"
  (:inherit "basic")
  (:variables
   (lisp-lambda-list-keyword-alignment t)
   (lisp-lambda-list-keyword-parameter-alignment t)
   (lisp-lambda-list-keyword-parameter-indentation 0)
   (lisp-loop-indent-subclauses t)))

(setq common-lisp-style-default "gnp")

(make-variable-buffer-local 'lisp-indent-function)
(setq lisp-indent-function 'common-lisp-indent-function)
(put 'iter 'common-lisp-indent-function '(&lambda 6)) ; iter modification


;;; Key bindings and faces

(defun gnp-slime-key-bindings ()
  (interactive)
  (define-key slime-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-mode-map (kbd "<f9>") 'slime-compile-defun)
  (define-key slime-mode-map (kbd "<f10>") 'slime-compile-and-load-file)
  (define-key slime-mode-map (kbd "<f11>") 'slime-eval-last-expression)

  (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
  (define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
  (define-key slime-mode-map (kbd "C-;") 'slime-insert-balanced-comments) ;
  (define-key slime-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments)

  (define-key sldb-mode-map (kbd "C-M-<left>") 'slime-previous-presentation)
  (define-key sldb-mode-map (kbd "C-M-<right>") 'slime-next-presentation)

  (define-key global-map (kbd "<f1>") 'slime-selector)
  (define-key global-map (kbd "C-c 1") (lambda ()
                                         (interactive)
                                         (slime 'sbcl)))
  (define-key global-map (kbd "C-c 2") (lambda ()
                                         (interactive)
                                         (slime 'sbcl-web)))
  (define-key global-map (kbd "C-c 3") (lambda ()
                                         (interactive)
                                         (slime 'ccl)))
  (define-key global-map (kbd "C-c 4") (lambda ()
                                         (interactive)
                                         (slime 'ccl-web)))
  (define-key global-map (kbd "C-c 5") (lambda ()
                                         (interactive)
                                         (slime 'clisp))))

(defun gnp-repl-key-bindings ()
  (interactive)
  (define-key slime-repl-mode-map (kbd "C-M-q") 'slime-reindent-defun)
  (define-key slime-repl-mode-map (kbd "M-r") nil)
  (define-key slime-repl-mode-map (kbd "M-s") nil)
  (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-newline-and-indent)
  (define-key slime-repl-mode-map (kbd "<kp-enter>") 'slime-repl-newline-and-indent)
  (define-key slime-repl-mode-map (kbd "M-RET") 'slime-repl-return)
  (define-key slime-repl-mode-map (kbd "M-<kp-enter>") 'slime-repl-return)

  (define-key slime-repl-mode-map (kbd "C-;") 'slime-insert-balanced-comments)
  (define-key slime-repl-mode-map (kbd "C-M-;") 'slime-remove-balanced-comments)

  (define-key slime-repl-mode-map (kbd "C-M-S-<up>") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "C-M-S-<down>") 'slime-repl-next-prompt))

(defun gnp-slime-faces ()
  (interactive)
  (set-face-attribute 'slime-highlight-face nil :underline "lightgreen")
  (set-face-attribute 'slime-note-face nil :underline t))


;;; Finally load customizations

(eval-after-load 'slime
  '(progn
     (gnp-slime-key-bindings)
     (gnp-slime-faces)
     (gnp-repl-key-bindings)))


(provide 'setup-slime)

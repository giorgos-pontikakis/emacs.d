(require 'redshank-loader)

(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))

(setq redshank-accessor-name-function 'identity)

(setq redshank-prefix-key "C-c C-r")

(provide 'setup-redshank)
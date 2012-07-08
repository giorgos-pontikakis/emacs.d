
;;; (autoload FUNCTION FILE &optional DOCSTRING INTERACTIVE TYPE)


(autoload 'regex-tool "regex-tool" t)

(require 'redo+)

(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)

(provide 'setup-misc)

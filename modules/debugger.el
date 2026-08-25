;; -*- lexical-binding: t; -*-

(require 'debug)


(setq debugger-stack-frame-as-list t)

(bz/keys debugger-mode-map
  "j" nil
  "l" nil
  "C-e" debugger-eval-expression
  "RET" debugger-jump)


;;; Provide

(provide 'bz-debugger)
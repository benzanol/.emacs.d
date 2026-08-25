;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-eglot)


(bz/keys java-mode-map
  :sparse t)

(bz/hook java-mode-hook bz/java-setup
  (company-mode 1))

()


;;; Provide

(provide 'bz-java)

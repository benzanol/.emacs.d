;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'rainbow-delimiters)


(bz/hook emacs-lisp-mode-hook rainbow-delimiters-mode)
(setq rainbow-delimiters-max-face-count 7)

(bz/face rainbow-delimiters-depth-1-face :fg "tomato")
(bz/face rainbow-delimiters-depth-2-face :fg orange)
(bz/face rainbow-delimiters-depth-3-face :fg yellow)
(bz/face rainbow-delimiters-depth-4-face :fg "green3")
(bz/face rainbow-delimiters-depth-5-face :fg "cyan")
(bz/face rainbow-delimiters-depth-6-face :fg "dodgerblue")
(bz/face rainbow-delimiters-depth-7-face :fg "mediumorchid2")


;;; Provide

(provide 'bz-rainbow)

(qv/package rainbow-delimiters)
(rainbow-delimiters-mode 1)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(setq rainbow-delimiters-max-face-count 7)

(qv/face rainbow-delimiters-depth-1-face :fg "tomato")
(qv/face rainbow-delimiters-depth-2-face :fg "orange")
(qv/face rainbow-delimiters-depth-3-face :fg "yellow")
(qv/face rainbow-delimiters-depth-4-face :fg "green1")
(qv/face rainbow-delimiters-depth-5-face :fg "cyan")
(qv/face rainbow-delimiters-depth-6-face :fg "royalblue2")
(qv/face rainbow-delimiters-depth-7-face :fg "mediumorchid2")

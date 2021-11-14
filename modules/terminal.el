(qv/package term)
(qv/package multi-term)

(defun terminal ()
  (interactive)
  (multi-term)
  (qv/term-setup))

(defun qv/term-setup ()
  (interactive)
  (term-char-mode)

  (setq-local winner-mode nil)
  (buffer-face-set 'fixed-pitch)
  (display-line-numbers-mode 0)

  (qv/face1 'term-color-blue nil qv/blue-color nil)
  (qv/face1 'term-color-red nil "#EF2929" nil)
  (qv/face1 'term-color-yellow nil "#FCE94F" nil)
  (qv/face1 'term-color-green nil "#8AE234" nil)
  (qv/face1 'term-color-magenta nil "#FF66BB" nil)
  (qv/face1 'term-color-black nil qv/gray3-color nil)

  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0.0)

  (define-key term-mode-map (kbd "i") 'term-char-mode)

  (define-key term-raw-map (kbd "ESC") term-raw-escape-map)
  (setq term-raw-escape-map (make-sparse-keymap))
  (dolist (i (number-sequence 1 126))
    (define-key term-raw-escape-map (vector i) 'term-send-raw-meta)
    (unless (eq i 27)
      (unbind-key (kbd (string i)) term-raw-map)
      (define-key term-raw-map (vector i) 'term-send-raw)))

  (qv/keys term-raw-map
    "<return>" term-send-return
    "C-\\" nil
    "C-\\ C-\\" term-send-raw
    "C-\\ C-n" term-line-mode
    "s-q" term-line-mode
    "C-V" term-paste
    "M-l" term-send-right
    "M-h" term-send-left
    "M-k" term-send-up
    "M-j" term-send-down
    "M-L" (dotimes (i 4) (term-send-right))
    "M-H" (dotimes (i 4) (term-send-left))
    "M-K" (dotimes (i 4) (term-send-up))
    "M-J" (dotimes (i 4) (term-send-down))
    "M-z" term-send-backspace
    "M-Z" term-send-backward-kill-word
    "M-x" term-send-del
    "M-X" term-send-forward-kill-word))

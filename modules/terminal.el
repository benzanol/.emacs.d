(qv/package term)
(qv/package multi-term)

(defun terminal ()
  (interactive)
  (multi-term)
  (qv/term-setup))

(defun qv/term-setup ()
  (interactive)
  (setq-local minor-mode-map-alist nil)
  (display-line-numbers-mode 0)
  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0.0)
  (qv/key term-raw-map "C-c" term-send-raw)
  
  (qv/face term-color-blue :fg blue)
  (qv/face term-color-red :fg "#EF2929")
  (qv/face term-color-yellow :fg "#FCE94F")
  (qv/face term-color-green :fg "#8AE234")
  (qv/face term-color-magenta :fg "#FF66BB")
  (qv/face term-color-black :fg gray3)
  
  (qv/term-keybindings)
  (qv/term-char))

(defun qv/term-char ()
  (interactive)
  (term-char-mode)

  (recenter-cursor-mode 0))

(defun qv/term-line ()
  (interactive)
  (term-line-mode)
  
  (recenter-cursor-mode 1))

(defun qv/term-keybindings ()
  (interactive)
  (qv/key term-mode-map "i" qv/term-char)
  
  (qv/key term-raw-map "ESC" ,term-raw-escape-map)
  (setcdr term-raw-escape-map nil)
  (dolist (i (number-sequence 1 126))
    (define-key term-raw-escape-map (vector i) 'term-send-raw-meta)
    (unless (eq i 27)
      (define-key term-raw-map (kbd (string i)) nil)
      (define-key term-raw-map (vector i) 'term-send-raw)))

  (qv/keys term-raw-map
    "s-q" qv/term-line
    "RET" term-send-return
    "C-\\" nil
    "C-\\ C-\\" term-send-raw
    "C-\\ C-n" qv/term-line
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

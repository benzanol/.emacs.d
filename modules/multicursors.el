(qv/package multiple-cursors)

(setq mc/always-run-for-all t)
(setq mc/insert-numbers-default 1)

(qv/keys mc/keymap
  :sparse t
  "C-g" mc/keyboard-quit
  "C-n" mc/mark-next-like-this
  "C-p" mc/unmark-next-like-this
  "C-a" mc/mark-all-like-this
  "C-j" mc/cycle-forward
  "C-k" mc/cycle-backward
  "C-r" mc/insert-numbers
  "C-m" mc-hide-unmatched-lines-mode
  "C-v" mc/vertical-align
  "C-s" mc/vertical-align-with-space)

(qv/keys qvk-visual-map
  "A" mc/edit-lines
  "C-n" mc/mark-next-like-this
  "C-a" mc/mark-all-like-this)

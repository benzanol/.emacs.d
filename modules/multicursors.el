(qv/package multiple-cursors)

(setq mc/always-run-for-all t)

(qv/keys mc/keymap
  :sparse t
  "C-g" mc/keyboard-quit
  "C-j" mc/mark-next-like-this
  "C-k" mc/unmark-next-like-this)

(qv/keys modeal-visual-map
  "I" mc/edit-lines
  "A" mc/edit-lines)

(qv/key * "C-j" ,mc/keymap)

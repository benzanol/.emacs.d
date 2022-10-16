(load-file "../my-packages/shed/shed.el")

(setq shed-vertical-separator 'block
      shed-use-pixels t)

(qv/keys shed-mode-map
  :sparse t
  :keymode normal
  [remap qvk-down] shed-next-line
  [remap qvk-up] shed-previous-line
  "M-h" shed-backward-column
  "M-l" shed-forward-column
  "M-j" shed-next-row
  "M-k" shed-previous-row
  "o" shed-add-row
  "O" shed-insert-row
  "A" shed-add-column
  "I" shed-insert-column
  "D" shed-delete-row
  "C" shed-delete-column
  "RET" shed-newline
  [remap newline] shed-newline)

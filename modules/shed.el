(load-file "../my-packages/shed/shed.el")

(setq shed-vertical-separator 'block
      shed-use-pixels t)

(qv/keys shed-mode-map
  :sparse t
  [remap qvk-down] shed-next-line
  [remap qvk-up] shed-previous-line
  [remap newline] shed-newline

  "<normal> M-h" shed-backward-column
  "<normal> M-l" shed-forward-column
  "<normal> M-j" shed-next-row
  "<normal> M-k" shed-previous-row

  "<normal> o" shed-add-row
  "<normal> O" shed-insert-row
  "<normal> A" shed-add-column
  "<normal> I" shed-insert-column
  "<normal> D" shed-delete-row
  "<normal> C" shed-delete-column)

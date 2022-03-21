(add-hook 'Info-mode-hook 'variable-pitch-mode)

(qv/keys Info-mode-map
  :sparse t
  "RET" Info-follow-nearest-node
  ")" Info-history-forward
  "(" Info-history-back
  "]" Info-forward-node
  "[" Info-forward-node
  "U" Info-top-node
  "u" Info-up
  "s" Info-goto-node)

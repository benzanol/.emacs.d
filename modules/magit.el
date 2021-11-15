(qv/hook magit-mode-hook nil
  (undo-tree-mode 0))

(qv/keys magit-status-mode-map
  "j" nil "k" nil "h" nil "l" nil
  "J" nil "K" nil "H" nil "L" nil
  "g" nil "r" magit-refresh
  "SPC" magit-section-toggle)

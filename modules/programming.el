(qv/keys prog-mode-map
  :mode normal
  "g =" (@ qv/indent-document (indent-region (point-min) (point-max))))

(qv/package hideshow)

(qv/define-keys hs-minor-mode-map
  :sparse t
  "SPC" hs-toggle-hiding
  "g z r" hs-show-all
  "g z m" hs-hide-all)

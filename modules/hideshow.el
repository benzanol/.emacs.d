(qv/package hideshow)

(qv/keys hs-minor-mode-map
  :sparse t
  "SPC" hs-toggle-hiding
  "g z r" hs-show-all
  "g z m" hs-hide-all
  "g z k" beginning-of-defun
  "g z j" end-of-defun
  "g z h" (@ qv/hs-hide-defun (beginning-of-defun) (hs-hide-block)))


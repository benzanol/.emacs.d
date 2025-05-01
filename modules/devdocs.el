(qv/package devdocs)
(qv/require posframe)

(qv/keys devdocs-mode-map
  :sparse t)

(push '("*devdocs*" qv/posframe-display-buffer ())
      display-buffer-alist)

(pop display-buffer-alist)


(qv/keys prog-mode-map
  "g =" (@ qv/indent-document (indent-region (point-min) (point-max))))

(qv/hook prog-mode-hook qv/prog-mode-startup
  (buffer-face-set nil 'fixed-pitch))

(qv/hook prog-mode-hook qv/prog-mode-setup
  (outline-minor-mode 1)
  (setq-local outline-level 'outline-level)
  (setq-local outline-regexp "^\\(\t\\| \\)*;;;+ \\|^\\(\t\\| \\)*///+ "))

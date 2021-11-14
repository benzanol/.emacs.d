;;;; Whitespace Mode
(qv/package whitespace)

(setq whitespace-space-regexp "[z-a]"
      whitespace-hspace-regexp "[z-a]"
      whitespace-display-mappings
      '((newline-mark 10 [?↲ 10])
        (tab-mark 9 [?» ?  ?  ? ])))

(global-whitespace-mode 1)

;;;; Whitespace Mode
(qv/package whitespace)

(setq whitespace-display-mappings
      '((space-mark ?\s [?·])
        (tab-mark ?\t [?» ?\t])
        (newline-mark ?\n [?\xB6 ?\n])))

(qv/face whitespace-indentation :fg gray3 :bg nil)
(qv/face whitespace-space :fg gray3 :bg nil)
(qv/face whitespace-hspace :fg gray3 :bg nil)
(qv/face whitespace-tab :fg gray3 :bg nil)
(qv/face whitespace-newline :fg gray3 :w bold :bg nil :h 0.9)

(qv/face whitespace-line nil :fg nil :bg nil)
(qv/face whitespace-trailing error :fg nil :bg yellow :iv t)

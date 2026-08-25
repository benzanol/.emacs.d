;;; Whitespace Mode  -*- lexical-binding: t; -*-

(bz/package whitespace)

(bz/hook prog-mode-hook whitespace-mode)

(setq whitespace-display-mappings
      '((space-mark ?\s [?·])
        (tab-mark ?\t [?» ?\t])
        (newline-mark ?\n [?\n])))

(setq nobreak-char-display nil)

(bz/face whitespace-indentation :fg gray4 :bg nil)
(bz/face whitespace-space :fg gray4 :bg nil)
(bz/face whitespace-hspace :fg gray4 :bg nil)
(bz/face whitespace-tab :fg gray4 :bg nil)
(bz/face whitespace-newline :fg gray4 :w bold :bg nil)

(bz/face whitespace-line nil :fg nil :bg nil)
(bz/face whitespace-empty nil :fg nil :bg nil)

(bz/face bz/whitespace-indentation-error :fg "orange")
(bz/face bz/whitespace-error :fg "orange")

(bz/face whitespace-indentation bz/whitespace-indentation-error :fg nil :bg nil)
(bz/face whitespace-trailing bz/whitespace-indentation-error :fg nil :bg nil)
(bz/face whitespace-space-after-tab bz/whitespace-indentation-error :fg nil :bg nil)
(bz/face whitespace-space-before-tab bz/whitespace-indentation-error :fg nil :bg nil)

(bz/face whitespace-trailing bz/whitespace-error :fg nil :bg nil)
(bz/face whitespace-missing-newline-at-eof bz/whitespace-error :fg nil :bg nil)


;;; Provide

(provide 'bz-whitespace)
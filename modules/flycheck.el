;;; Flycheck
(qv/package flycheck)
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

(setq flycheck-keymap-prefix "C-f")

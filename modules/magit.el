(qv/package magit)

(qv/hook magit-mode-hook nil
  (undo-tree-mode 0))

(qv/keys magit-status-mode-map
  "j" nil "k" nil "h" nil "l" nil
  "J" nil "K" nil "H" nil "L" nil
  "g" nil
  "M-w" nil
  "r" magit-refresh
  "R" magit-refresh-all
  "G" magit-checkout
  "SPC" (@ qv/magit-section-toggle
           (ignore-errors (next-line) (goto-char (1- (line-beginning-position)))
                          (beginning-of-line))
           (call-interactively 'magit-section-toggle)))

;; When commiting, jump right to commit message
(setq magit-commit-show-diff nil)

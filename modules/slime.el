(qv/keys slime-mode-map
  "C-e" (if mark-active (slime-eval-region (mark) (point)) (slime-eval-last-expression))
  "C-d" slime-eval-defun
  "C-f" slime-call-defun
  )

;;;; Center Cursor
(setq-default scroll-margin 100000)
(setq-default maximum-scroll-margin 1.0)

(define-minor-mode recenter-cursor-mode
  "Recenter the cursor after every command." t)

(qv/hook post-command-hook qv/recenter
  (when recenter-cursor-mode
    (let ((cursor (point)))
      (recenter)
      (goto-char cursor))))

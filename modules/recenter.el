(setq-default scroll-margin 100000)
(setq-default maximum-scroll-margin 1.0)

(bz/hook post-command-hook bz/recenter
  (when bz/recenter-cursor-mode (ignore-errors (recenter))))

(define-minor-mode bz/recenter-cursor-mode
  "Recenter the cursor after every command."
  :init-value t)

(bz/hook (minibuffer-setup-hook calendar-mode-hook) bz/disable-recenter-cursor
  (interactive)
  (bz/recenter-cursor-mode 0))

(qv/package calfw)

(qv/hook cfw:calendar-mode-hook nil
  (setq-local scroll-margin 0)
  (display-line-numbers-mode 0))

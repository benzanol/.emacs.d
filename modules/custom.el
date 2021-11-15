(qv/package custom)

(qv/hook custom-mode-hook qv/custom-setup
  (qv/face custom-button mode-line :box (:line-width 1 :color "#505860"))
  (qv/face widget-field mode-line :bg nil)
  (variable-pitch-mode 1))

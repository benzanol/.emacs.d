(qv/package custom)

(qv/hook custom-mode-hook qv/custom-setup
  (qv/face custom-button mode-line :fg nil :bg nil :u nil :w bold
           :b (:color ,(qv/color gray3)))
  (qv/face widget-field mode-line :bg nil)
  (variable-pitch-mode 0))

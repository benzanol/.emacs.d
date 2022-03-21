(qv/package custom)

;;; Face settings
(qv/hook custom-mode-hook qv/custom-setup
  (qv/face custom-group-tag :w bold :fg nil :bg nil :h 1.3 :u t)
  (qv/face custom-variable-tag :w bold :fg nil :bg nil :h 1.1)
  (qv/face custom-button (mode-line fixed-pitch) :fg nil :bg nil :w bold
           :b (:color ,(qv/color gray3)))
  (qv/face widget-field (mode-line fixed-pitch) :fg gray1 :bg nil)
  (qv/face custom-state fixed-pitch :fg yellow :h 0.9)
  (variable-pitch-mode 1))

;; Don't confirm when custom setting a variable
(qv/key custom-mode-map
  "C-c C-c"
  (@ qv/custom-set
     (dolist (child custom-options)
       (when (eq (widget-get child :custom-state) 'modified)
         (widget-apply child :custom-set)))))

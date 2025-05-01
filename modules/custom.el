(bz/package custom)
(bz/package cus-edit)

(bz/hook (custom-mode-hook Custom-mode-hook) bz/custom-setup
  (bz/face custom-group-tag :w bold :fg nil :bg nil :h 1.3 :u t)
  (bz/face custom-variable-tag :w bold :fg nil :bg nil :h 1.1)
  (bz/face custom-button fixed-pitch :fg nil :bg bg2 :w bold
           :b (:color ,(bz/color gray2)))
  (bz/face widget-field (bg2 fixed-pitch) :fg gray1 :bg nil)
  (bz/face custom-state fixed-pitch :fg yellow :h 0.9)
  (variable-pitch-mode 1))

;; Don't confirm when custom setting a variable
(bz/key custom-mode-map
  "C-c C-c"
  (@ bz/custom-set
     (dolist (child custom-options)
       (when (eq (widget-get child :custom-state) 'modified)
         (widget-apply child :custom-set)))))

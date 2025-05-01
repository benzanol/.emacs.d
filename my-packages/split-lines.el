(setq split-lines-max-width 50)


(defun split-lines-activate ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'split-lines t)
  (while (not (eobp))
    (while (progn (forward-char split-lines-max-width)
                  (not (eolp)))
      (insert "\n"))
    (forward-line 1)
    )
  )

(defun split-lines-open (file)
  )

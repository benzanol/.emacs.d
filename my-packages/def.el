(setq defs
      (->> (split-string text "\n")
           (--map (when (string-match "\\*\\(.*?\\):\\* \\(.*\\)" it)
                    (cons (replace-regexp-in-string "\\*\\(.*?\\)\\* \\(.*\\)" "\\1" it)
                          (replace-regexp-in-string "\\*\\(.*?\\)\\* \\(.*\\)" "\\2" it))))
           (--filter it)))

(defun qv/toggle-def-overlay (o)
  (let ((isdef (overlay-get o 'isdef)))
    (overlay-put o 'isdef (not isdef))
    (if isdef
        (overlay-put o 'after-string
                     (concat (overlay-get o 'def) " " (overlay-get o 'val)))
      (overlay-put o 'after-string ((overlay-get o 'def))))))

(qv/keys qv/toggle-def-map
  :sparse t
  "SPC" (qv/toggle-def-overlay (car (overlays-at (point))))
  "S-SPC" (dolist (o (overlays-in (point-min) (point-max))) (qv/toggle-def-overlay o))
  )

(dolist (line defs)
  (insert " ")
  (let ((o (make-overlay (- (point) 1) (point) nil t nil)))
    (overlay-put o 'def (car line))
    (overlay-put o 'val (cdr line))
    (overlay-put o 'isdef t)
    (overlay-put o 'after-string (car line))
    (overlay-put o 'keymap qv/toggle-def-map)
    )
  (newline)
  )

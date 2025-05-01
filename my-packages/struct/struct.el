(rows (CHILDREN...) PROPS)
(columns (CHILDREN...) PROPS)
(text TEXT)
(number NUMBER)

;; Row Cell:
(STRUCT)
;; Col Cell:
(STRUCT :wrap BOOL :expand BOOL)


(defun struct-format (struct width)
  "Returns a format object representing a struct.

A format object is a plist of properties defining how to insert
the struct into the buffer.

:lines (LINES...)
:width WIDTH)"

  )

(defun struct-format-rows (struct)
  (dolist (r (cadr struct))
    ()
    )
  )

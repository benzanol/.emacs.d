;; Location is a buffer, filename, or function

(bz/face lens-vertical-line :h 0.1 :bg black)
(bz/face lens-footer :o t :x t :h 0.5)
(bz/face lens-header :o t :x t :bg "#EEEEF0")

(defun lens--indent-string (level &optional header)
  (let* ((str #("   "
                0 1 (display (space :height (1) :width 1))
                1 2 (face lens-indent-line)
                2 3 (display (space :height (1) :width 2)))))
    (when header (add-face-text-property 1 (length str) 'lens-header 'append str))
    str))


(defun lens--section-string (text spec &optional header indented)
  (concat (propertize (concat (lens--indent-string 1 'header)
                              (propertize (format "%s\n" (or header "")) 'face 'lens-header))
                      'section-beg spec
                      'read-only t 'rear-nonsticky t 'front-sticky t)
          text "\n"
          (propertize "\n" 'face 'lens-footer
                      'section-end spec
                      'read-only t 'front-sticky t 'rear-nonsticky t)))

(defun lens-insert-lens (&keys onchange)
  (unless (get-buffer buffer) (error "Wrong type of argument `buffer or string`: %s" buffer))
  (let ((start (point))
        (horizontal-line (propertize (format "\n%s\n" (make-string 30 ?-)) 'read-only t))
        ol)
    (insert horizontal-line)
    (insert (buffer-string buffer))
    (insert horizontal-line)
    )
  )

(defun lens-insert-lens (content &rest props)
  "Insert a lens with the specified properties
:onchange - Function or buffer
:onsave - Function or filename"

  (let (())
    
)

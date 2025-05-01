(defun lens-get-prop (file prop)
  (alist-get prop (car (lens-parse file))))

(defun lens-set-prop (file prop string)
  (lens-operate
   file
   (lambda (struct)
     (setf (alist-get prop (car struct)) string))))

(defun lens-modify-prop (file prop func)
  (lens-operate
   file
   (lambda (struct)
     (let* ((old (alist-get prop (car struct)))
            (new (funcall func old)))
       (setf (alist-get prop (car struct)) new)))))


(defun lens--get-or-create-subsection (section headings)
  "Helper function for finding/creating a nested heading within an existing section."
  (if (null headings) section

    (let ((next-sec (alist-get (car headings) (cdr section) nil nil #'string=)))
      (unless next-sec
        (setq next-sec (list nil))
        (nconc section (list (cons (car headings) next-sec))))

      (lens--get-or-create-subsection next-sec (cdr headings)))))

(defun lens--get-subsection (section headings)
  "Helper function for finding/creating a nested heading within an existing section."
  (if (null headings) section
    (let ((next-sec (alist-get (car headings) (cdr section) nil nil #'string=)))
      (when next-sec (lens--get-subsection next-sec (cdr headings))))))


(defun lens-insert-line (file elem pos &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings))
             (new-lines (-insert-at (or pos (length old-lines)) elem (car section))))
        (setcar section new-lines)))))

(defun lens-add-line (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (unless (member elem (car section))
          (setcar section (append (car section) (list elem))))))))

(defun lens-remove-line (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (setcar section (--filter (not (equal it elem)) (car section)))))))

(defun lens-get-section (file &rest headings)
  (lens--get-subsection (cdr (lens-parse file)) headings))

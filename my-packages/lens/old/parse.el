;; State structure (returned by parse) looks like: (PROPS . ROOT-SECTION)
;; Every section has the form (LINES (HEADING . SUBSECTION)...)
;; Each of LINES is one of the following:
;; - a string
;; - (link TARGET DESCRIPTION?)
;; - (block LANGUAGE CODE PROP-STRING?)


(setq lens-mode-formats
      '((".org" lens--parse-org      lens--generate-org)
        (".md"  lens--parse-markdown lens--generate-markdown)))

(defun lens--get-mode-spec (file)
  (or (--find (s-ends-with-p (car it) file)
              lens-mode-formats)
      (error "No mode format known for %s. See lens-mode-formats for more details." mode)))


(defun lens-parse-file (file)
  "Fetch the struct of a file as it is saved on disk."
  (with-temp-buffer
    (insert-file file)
    (funcall (cadr (lens--get-mode-spec file)))))

(defun lens-parse (&optional buf-or-file ext)
  "Fetch the most up to date struct (from an unsaved buffer if necessary.)
BUF-OR-FILE specifies a buffer or file to use.
EXT specifies the extension to use for parsing, ex 'org'."

  ;; By default, use the current buffer
  (unless buf-or-file (setq buf-or-file (current-buffer)))

  ;; If it is a file with an open buffer, use the open buffer
  (when (and (stringp buf-or-file) (get-file-buffer buf-or-file))
    (setq buf-or-file (get-file-buffer buf-or-file)))

  (if (stringp buf-or-file) (lens-parse-file buf-or-file)
    (with-current-buffer buf-or-file
      (funcall (cadr (lens--get-mode-spec (or (concat "." ext) buffer-file-name)))))))


(defun lens-operate (file func)
  (let* ((existing-buf (get-file-buffer file))
         (buf (or existing-buf (find-file-noselect file))))

    (unwind-protect
        (with-current-buffer buf
          (let* ((spec (lens--get-mode-spec buffer-file-name))
                 (struct (funcall (cadr spec))))
            ;; Mutate the struct
            (funcall func struct)

            (delete-region (point-min) (point-max))
            (insert (funcall (caddr spec) struct)))

          (save-buffer))

      ;; If the buffer was just created, close it
      (unless existing-buf (kill-buffer buf)))))

;; (defun lens-write (file struct)
;;   (lens-operate file (lambda (s)
;;                        (setcar s (car struct))
;;                        (setcdr s (cdr struct)))))

;;; Query commands: util functions that use the above functions as helpers


(defun lens-query-get (file prop)
  (alist-get prop (car (lens-parse file))))

(defun lens-query-set (file prop string)
  (lens-operate
   file
   (lambda (struct)
     (setf (alist-get prop (car struct)) string))))

(defun lens-query-modify (file prop func)
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


(defun lens-query-insert (file elem pos &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings))
             (new-lines (-insert-at (or pos (length old-lines)) elem (car section))))
        (setcar section new-lines)))))

(defun lens-query-add (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (unless (member elem (car section))
          (setcar section (append (car section) (list elem))))))))

(defun lens-query-remove (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (setcar section (--filter (not (equal it elem)) (car section)))))))

(defun lens-query-section (file &rest headings)
  (lens--get-subsection (cdr (lens-parse file)) headings))

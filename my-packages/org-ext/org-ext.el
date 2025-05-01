(define-derived-mode oext-mode org-mode "Org Extended"
  "Org mode with custom extensions added on")




(defun oext-find-file (file)
  (interactive "fFile: ")
  (find-file file)

  )


(defun oext-parse-buffer ()
  )

(setq a '(1 2 3))
(setf (nth 1 a) a)
(eq a (nth 1 a))

(setq b (bz/copy-looped-tree a))

(bz/copy-looped-tree )




(defun oext--remove-surrounding-blank (element &optional previous)
  (unless (memq element previous)

    (unless previous (setf previous (list nil)))
    (nconc previous (list element))

    (dotimes (i (length element))
      (cond ((eq (nth i element) :post-blank) (setf (nth (1+ i) element) 0))
            ((listp (nth i element))
             (oext--remove-surrounding-blank (nth i element) previous))))))

(setq a (copy-tree $10))
(insert (org-element-interpret-data ))

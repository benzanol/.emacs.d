(defmacro elsnip (&rest args)
  `(let (vars)
     (dolist (a ',args)
       (when (and (symbolp a) (s-starts-with-p "$" (symbol-name a)))
         (push (cons a (read-string (format "%s: " (symbol-name a)))) vars)))
     (dolist (a ',args)
       (cond ((stringp a) (insert a))
             ((or (listp a) (symbolp a))
              (insert (eval (qv/replace-dollar-vars a))))))))

(elsnip "hello-" (upcase $1) "-there-" $1)



(defun qv/replace-dollar-vars (e)
  (cond ((and (symbolp e) (s-starts-with-p "$" (symbol-name e)))
         `(alist-get ',e vars))
        ((listp e) (mapcar #'qv/replace-dollar-vars e))
        (t e)))

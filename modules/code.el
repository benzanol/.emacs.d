(defun qv/insert-code (code lang &rest args)
  
  )

(defvar qv/code-snippets nil
  "Alist of languages to alists of snippets.")

(defvar qv/custom-codes
  '((ctl ; Basic control structures
     (set (name type value) "Set a variable")
     (function (name type (* arg) body) "Define a function")
     (if ((* cond body)) "Do something if a condition is true")
     (match (obj (* cond body)) "Test multiple cases for the value of an object")
     (while (cond body) "Perform some action while a condition is true")
     (loop (list body) "Loop through the elements of a sequence")
     (range (var from to by body) "Loop through a range of values"))
    (seq ; Methods for dealing with sequences
     (get (seq index) "Get a certain index of a sequence")
     (set (seq index val) "Set a certain index of a sequence")
     (insert (seq index val) "Insert a value to a sequence")
     (append (seq val) "Append a value to a sequence")
     (functional
      (map (seq func) "Map over the elements of a sequence")
      (reduce (seq func) "Reduce the elements of a sequence")
      (fold (seq start func) "Fold the elements of a sequence"))
     (utils
      (sort (seq) "Sort a sequence")
      (reverse (seq) "Reverse a sequence"))
     )
    )
  "Nested alist of custom code names and signatures."
  )

(defun qv/code-define (code lang))

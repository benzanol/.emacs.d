(require 'dash)



(defvar lens-date-regexp "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]")
(defun lens-parse-date (string)
  (let ((time-list (parse-time-string string)))
    ;; Set time zone to zero
    (setf (nth 8 time-list) 0)
    ;; Convert to days and back to get a 4 number time stamp
    (floor (time-to-number-of-days (encode-time (--map (or it 0) time-list))))))

(defvar lens-time-regexp "[0-9]\\{1,2\\}:[0-9][0-9]")
(defun lens-parse-time (str)
  (let ((nums (mapcar #'string-to-number (split-string str ":"))))
    (+ (* 60 (car nums)) (cadr nums))))


(defun lens-parse-table (string columns &optional nonstrict)
  "Parse STRING as an org table, returning (HEADER-ROWS . BODY-ROWS).

COLUMNS can be a regexp, a mapping function, (REGEXP . FUNC), or
a list of (REGEXP . FUNC). If one or more regexps is provided,
and none match, then an error will be thrown.

If NONSTRICT is non-nil, instead of erroring when a cell doesn't
match any regexp, the line will be left as a single string."

  (let* ((hline "\\`|\\(?:-+\\+\\)*-+|\\'")
         (lines (split-string (string-trim string) "\n"))
         (header (cond ((string-match-p hline (car lines)) (list (pop lines)))
                       ((string-match-p hline (cadr lines)) (list (pop lines) (pop lines)))))
         (rows (apply #'list lines))
         cell-strs cells cell pat funcs)

    (dotimes (i (length lines))
      (setq cell-strs (-slice (split-string (nth i lines) "|" nil " +") 1 -1)
            cells (apply #'list cell-strs))

      (catch 'nomatch
        (dotimes (j (length cell-strs))
          (setq cell (nth j cell-strs) pat (nth j columns)
                funcs (pcase pat
                        ((pred stringp) (list (cons pat #'identity)))
                        ((pred functionp) (list (cons ".*" pat)))
                        (`(,(pred stringp) . ,_) (list pat))
                        (_ pat)))

          ;; Look for a function that matches
          (let ((match (--find (string-match-p (format "\\`\\(?:%s\\)\\'" (car it)) cell) funcs)))
            ;; Update the cell with the matching function
            (if match (setf (nth j cells) (funcall (cdr match) cell))
              ;; If funcs was defined (but no match) and strict mode is enabled, throw an error
              (when (and funcs (not nonstrict))
                (error "Incorrect table format at (%s %s): %s" i j cell)))))

        ;; If nothing was thrown, set the row to the result
        (setf (nth i rows) cells)))

    (cons header rows)))

(defun lens-generate-table (header data &optional columns)
  "Generate an org table format from DATA, a 2D list of cells.

COLUMNS is a list of functions for converting each column to
their text representation."
  (with-temp-buffer
    (dolist (line header) (insert line "\n"))

    (dolist (line data)
      (if (stringp line) (insert line "\n")

        ;; Insert each cell individually
        (dotimes (i (length line))
          (insert "|")
          (if (nth i columns)
              (insert (or (funcall (nth i columns) (nth i line)) ""))
            (insert (nth i line))))
        (insert "|\n")))

    (org-table-align)

    (buffer-substring-no-properties (point-min) (point-max))))

(defun lens-generate-bar-graph (labels nums &rest props)
  "Return a text bar graph with LABELS on the bottom and NUMS for data.

PROPS can contain:
:step (default 1)
:width (default 3)
:spacing (default 1)
:bar-width (default 1)"

  (unless (eq (length labels) (length nums))
    (error "Mismatched number of labels and datapoints"))

  (let* ((spacing (make-string (or (plist-get props :spacing) 1) ?\s))
         (bar-w (or (plist-get props :bar-width) 1))
         (width (max bar-w (or (plist-get props :width) 3)))

         (bar-matrix (list (make-list (length nums) (make-string bar-w ?\s))))
         num bar-h bar-h-ceil lines)

    (dotimes (col (length nums))
      (setq num (nth col nums))

      ;; Generate the bar characters
      (setq bar-h (/ (float num) (or (plist-get props :step) 1)))
      (setq bar-h-ceil (ceiling bar-h))

      ;; If the bar matrix isnt tall enough, add nil rows until it is
      (while (< (length bar-matrix) bar-h-ceil)
        (nconc bar-matrix (list (make-list (length nums) (make-string bar-w ?\s)))))

      ;; Fill in the bars
      (dotimes (row bar-h-ceil)
        (setf (nth col (nth row bar-matrix)) (make-string bar-w ?█)))

      ;; Set the top bar to a more precise height
      (when (> bar-h 0)
        (setf (nth col (nth (1- bar-h-ceil) bar-matrix))
              (make-string bar-w (aref "█▇▆▅▄▃▂▁" (floor (* 8 (- bar-h-ceil bar-h))))))))

    (format "%s\n%s\n%s"
            (string-join (--map (string-join (--map (s-center width it) it) spacing)
                                (reverse bar-matrix))
                         "\n")
            (string-join (--map (s-center width it) labels) spacing)
            (string-join (--map (s-center width (s-left width (format "%s" it))) nums) spacing))))


(bz/face lens-today :fg blue :w bold)

(defun lens-ui--daily-table-functions (&rest options)
  "OPTIONS can include :col-filters, :col-reverters."
  `(:state-fn
    (lambda (table-text props)
      (let* ((row-fmt (cons (cons lens-date-regexp #'lens-parse-date)
                            ,(plist-get options :col-filters)))
             (table (lens-parse-table table-text row-fmt 'include))
             ;; Start the ui state at the sunday before the last day in the table
             (this-day (lens-parse-date (format-time-string "%Y-%m-%d")))
             (this-dow (string-to-number (format-time-string "%w" (days-to-time this-day) 0)))
             (weeks (or (plist-get props :weeks) 1))
             (last-sunday (- this-day this-dow (* (1- weeks) 7))))

        `(:header ,(car table) :rows ,(cdr table) :sunday ,last-sunday . ,props)))

    :text-fn
    (lambda (state)
      ;; Sort the days in chronological order
      (plist-put state :rows (seq-sort-by #'car #'< (plist-get state :rows)))
      (lens-generate-table
       (plist-get state :header)
       (plist-get state :rows)
       (cons (lambda (day) (format-time-string "%Y-%m-%d" (days-to-time day) 0))
             ,(plist-get options :col-reverters))))))

(defun lens-ui--daily-table-header (state &optional header)
  (let ((sun (plist-get state :sunday))
        (weeks (or (plist-get state :weeks) 1)))
    `(columns ((button " < " :onclick (lambda (s) (plist-put s :sunday ,(- sun 7))))
               (button " > " :onclick (lambda (s) (plist-put s :sunday ,(+ sun 7))))
               (button " <> " :onclick (lambda (s) (plist-put s :weeks ,(+ weeks 1))))
               (button " >< " :onclick (lambda (s) (plist-put s :weeks ,(max 1 (- weeks 1)))))
               (string ,(format "%s - %s%s"
                                (format-time-string "%d %b %Y" (days-to-time sun) 0)
                                (format-time-string "%d %b %Y" (days-to-time (+ sun (* 7 weeks) -1)) 0)
                                (or header "")))))))


(lens-defui sleep-schedule
  "This is the docstring"

  ,@(lens-ui--daily-table-functions)

  :ui-fn
  (lambda (state)
    (let* ((sun (plist-get state :sunday))
           (weeks (or (plist-get state :weeks) 1))
           (hrs (--map (pcase-let ((`(,s ,w) (--map (and it (not (string= it "")) (lens-parse-time it))
                                                    (alist-get it (plist-get state :rows)))))
                         (if (not (and s w)) 0
                           ;; If the sleep time is 6pm or later, it was the previous day
                           (when (>= s (* 18 60)) (setq s (- s (* 24 60))))
                           (/ (- w s) 60.0)))
                       (number-sequence sun (+ sun (* weeks 7) -1))))
           (defined-days (min (* weeks 7) (- (car (--last t (plist-get state :rows))) sun)))
           (avg (when (> defined-days 0) (/ (apply #'+ hrs) defined-days 1.0)))
           (avg-str (when avg (propertize (format "%.3f" avg) 'font-lock-face 'bold)))
           (today (lens-parse-date (format-time-string "%Y-%m-%d"))))

      (list
       (lens-ui--daily-table-header state (when avg (format "  Average: %s hours" avg-str)))
       (lens-generate-bar-graph
        (--map (propertize
                (nth (mod it 7) '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                'lens-onclick
                `(lambda (s)
                   (pcase-let ((`(,wake ,slep) (alist-get ,(+ sun it) (plist-get s :rows))))
                     (setf (alist-get ,(+ sun it) (plist-get s :rows))
                           (list (read-string "Sleep: " slep)
                                 (read-string "Wake: " wake)))))
                'font-lock-face (when (= today (+ sun it)) 'lens-today))
               (number-sequence 0 (1- (* weeks 7))))
        hrs
        :bar-width (or (plist-get state :bar-width) 5))))))


(bz/face lens-ui-habit-checked :fg green)
(bz/face lens-ui-habit-unchecked :fg red)
(lens-defui habit
  "This is the docstring"

  ,@(lens-ui--daily-table-functions)

  :ui-fn
  (lambda (state)
    (let ((sun (plist-get state :sunday))
          (weeks (or (plist-get state :weeks) 1))
          (today (lens-parse-date (format-time-string "%Y-%m-%d"))))

      `(,(lens-ui--daily-table-header state)
        (columns
         ,(--map (let ((entry (assoc it (plist-get state :rows))))
                   (concat
                    (propertize
                     (cond (entry (propertize "✔" 'font-lock-face 'lens-ui-habit-checked))
                           ((< it today) (propertize "✖" 'font-lock-face 'lens-ui-habit-unchecked))
                           (t "□"))
                     'lens-onclick
                     `(lambda (s)
                        (setf (alist-get ,it (plist-get s :rows) :rem 'remove)
                              ;; Remove the item with :rem, or insert a default value of nil
                              ,(if entry :rem nil))))
                    "\n"
                    (propertize (nth (mod (- it sun) 7) '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                                'font-lock-face (if (= it today) 'lens-today 'shadow))))
                 (number-sequence sun (+ sun (* weeks 7) -1))))))))

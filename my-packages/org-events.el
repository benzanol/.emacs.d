(defvar org-events-weekday-abbreviations
  '(((monday    mon m) 0)
    ((tuesday   tue t) 1)
    ((wednesday wed w) 2)
    ((thursday  thu r) 3)
    ((friday    fri f) 4)
    ((saturday  sat s) 5)
    ((sunday    sun u) 6))
  "A list of possible abbreviations for each day of the week.
Each element is another list, with the first element being a list
of possible abbreviations (as symbols), and the second element
being the number of the day (0 for monday to 6 for sunday)")

(defun org-events-generate-repetitions (&optional pos)
  "Create a drawer contain repetitions of an event

If `pos` is non-nil, the event used will be the heading at buffer
position `pos`, otherwise, it will be the heading at the point."

  (unless pos (setq pos (point)))

  (save-excursion
    (goto-char pos) (end-of-line)
    (search-backward-regexp "^\\*+ EVENT ")
    (setq pos (point))
    (let ((timestamps (org-events-get-times))
          (heading-level (car (org-heading-components)))
          (heading-name (replace-regexp-in-string
                         "^EVENT " "" (nth 4 (org-heading-components))))
          (heading-end (plist-get (cadr (org-element-headline-parser (point-max)))
                                  :contents-end)))
      ;; Remove any past repetitions drawer
      (goto-char pos)
      (replace-regexp "\n:REPETITIONS:[^z-a]*?\n *:END: *$" "" nil (point) heading-end)

      (next-line 1) (beginning-of-line)
      ;; If the first thing in the heading is a properties drawer, go to the end of it
      (if (search-forward-regexp "\\= *:PROPERTIES: *\n\\( *:.+:.*\n\\)*? *:END: *$" nil t)
          (newline)
        (newline) (previous-line 1))

      (insert ":REPETITIONS:\n")
      (dolist (timestamp timestamps)
        (insert (format "%s TODO %s\nSCHEDULED: %s\n\n"
                        (make-string (1+ heading-level) ?*)
                        heading-name timestamp)))
      (insert ":END:")
      nil)))

(defun org-events-get-times (&optional pos)
  "Return a list of org time stamps at which an event occurs.

If `pos` is non-nil, the event used will be the heading at buffer
position `pos`, otherwise, it will be the heading at the point."

  ;; Use the current position if none is specified
  (unless pos (setq pos (point)))

  ;; Get the values of relevant properties of the org heading at point
  (when-let ((interval (org-entry-get pos "REPEAT-INTERVAL"))
             (days (split-string (or (org-entry-get pos "REPEAT-DAYS") "") " " t))
             (start (org-entry-get pos "REPEAT-START"))
             (end (org-entry-get pos "REPEAT-END")))
    (let (start-time end-time start-day end-day
                     all-days all-times all-stamps)

      ;; Parse the time stamps for the beginning and end times
      (dolist (var '(start end))
        (if (eval var)
            (with-temp-buffer
              (insert (eval var))
              (beginning-of-buffer)
              (set var (org-element-timestamp-parser)))
          (error "Event %s time must be specified" var)))
      (setq start-time (org-timestamp-to-time start)
            end-time (org-timestamp-to-time end)
            start-day (time-to-days start-time)
            end-day (time-to-days end-time))

      ;; Set all-days to be a list of absolute days based on the repeat specifications
      (cond ((string= interval "week")
             ;; Calculate times for weekly repetition
             (let ((start-week-day (mod (1- start-day) 7))
                   (week-days (-> (--map (eval `(cl-case (intern (downcase it))
                                                  . ,org-events-weekday-abbreviations))
                                         (-filter 'stringp days))
                                (-non-nil)
                                (sort '<)
                                (remove-duplicates))))
               (dolist (week-day week-days)
                 (let* ((days-after-start (mod (- week-day start-week-day) 7))
                        (new-day (+ start-day days-after-start)))
                   (while (< new-day end-day)
                     (push new-day all-days)
                     (setq new-day (+ 7 new-day)))))
               (setq all-days (sort all-days '<)))))

      ;; Calculate time stamps for each repetition
      (setq all-stamps
            (--map (let ((date-stamp (format-time-string
                                      "%Y-%m-%d %a" (days-to-time (- it 719162))))
                         (time-stamp (replace-regexp-in-string
                                      "^[[<][-0-9]\\{10\\} +\\(?:[SMTWF][^] \t>]* +\\)?\\([^] \t>]+\\).*" "\\1"
                                      (plist-get (cadr start) :raw-value))))
                     (format "<%s %s>" date-stamp time-stamp))
                   all-days)))))

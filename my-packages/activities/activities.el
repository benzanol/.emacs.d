(require 'subr-x)
(require 'dash)

(defvar bz/current-activity
      (list "Default" (cons 'current-layout "Default")
            (cons 'layouts
                  (list (cons "Default"
                              (cons nil (current-window-configuration)))))
            (cons 'buffers (mapcar (lambda (buf) (cons nil buf)) (buffer-list)))
            (cons 'point (point))))
(defvar bz/activities (list bz/current-activity))


(defun bz/activity-base-frame ()
  (cond ((bound-and-true-p exwm--floating-frame)
         (nth exwm-workspace-current-index exwm-workspace--list))
        ((selected-frame))))


(defun bz/add-activity (name &optional window-config)
  "Add a new activity with the name specified by the symbol NAME
By default, only the current buffer is added to the new activity,
but BUFFERS specifies which should be used instead."
  (interactive "MActivity Name: ")
  (select-frame (bz/activity-base-frame))
  (if (assoc name bz/activities)
      (message "Activity already exists")
    (push (list (upcase-initials (substring-no-properties name))
                (cons 'current-layout "default")
                (cons 'layouts `(("default" . (nil . ,(current-window-configuration)))))
                (cons 'point (point))
                (cons 'plist nil))
          bz/activities)
    (bz/switch-to-activity name)
    (if window-config
        (set-window-configuration window-config)
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (delete-other-windows))
    (setcdr (cdar (alist-get 'layouts bz/current-activity))
            (current-window-configuration))))

(defun bz/rename-activity (name)
  (interactive (list (read-string (format "Rename %s to: " (car bz/current-activity)))))
  (let ((numbered (rassoc (car bz/current-activity) bz/activity-numbers))
        (upname (upcase-initials name)))

    (when numbered (setcdr numbered upname))
    (setcar bz/current-activity upname)))

(defun bz/move-activity (name)
  (interactive (list (bz/read-activity-name (format "Move %s to: " (car bz/current-activity)))))
  (let ((config (current-window-configuration)))
    (bz/switch-to-activity name)
    (set-window-configuration config)))

(defun bz/delete-activity ()
  "Delete the current activity from the activities list"
  (interactive)

  (select-frame (bz/activity-base-frame))

  (setq bz/activities (delq bz/current-activity bz/activities))

  (setq bz/activity-numbers
        (--remove (string= (cdr it) (car bz/current-activity))
                  bz/activity-numbers))

  (bz/other-activity))

(defvar bz/activity-history nil)

(defvar bz/positional-activities nil)
(defun bz/read-activity-name (prompt &optional nonpositional)
  (let ((numbered
         (--map (concat (cdr it) (propertize (format "    (%s)" (car it)) 'face 'font-lock-keyword-face))
                (--filter (and it (or (not nonpositional)
                                      (not (member (downcase (cdr it)) bz/positional-activities))))
                          (--map (assoc it bz/activity-numbers)
                                 (number-sequence 1 10)))))
        (nonnumbered
         (-map #'car (--remove (or (eq it bz/current-activity)
                                   ;; (member (downcase (car it)) bz/positional-activities)
                                   (rassoc (car it) bz/activity-numbers)
                                   )
                               bz/activities))))
    (car (s-split-up-to "    " (completing-read prompt (append numbered nonnumbered)) 1))))

(defun bz/switch-to-nonpositional-activity (name)
  (interactive (list (bz/read-activity-name (format "Activity (%s): " (car bz/current-activity)) t)))
  (bz/switch-to-activity (car (split-string name "    "))))

(defun bz/switch-to-activity (name)
  "Set the current activity to the activity with the name NAME"
  (interactive
   (list (completing-read
          (format "Activity (%s): " (car bz/current-activity))
          (mapcar 'car (--remove (eq it bz/current-activity) bz/activities)))))
  (let ((new-activity (--find (string= (downcase name) (downcase (car it))) bz/activities)))
    (if (null new-activity) (when (> (length name) 0) (bz/add-activity name))

      (select-frame (bz/activity-base-frame))

      ;; Add the activity being switched away from to the start of the history
      (push (car bz/current-activity) bz/activity-history)

      (when bz/current-activity (ignore-errors (bz/save-current-layout)))
      (setq bz/current-activity new-activity)
      (set-window-configuration (cddr (assoc (alist-get 'current-layout bz/current-activity)
                                             (alist-get 'layouts bz/current-activity))))

      ;; Remove the new current activity from the history
      (setq bz/activity-history (delete (car bz/current-activity) bz/activity-history))

      (with-current-buffer (get-buffer-create "*scratch*")
        (setq default-directory (file-name-as-directory (or (bz/activity-get :path) "~"))))

      (echo-bar-update)

      (ignore-errors (echo-bar-update)))))

(defun bz/back-activity ()
  (interactive)
  (bz/switch-to-activity (car bz/activity-history))
  (setq bz/activity-history (append (cdr bz/activity-history) (list (car bz/activity-history))))
  (message "%s" bz/activity-history))

(defun bz/other-activity ()
  (interactive)
  (if bz/activity-history
      (bz/switch-to-activity (car bz/activity-history))
    (message "No last activity")))

(defun bz/last-unnumbered-activity ()
  (interactive)
  (let ((last (--find (not (rassoc it bz/activity-numbers)) bz/activity-history)))
    (if last (bz/switch-to-activity last)
      (message "No last unnumbered activity"))))

(defun bz/save-current-layout ()
  (select-frame (bz/activity-base-frame))
  (setcdr (cdr (assoc (alist-get 'current-layout bz/current-activity)
                      (alist-get 'layouts bz/current-activity)))
          (current-window-configuration))
  (setcdr (assoc 'point bz/current-activity) (point)))

(defun bz/add-layout (name &optional layout)
  (interactive "MLayout Name: ")
  (select-frame (bz/activity-base-frame))
  (let ((layout-list (assoc 'layouts bz/current-activity)))
    (if (assoc name layout-list)
        (message "Layout already exists")
      (bz/save-current-layout)
      (bz/activity-switch-buffer)
      (delete-other-windows)
      (setcdr layout-list
              (append (cdr layout-list)
                      (list (cons name (cons nil (current-window-configuration))))))
      (setcdr (assoc 'current-layout bz/current-activity) name))))

(defun bz/remove-layout (name)
  "Delete the layout with NAME from the layout list of the current activity"
  (interactive
   (list (completing-read "Remove Layout: " (alist-get 'layouts bz/current-activity))))
  (select-frame (bz/activity-base-frame))
  (if (equal name (alist-get 'current-layout bz/current-activity))
      (message "You can't delete the current layout")
    (let ((new-layout-list ()))
      (dolist (i (alist-get 'layouts bz/current-activity))
        (unless (equal (car i) name)
          (setq new-layout-list (append new-layout-list (list i)))))
      (setcdr (assoc 'layouts bz/current-activity) new-layout-list))))

(defun bz/switch-to-layout (name)
  (interactive (list (completing-read "Select Layout: " (alist-get 'layouts bz/current-activity))))
  (select-frame (bz/activity-base-frame))
  (bz/save-current-layout)
  (setcdr (assoc 'current-layout bz/current-activity) name)
  (set-window-configuration (cddr (assoc name (alist-get 'layouts bz/current-activity)))))

(defvar bz/activity-view-alist nil
  "User facing facilities for activities
Each element has the form (activity key icon)")

(defun bz/activity-key ()
  (interactive)
  (let ((as bz/activity-view-alist))
    (while as
      (if (not (eq last-input-event (nth 1 (car as)))) (pop as)
        (bz/switch-to-activity (nth 0 (car as)))
        (setq as nil)))))

(defun bz/activity-set (prop val)
  (interactive
   (let* ((plist (alist-get 'plist bz/current-activity))
          (props (--map (substring (symbol-name (nth it plist)) 1)
                        (number-sequence 0 (1- (length plist)) 2)))
          (prop (intern (concat ":" (completing-read "Property: " props))))
          (val (eval (read--expression "Value: " (prin1-to-string (bz/activity-get prop))))))
     (list prop val)))
  (setf (plist-get (alist-get 'plist (cdr bz/current-activity)) prop) val))

(defun bz/activity-get (prop)
  (plist-get (alist-get 'plist bz/current-activity) prop))


(defun bz/activity-number-set (number)
  (setf (alist-get number bz/activity-numbers) (car bz/current-activity)))

(defun bz/activity-number-go (number)
  (when-let ((a (alist-get number bz/activity-numbers)))
    (bz/switch-to-activity a)))

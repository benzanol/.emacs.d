(setq qv/current-activity
      (list "default" (cons 'current-layout "default")
            (cons 'layouts
                  (list (cons "default"
                              (cons nil (current-window-configuration)))))
            (cons 'buffers (mapcar (lambda (buf) (cons nil buf)) (buffer-list)))
            (cons 'point (point))))
(setq qv/activities (list qv/current-activity))

(defun qv/add-activity (name &optional buffers)
  "Add a new activity with the name specified by the symbol NAME
By default, only the current buffer is added to the new activity,
but BUFFERS specifies which should be used instead."
  (interactive "MActivity Name: ")
  (if (assoc name qv/activities)
      (message "Activity already exists")
    (push `(,(substring-no-properties name) (current-layout . "default")
            (layouts . (("default" . (nil . ,(current-window-configuration)))))
            (buffers . ())
            (point . ,(point)))
          qv/activities)
    (qv/switch-to-activity name)
    (switch-to-buffer (or (cdar (alist-get 'buffers qv/current-activity))
                          "*scratch*"))
    (delete-other-windows)
    (setcdr (cdar (alist-get 'layouts qv/current-activity))
            (current-window-configuration))))

(defun qv/rename-activity (activity name)
  (interactive (list (assoc (completing-read "Rename Activity: " qv/activities) qv/activities)
                     (completing-read "New Name: " nil)))
  (setcar activity name))

(defun qv/remove-activity (activity)
  "Delete ACTIVITY from the activities list"
  (interactive (list (completing-read "Remove Activity: " qv/activities)))
  (when (symbolp activity) (setq activity (format "%s" activity)))
  (setq qv/activities (seq-remove (lambda (x) (string= (car x) activity)) qv/activities)))

(defvar qv/last-activity nil "The last activity used before the current one.")

(defun qv/switch-to-activity (name)
  "Set the current activity to the activity with the name NAME"
  (interactive
   (list (completing-read
          (concat "Activity (" (car qv/current-activity) "): ")
          (mapcar 'car (--remove (eq it qv/current-activity) qv/activities)) nil nil "^")))

  (setq qv/last-activity qv/current-activity)
  (when qv/current-activity (ignore-errors (qv/save-current-layout)))
  (setq qv/current-activity (assoc name qv/activities))
  (set-window-configuration (cddr (assoc (alist-get 'current-layout qv/current-activity)
                                         (alist-get 'layouts qv/current-activity)))))

(defun qv/add-buffer-to-activity (buffer &optional activity)
  "Move a certain buffer to the current activity, then return that buffer."
  (interactive
   (list (completing-read
          "Select a buffer: "
          (let ((activity-bufs (mapcar 'cdr (alist-get 'buffers qv/current-activity))))
            (--map (buffer-name it)
                   (--remove (memq it activity-bufs) (buffer-list)))))))

  (if (string= buffer " ")
      buffer
    (let ((buffer-object (if (bufferp buffer) buffer (get-buffer buffer))))
      (dolist (i qv/activities) (qv/remove-buffer-from-activity buffer-object i))
      (setcdr (assoc 'buffers (or activity qv/current-activity))
              (append (alist-get 'buffers (or activity qv/current-activity))
                      (list (cons nil (get-buffer buffer-object)))))
      buffer-object)))

(defun qv/remove-buffer-from-activity (buffer &optional activity)
  "Remove BUFFER from the list of buffers that are a part of ACTIVITY"
  (let ((new-buffer-list ()))
    (dolist (i (alist-get 'buffers (or activity qv/current-activity)))
      (unless (eq (cdr i) buffer) (setq new-buffer-list (append new-buffer-list (list i)))))
    (setcdr (assoc 'buffers (cdr (or activity qv/current-activity))) new-buffer-list)))

(defun qv/activity-switch-buffer ()
  "Switch to a certain buffer that is part of the current activity"
  (interactive)
  (let ((new-buffer-list ()) (current-in-activity nil))
    (dolist (i (alist-get 'buffers qv/current-activity))
      (when (buffer-name (cdr i))
        (if (eq (cdr i) (current-buffer))
            (setq current-in-activity i)
          (setq new-buffer-list (append new-buffer-list (list i))))))
    (let ((new-buffer
           (completing-read
            "Buffer in Activity: "
            (mapcar (lambda (element)
                      (buffer-name (cdr element)))
                    new-buffer-list)))
          (new-obj nil)
          (new-list nil))
      (dolist (i new-buffer-list)
        (if (eq (cdr i) (get-buffer new-buffer))
            (setq buffer-obj i)
          (setq new-list (append new-list (list i)))))
      (switch-to-buffer new-buffer)
      (setcdr (assoc 'buffers qv/current-activity)
              (append (list buffer-obj)
                      (if current-in-activity (list current-in-activity) nil)
                      new-list)))))

(defun qv/save-current-layout ()
  (setcdr (cdr (assoc (alist-get 'current-layout qv/current-activity)
                      (alist-get 'layouts qv/current-activity)))
          (current-window-configuration))
  (setcdr (assoc 'point qv/current-activity) (point)))

(defun qv/add-layout (name &optional layout)
  (interactive "MLayout Name: ")
  (let ((layout-list (assoc 'layouts qv/current-activity)))
    (if (assoc name layout-list)
        (message "Layout already exists")
      (qv/save-current-layout)
      (qv/activity-switch-buffer)
      (delete-other-windows)
      (setcdr layout-list
              (append (cdr layout-list)
                      (list (cons name (cons nil (current-window-configuration))))))
      (setcdr (assoc 'current-layout qv/current-activity) name))))

(defun qv/remove-layout (name)
  "Delete the layout with NAME from the layout list of the current activity"
  (interactive
   (list (completing-read "Remove Layout: " (alist-get 'layouts qv/current-activity))))
  (if (equal name (alist-get 'current-layout qv/current-activity))
      (message "You can't delete the current layout")
    (let ((new-layout-list ()))
      (dolist (i (alist-get 'layouts qv/current-activity))
        (unless (equal (car i) name)
          (setq new-layout-list (append new-layout-list (list i)))))
      (setcdr (assoc 'layouts qv/current-activity) new-layout-list))))

(defun qv/switch-to-layout (name)
  (interactive (list (completing-read "Select Layout: " (alist-get 'layouts qv/current-activity))))
  (qv/save-current-layout)
  (setcdr (assoc 'current-layout qv/current-activity) name)
  (set-window-configuration (cddr (assoc name (alist-get 'layouts qv/current-activity)))))

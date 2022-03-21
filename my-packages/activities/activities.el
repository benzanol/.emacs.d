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
    (switch-to-buffer "*scratch*")
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
                                         (alist-get 'layouts qv/current-activity))))
  (ignore-errors (echo-bar-update)))

(defun qv/add-buffer-to-activity (buffer &optional activity)
  "Move a certain buffer to the current activity, then return that buffer."
  (interactive
   (list (completing-read
          "Select a buffer: "
          (let ((activity-bufs (mapcar 'cdr (alist-get 'buffers qv/current-activity))))
            (--map (buffer-name it)
                   (--remove (memq it activity-bufs) (buffer-list)))))))
  (if (string= buffer " ") buffer
    (let ((buffer-object (if (bufferp buffer) buffer (get-buffer buffer))))
      (dolist (i qv/activities) (qv/remove-buffer-from-activity buffer-object i))
      (setcdr (assoc 'buffers (or activity qv/current-activity))
              (append (alist-get 'buffers (or activity qv/current-activity))
                      (list (cons nil (get-buffer buffer-object)))))
      buffer-object)))

(defun qv/add-buffer-advice (func buffer)
  (if (not (or (get-buffer buffer)
               (not (stringp buffer))
               (s-starts-with-p " " buffer)
               (s-starts-with-p "*" buffer)))
      (prog1 (funcall func buffer)
        (qv/add-buffer-to-activity buffer qv/current-activity))
    (funcall func buffer)))
;;(advice-add 'get-buffer-create :around 'qv/add-buffer-advice)

(defun qv/remove-buffer-from-activity (buffer &optional activity)
  "Remove BUFFER from the list of buffers that are a part of ACTIVITY"
  (interactive
   (list (completing-read
          "Select a buffer: "
          (let ((activity-bufs (mapcar 'cdr (alist-get 'buffers qv/current-activity))))
            (--map (buffer-name it)
                   (--filter (memq it activity-bufs) (buffer-list)))))))
  (let ((new-buffer-list ()))
    (dolist (i (alist-get 'buffers (or activity qv/current-activity)))
      (unless (eq (cdr i) (get-buffer buffer))
        (setq new-buffer-list (append new-buffer-list (list i)))))
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

(defvar qv/activity-view-alist nil
  "User facing facilities for activities
Each element has the form (activity key icon)")

(defun qv/activity-key ()
  (interactive)
  (let ((as qv/activity-view-alist))
    (while as
      (if (not (eq last-input-event (nth 1 (car as)))) (pop as)
        (qv/switch-to-activity (nth 0 (car as)))
        (setq as nil)))))

(defun qv/customize-activity ()
  (interactive)
  (when-let* ((cur (car qv/current-activity))
              (key (read-event (concat cur " - ")))
              (key (unless (eq key (aref (kbd "s-q") 0)) key))
              (num (- key (aref (kbd "s-0") 0)))
              (icon (with-temp-buffer
                      (all-the-icons-insert)
                      (buffer-substring-no-properties (point-min) (point-max))))
              (view (list cur key (format "%s %s" (propertize (format "%s" num) 'face 'bold) icon))))
    (setq qv/activity-view-alist
          (seq-remove (lambda (a) (or (string= (car a) cur) (eq (cadr a) key)))
                      qv/activity-view-alist))
    (push view qv/activity-view-alist)
    (setq qv/activity-view-alist (sort qv/activity-view-alist (lambda (a b) (< (cadr a) (cadr b)))))))

(defun qv/activity-string ()
  (let ((str "| ") (cur nil)
        (face (list :foreground (qv/color yellow))))
    (dolist (a qv/activity-view-alist)
      (setq cur (substring (caddr a)))
      (when (string= (car a) (car qv/current-activity))
        (add-face-text-property 0 (length cur) face nil cur))
      (setq str (concat str cur " | ")))
    str))

  (dotimes (i 10) (global-set-key (kbd (format "s-%s" i)) 'qv/activity-key))

  (setq qv/activity-view-alist
        '(("browser" 8388657
           #("1  " 0 1
             (face bold)))
          ("default" 8388658
           #("2 " 0 1
             (face bold)))
          ("emacs" 8388659
           #("3 " 0 1
             (face bold)))
          ("scala" 8388660
           #("4 " 0 1
             (face bold)))
          ("arduino" 8388661
           #("5 " 0 1
             (face bold)))))


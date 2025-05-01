;;;; Xinput
(defun bz/xinput-id (device-name)
  "Return the xinput id of the device with DEVICE-NAME"
  (string-to-number
   ($$ "xinput list | grep '%s[ \t]*id=' | sed \"s/.*id=\\\\([0-9]*\\\\).*/\\\\1/\"" device-name)))

(defun bz/xinput-property-number (device property-name)
  "Return the number of the property with PROPERTY-NAME
for the device with id or name of DEVICE"
  (string-to-number
   ($$ "xinput list-props %s | sed -n \"s/.*%s *(\\\\([0-9]*\\\\).*/\\\\1/p\""
       (if (numberp device) device (bz/xinput-id device)) property-name)))

(defun bz/xinput-set-property (device property value)
  (interactive
   (let ((id (bz/xinput-id
              (completing-read
               "Select Device: "
               (split-string ($$ "xinput list | sed -n \"s/[^a-zA-Z]*\\\\([a-zA-Z].*[^ \t]\\\\)[ \t]*id=.*/\\\\1/p\"")
                             "\n")))))
     (list id (bz/xinput-property-number
               id (car (split-string
                        (completing-read
                         "Property: "
                         (split-string
                          ($$ (concat
                               "xinput list-props 12"
                               "| sed -n \""
                               "s/[ \t]*\\\\(.*[^ \t]\\\\)"
                               "[ \t]*([0-9]*):[ \t]*\\\\(.*\\\\)/"
                               "\\\\1 (\\\\2)/p\""
                               "| sed \"s/libinput *//\""))
                          "\n"))
                        " (")))
           (read-string "New value: "))))
  ($ "xinput set-prop %s %s %s"
     (if (numberp device) device
       (bz/xinput-id device))
     (if (numberp property) property
       (bz/xinput-property-number device property))
     (if (numberp value) (number-to-string value) value))
  value)

;;;; Touchpad Tapping
(setq bz/xinput-tapping-enabled 0)
(defun bz/xinput-toggle-tapping ()
  (interactive)
  (setq bz/xinput-tapping-enabled (1+ (- bz/xinput-tapping-enabled)))
  (if (string= system-name "fedora")
      ($ "xinput set-prop 12 329 %s" bz/xinput-tapping-enabled)
    ($ "xinput set-prop 12 334 %s" bz/xinput-tapping-enabled))
  (message "Tapping %s" (if (eq 1 bz/xinput-tapping-enabled) "Enabled" "Disabled")))
(bz/xinput-toggle-tapping)

;;; X Settings
(shell-command "xrandr -s 1920x1080")
(set-frame-size (selected-frame) 1900 1080 t)

;;; EXWM
;;;; Installing
(qv/package exwm)

;;;; EXWM Window Mode
(setq exwm-manage-configurations '((t char-mode t)))

;;;; Renaming EXWM Buffers
(defun qv/exwm-update-class ()
  (if (string= exwm-class-name ".blueman-applet-wrapped")
      (kill-buffer nil))
  (exwm-layout-hide-mode-line)
  (exwm-workspace-rename-buffer (concat ":" exwm-class-name ":")))

(add-hook 'exwm-update-class-hook 'qv/exwm-update-class)

;;;; Char Mode
(defun qv/exwm-char-mode ()
  (interactive)
  (exwm-input-release-keyboard
   (exwm--buffer->id (current-buffer))))

(add-hook 'exwm-input--event-hook 'qv/exwm-char-mode)
(add-hook 'post-command-hook 'qv/exwm-char-mode)

;;; Global Keybindings
(setq exwm-input-global-keys nil)

;;;; Running commands
(qv/keys exwm
  "s-x" execute-extended-command
  "s-r" qv/run-in-background
  "s-e" eval-expression
  "s-E" repeat-complex-command
  "s-o" counsel-linux-app
  "s-s" qv/read-emacs-key-sequence)

(defun qv/read-emacs-key-sequence (key-sequence)
  (interactive
   (progn (exwm-reset)
          (remove-hook 'exwm-input--event-hook 'qv/exwm-char-mode)
          (prog1 (list (read-key-sequence (propertize "Key Sequence:" 'face 'minibuffer-prompt)))
            (add-hook 'exwm-input--event-hook 'qv/exwm-char-mode))))
  (call-interactively (key-binding key-sequence)))

;;;; Quitting
(qv/keys exwm
  "s-q" (@ qv/keyboard-quit
           (if (active-minibuffer-window)
               (with-selected-window (active-minibuffer-window)
                 (keyboard-escape-quit))
             (eval (list (key-binding (kbd "C-g"))))))
  ;; When deleting a window, go to the closest window in the layout
  "s-w" (@ qv/close-window
           (let ((remove (selected-window))
                 (goto (or (window-next-sibling) (window-prev-sibling))))
             (while (and (windowp goto) (not (window-live-p goto)))
               (setq goto (window-child goto)))
             (select-window goto)
             (delete-window remove)))
  "s-W" kill-buffer
  "s-C-w" (kill-buffer (current-buffer))
  "s-M-w" (switch-to-buffer "*scratch*"))

;;;; Activity manager
(defun qv/switch-to-buffer (buffer)
  (interactive
   (list (completing-read
          "Switch to Buffer: "
          (mapcar 'buffer-name (buffer-list)))))
  (switch-to-buffer buffer))

(qv/keys exwm
  "s-B" qv/switch-to-buffer
  "s-b" qv/activity-switch-buffer
  "s-M-b" qv/add-buffer-to-activity
  "s-a" qv/switch-to-activity
  "s-A" qv/add-activity
  "s-C-a" qv/remove-activity
  "s-d" qv/switch-to-layout
  "s-D" qv/add-layout
  "s-C-d" qv/remove-layout
  "s-c" (@ qv/next-activity (qv/switch-to-activity (car qv/last-activity)))
  "s-C" (@ qv/previous-activity
           (qv/switch-to-activity
            (car (nth (mod (1+ (-elem-index
                                qv/current-activity
                                qv/activities))
                           (length qv/activities))
                      qv/activities)))))

;;;; Logging out
(qv/keys exwm
  "s-C-Z" (shell-command "ps -ef | grep emacs | awk '{print $2}' | xargs kill")
  "s-C-S-S" (shell-command
             (format "%s & systemctl suspend"
                     (expand-file-name "~/.bin/custom-lock")))
  "s-C-R" (shell-command "systemctl reboot")
  "s-C-P" (shell-command "shutdown now"))

;;;; Playerctl
(qv/keys exwm "<XF86AudioPlay>" (shell-command "playerctl play-pause"))

;;;; Clicking
(qv/keys exwm
  "s-c" (shell-command "xdotool click 1")
  "s-M-c" (shell-command "xdotool click 3")
  "s-C" (shell-command "xdotool click 3")
  "s-C-c" (shell-command
           (string-join
            '("xdotool mousemove 960 540"
              "xdotool click 1"
              "xdotool mousemove 1920 1080")
            " ; ")))

;;;; Window management
(qv/keys exwm
  "M-TAB" other-frame
  "<s-tab>" other-frame

  "s-f" exwm-floating-toggle-floating
  "s-F" exwm-layout-toggle-fullscreen

  "s-h" windmove-left
  "s-l" windmove-right
  "s-j" windmove-down
  "s-k" windmove-up

  "s-C-h" qv/window-move-left
  "s-C-l" qv/window-move-right
  "s-C-j" qv/window-move-down
  "s-C-k" qv/window-move-up

  "s-C-M-h" windmove-swap-states-left
  "s-C-M-l" windmove-swap-states-right
  "s-C-M-j" windmove-swap-states-down
  "s-C-M-k" windmove-swap-states-up)

;;;; Resizing Windows
(defun qv/window-resize (delta horizontal)
  (let ((before-fixed window-size-fixed))
    (setq-local window-size-fixed nil)
    (window-resize (selected-window) delta horizontal)
    (setq-local window-size-fixed before-fixed)))

(qv/keys exwm
  "s-M-h" (qv/window-resize -4 t)
  "s-M-l" (qv/window-resize +4 t)
  "s-M-j" (qv/window-resize -1 nil)
  "s-M-k" (qv/window-resize +1 nil)

  "s-M-H" (qv/window-resize -16 t)
  "s-M-L" (qv/window-resize +16 t)
  "s-M-J" (qv/window-resize -5 nil)
  "s-M-K" (qv/window-resize +5 nil))

;;;; Splitting Windows
(qv/keys exwm
  "s-H" ( qv/split-window 'left)
  "s-L" ( qv/split-window 'right)
  "s-J" ( qv/split-window 'down)
  "s-K" ( qv/split-window 'up))

;; Neither function works on both operating systems
;;;;; Works on debian
(defun qv/split-window (direction)
  (let ((window-size-fixed nil))
    (select-window (split-window nil nil direction))
    (switch-to-buffer "*scratch*")))

;;; Configuration
(exwm-enable)
(exwm-init)

;;;; Transparency
(defun qv/set-opacity (opacity)
  (set-frame-parameter (selected-frame) 'alpha (cons opacity opacity))
  (add-to-list 'default-frame-alist (cons 'alpha (cons opacity opacity)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
(qv/set-opacity 88)
(qv/run-in-background "compton")

;;;; Wallpaper
(defun qv/set-wallpaper (wallpaper)
  (shell-command
   (concat "feh --bg-scale "
           (expand-file-name "~/Media/Wallpaper/")
           wallpaper)))
(qv/set-wallpaper "RedNebulaWallpaper.jpeg")

;;;; Audio Settings
(defun qv/current-pulseaudio-sink ()
  "Return the number of the active pulse audio sink"
  (string-to-number (replace-regexp-in-string
                     ".*Sink #\\([[:digit:]]+\\)--State: RUNNING.*" "\\1"
                     (replace-regexp-in-string
                      "\n[[:space:]]*" "--"
                      (shell-command-to-string "pactl list sinks")))))
(defun qv/pulseaudio-sink-volume (sink)
  "Return the current volume of the sink number specified by the argument"
  (string-to-number
   (replace-regexp-in-string
    (concat ".*Sink #" (number-to-string sink) "--.*?Volume:.*? \\([[:digit:]]+\\)%.*")
    "\\1"
    (replace-regexp-in-string
     "\n[[:space:]]*" "--"
     (shell-command-to-string "pactl list sinks")))))

(defun qv/pulseaudio-sink-description (sink)
  "Return the description of the sink number specified by the argument"
  (replace-regexp-in-string
   (concat ".*Sink #" (number-to-string sink) "--.*?Description: \\(.*?\\)--.*")
   "\\1"
   (replace-regexp-in-string
    "\n[[:space:]]*" "--"
    (shell-command-to-string "pactl list sinks"))))

(defun qv/change-volume (delta &optional sink)
  "Change the system volume by DELTA using pactl. \
If SINK is specified, use that as the output device instead of the active sink"
  (let ((use-sink (or sink (qv/current-pulseaudio-sink))))
    (if delta
        (shell-command
         (concat "pactl set-sink-volume "
                 (number-to-string use-sink) " +"
                 (number-to-string delta) "%"))
      (shell-command
       (concat "pactl set-sink-volume " (number-to-string use-sink) " 0%")))
    (message (concat "Volume is now "
                     (number-to-string (qv/pulseaudio-sink-volume use-sink))
                     "%% for "
                     (qv/pulseaudio-sink-description use-sink)))))

(qv/keys exwm
  "<XF86AudioRaiseVolume>" (qv/change-volume 5)
  "<XF86AudioLowerVolume>" (qv/change-volume -5)
  "<XF86AudioMute>" (qv/change-volume nil))

;;;; Brightness
(setq qv/system-brightness 1.0)
(defun qv/change-brightness (delta &optional monitor)
  "Change the system brightness by DELTA using xrandr. \
If delta is a float, multiply the current brightness by delta instead. \
If MONITOR is specified, change the brightness of it instead of eDP-1"
  (setq qv/system-brightness (min 1.0 (max 0.01 (+ qv/system-brightness delta))))
  (let ((xrandr-cmd
         (format "xrandr --output %s --brightness %s"
                 (or monitor "eDP-1") qv/system-brightness))
        (brightnessctl-cmd
         (format "brightnessctl s %s%%"
                 (round (* 100 qv/system-brightness)))))
    (shell-command brightnessctl-cmd)
    (message "Brightness is now %s%%" (round (* 100 qv/system-brightness)))))

(qv/keys exwm
  "<XF86MonBrightnessUp>" (qv/change-brightness 0.01)
  "<XF86MonBrightnessDown>" (qv/change-brightness -0.01))

;;;; Xinput
(setq qv/xinput-tapping-enabled 0)
(defun qv/xinput-toggle-tapping ()
  (interactive)
  (setq qv/xinput-tapping-enabled (1+ (- qv/xinput-tapping-enabled)))
  (qv/xinput-set-property 12 336 qv/xinput-tapping-enabled)
  (message "Tapping %s" (if (eq 1 qv/xinput-tapping-enabled) "Enabled" "Disabled")))
;;(global-set-key (kbd "C-x C-s C-t") 'qv/xinput-toggle-tapping)

(defun qv/xinput-id (device-name)
  "Return the xinput id of the device with DEVICE-NAME"
  (string-to-number
   (shell-command-to-string (concat "xinput list | grep '" device-name
                                    "[ 	]*id=' | sed \"s/.*id=\\\\([0-9]*\\\\).*/\\\\1/\""))))

(defun qv/xinput-property-number (device property-name)
  "Return the number of the property with PROPERTY-NAME
for the device with id or name of DEVICE"
  (string-to-number
   (shell-command-to-string
    (concat "xinput list-props "
            (number-to-string (if (numberp device) device
                                (qv/xinput-id device)))
            " | sed -n \"s/.*" property-name " *(\\\\([0-9]*\\\\).*/\\\\1/p\""))))

(defun qv/xinput-set-property (device property value)
  (interactive
   (let ((id (qv/xinput-id
              (completing-read
               "Select Device: "
               (split-string (shell-command-to-string
                              (concat
                               "xinput list | sed -n \"s/"
                               "[^a-zA-Z]*\\\\([a-zA-Z].*[^ 	]\\\\)[ 	]*id=.*/"
                               "\\\\1/p\"")) "\n")))))
     (list id (qv/xinput-property-number
               id (car (split-string
                        (completing-read
                         "Property: "
                         (split-string
                          (shell-command-to-string
                           (concat "xinput list-props 12"
                                   "| sed -n \""
                                   "s/[ 	]*\\\\(.*[^ 	]\\\\)"
                                   "[ 	]*([0-9]*):[ 	]*\\\\(.*\\\\)/"
                                   "\\\\1 (\\\\2)/p\""
                                   "| sed \"s/libinput *//\""))
                          "\n")) " (")))
           (read-string "New value: "))))
  (shell-command
   (concat "xinput set-prop "
           (number-to-string (if (numberp device)
                                 device
                               (qv/xinput-id device))) " "
           (number-to-string (if (numberp property)
                                 property
                               (qv/xinput-property-number device property))) " "
           (if (numberp value) (number-to-string value) value)))
  value)

;;;; Hardware
(qv/xinput-set-property 12 331 1)

;;;; Screen Resolution
(shell-command "xrandr -s 1920x1080")

;;; Miscellaneous
;;;; Browser
;; When opening a browser, do it in the browser activity
(advice-add 'browse-url-default-browser :before
            (lambda (&rest args) (qv/switch-to-activity "browser")))

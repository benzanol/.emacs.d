(bz/package exwm)
(bz/package desktop-environment)


;;; Char Mode
(push '(t char-mode t) exwm-manage-configurations)

(bz/hook (window-state-change-hook exwm-input--event-hook) bz/exwm-char-mode
  (interactive)
  (ignore-errors
    (when (and exwm--id (eq (window-buffer) (current-buffer)))
      (switch-to-buffer (current-buffer))
      (exwm-input-release-keyboard exwm--id))))

;;; Browser
(bz/advise :before browse-url-default-browser bz/browse-url-advice (&rest args)
  ;; When opening a browser, do it in the browser activity
  (bz/switch-to-activity "browser"))

;;; Floating/Monitors

;; Windows that start floating often crash exwm
(push '(t floating nil) exwm-manage-configurations)


(setq exwm-floating-border-color (bz/color yellow))
(setq exwm-floating-border-width 1)


(defvar-local bz/exwm-floating-move-pixel nil)
(defun bz/exwm-floating-toggle-pixel ()
  (interactive)
  (setq bz/exwm-floating-move-pixel (not bz/exwm-floating-move-pixel)))

(defun bz/exwm-floating-move (dir dist)
  (when bz/exwm-floating-move-pixel (setq dist 1))
  (let ((dx (* dist (pcase dir ('right 1) ('left -1) (_ 0))))
        (dy (* dist (pcase dir ('up -1) ('down 1) (_ 0)))))
    (exwm-floating-move dx dy)))

(defun bz/exwm-floating-resize (delta horizontal)
  (when bz/exwm-floating-move-pixel (setq delta 1))
  ;; In case in the minibuffer
  (if horizontal (set-frame-width nil (+ (frame-width) delta))
    (set-frame-height nil (+ (frame-height) delta))))

(bz/keys bz/exwm-floating-map
  :sparse t
  [remap windmove-left]               (@ bz/exwm-floating-move-left  (bz/exwm-floating-move 'left 30))
  [remap windmove-right]              (@ bz/exwm-floating-move-right (bz/exwm-floating-move 'right 30))
  [remap windmove-down]               (@ bz/exwm-floating-move-down  (bz/exwm-floating-move 'down 30))
  [remap windmove-up]                 (@ bz/exwm-floating-move-up    (bz/exwm-floating-move 'up 30))

  [remap bz/split-window-left]        (@ bz/exwm-floating-move-big-left  (bz/exwm-floating-move 'left 200))
  [remap bz/split-window-right]       (@ bz/exwm-floating-move-big-right (bz/exwm-floating-move 'right 200))
  [remap bz/split-window-down]        (@ bz/exwm-floating-move-big-down  (bz/exwm-floating-move 'down 200))
  [remap bz/split-window-up]          (@ bz/exwm-floating-move-big-up    (bz/exwm-floating-move 'up 200))

  [remap bz/shrink-window-horizontal] (@ bz/exwm-shrink-window-horizontal (bz/exwm-floating-resize -8 t))
  [remap bz/grow-window-horizontal]   (@ bz/exwm-grow-window-horizontal   (bz/exwm-floating-resize +8 t))
  [remap bz/shrink-window-vertical]   (@ bz/exwm-shrink-window-vertical   (bz/exwm-floating-resize -2 nil))
  [remap bz/grow-window-vertical]     (@ bz/exwm-grow-window-vertical     (bz/exwm-floating-resize +2 nil))

  [remap bz/pull-window]
  (@ bz/exwm-floating-nfl-to-monitor
     (bz/exwm-position-floating-window -220 1499 2500 1112)
     (exwm-layout-hide-mode-line))
  "M-p" bz/exwm-floating-toggle-pixel)

(setf (alist-get 'exwm--floating-frame minor-mode-map-alist) bz/exwm-floating-map)

(defun bz/exwm-position-floating-window (x y width height)
  (when exwm--floating-frame
    (set-frame-width nil width nil t)
    (set-frame-height nil height nil t)
    (exwm-floating-move (- x (car (frame-position))) (- y (cdr (frame-position))))))


(defun bz/scepter ()
  (interactive)
  ($$ "xrandr --output eDP-1 --pos 0x1080 --primary --output DP-1 --mode 1920x1080 --pos 0x0")
  ;; (set-frame-position (selected-frame) 0 0)
  )

(setq exwm-workspace-number 2)
(setq exwm-randr-workspace-monitor-plist '(1 "DP-1"))
;; (setq exwm-randr-workspace-monitor-plist nil)

;; (defun bz/exwm-update-border-width (width)
;;   (when exwm--floating-frame
;;     (xcb:+request exwm--connection
;;         (make-instance 'xcb:ConfigureWindow
;;                        :window (alist-get 'parent-id (frame-parameters))
;;                        :value-mask xcb:ConfigWindow:BorderWidth
;;                        :border-width width))
;;     (let ((delta (if (= 0 width) -1 1)))
;;       (exwm-floating-move delta delta))))

;; (bz/advise :before bz/remember-last-window bz/exwm-update-border ()
;;   "Since this is run before the remember function,
;; bz/current-window is actually the last window."
;;   (unless (eq bz/current-window (selected-window))
;;     (ignore-errors
;;       (with-selected-frame (window-frame bz/current-window)
;;         (bz/exwm-update-border-width 0)))
;;     (bz/exwm-update-border-width 1)))


;;; Update Class Hook

(bz/hook exwm-update-title-hook bz/exwm-update-title
  (let ((class (upcase-initials exwm-class-name))
        (title (->> (or exwm-title "")
                    (replace-regexp-in-string " — Mozilla Firefox$" "")
                    (replace-regexp-in-string " - LibreOffice Writer" ""))))
    (exwm-workspace-rename-buffer (format "%s: %s" class title))))

(defvar bz/electron-window nil)
(bz/hook exwm-update-class-hook bz/exwm-update-class
  (exwm-layout-hide-mode-line)
  (bz/exwm-update-title)

  (pcase (downcase exwm-class-name)
    ;; ("Gcr-Prompter: Unlock Login Keyring" (kill-buffer nil))
    (".blueman-applet-wrapped" (kill-buffer nil))
    ;; (".blueman-manager-wrapped" (exwm-floating-toggle-floating))
    ("electron" (run-with-timer
                 0.5 nil (lambda () (when (window-live-p bz/electron-window)
                                      (with-selected-window bz/electron-window
                                        (switch-to-buffer ":Electron:"))))))))

(run-with-timer 0.1 0.01 #'bz/kill-gcr-prompter)
(defun bz/kill-gcr-prompter ()
  (ignore-errors (kill-buffer "Gcr-Prompter: Unlock Login Keyring")))



;;; Action Keybindings

(bz/keys bz/exwm-action-map
  :sparse t
  :parent bz/action-map
  :prefix ("M-" bz/exwm-mod-action-map)

  "TAB" other-frame

  "o" bz/open-app-by-key
  "O" bz/app-or-window
  ;; "M-C-o" (@ bz/sudo-open-app (bz/app-or-window 'sudo))

  "s" bz/read-emacs-key-sequence

  "a" bz/switch-to-nonpositional-activity
  "A" bz/activity-set
  "C-S-a" bz/move-activity
  "d" bz/prj-open
  "C-S-d" bz/delete-activity

  "c" bz/other-activity
  "C" bz/last-unnumbered-activity

  "z" exwm-floating-toggle-floating
  ;; "z" (@ bz/left-click ($$ "xdotool click 1"))
  "Z" bz/exwm-floating-center

  "C-S-z" (@ bz/kill-emacs ($ "ps -ef | grep emacs | awk '{print $2}' | xargs kill"))
  ;; "M-C-S-w" (@ bz/i3lock ($$ "i3lock -i ~/Media/Wallpaper/BlurredWallpaper.png"))
  "C-S-q" (@ bz/suspend-and-lock
             ($$ "systemctl suspend")
             ($$ "i3lock -i ~/Media/Wallpaper/Icetwigs.png"))
  ;; "C-S-s" (@ bz/shutdown ($ "shutdown now"))
  ;; "C-S-r" (@ bz/reload ($ "reload"))

  "p" (@ bz/exwm-workspace-next
         (when (eq (length exwm-workspace--list) 1) (exwm-workspace-add 1))
         (exwm-workspace-switch (mod (1+ exwm-workspace-current-index) (length exwm-workspace--list))))
  "P" (@ bz/exwm-workspace-move-next
         (when (eq (length exwm-workspace--list) 1) (exwm-workspace-add 1))
         (let ((next (mod (1+ exwm-workspace-current-index) (length exwm-workspace--list))))
           (exwm-workspace-move-window next)
           (exwm-workspace-switch next)))

  "n" (@ bz/next-exwm-buffer
         (bz/buffer-history-back
          (lambda (b) (and (eq (buffer-local-value 'exwm--frame b) exwm-workspace--current)
                           (null (get-buffer-window b 'all-frames))))))
  "N" (@ bz/switch-to-exwm-buffer
         (->> (buffer-list)
              (--filter (buffer-local-value 'exwm-class-name it))
              (--map (if (eq (buffer-local-value 'exwm--frame it) (car exwm-workspace--list))
                         (propertize (buffer-name it) 'face 'bold)
                       (propertize (buffer-name it) 'face 'italic)))
              (completing-read "Exwm Buffer: " )
              (exwm-workspace-switch-to-buffer))
         (bz/exwm-char-mode))

  "0" (@ bz/activity-0 (bz/activity-number-go 0))
  "1" (@ bz/activity-1 (bz/activity-number-go 1))
  "2" (@ bz/activity-2 (bz/activity-number-go 2))
  "3" (@ bz/activity-3 (bz/activity-number-go 3))
  "4" (@ bz/activity-4 (bz/activity-number-go 4))
  "5" (@ bz/activity-5 (bz/activity-number-go 5))
  "6" (@ bz/activity-6 (bz/activity-number-go 6))
  "7" (@ bz/activity-7 (bz/activity-number-go 7))
  "8" (@ bz/activity-8 (bz/activity-number-go 8))
  "9" (@ bz/activity-9 (bz/activity-number-go 9))

  ")" (@ bz/activity-set-0 (bz/activity-number-set 0))
  "!" (@ bz/activity-set-1 (bz/activity-number-set 1))
  "@" (@ bz/activity-set-2 (bz/activity-number-set 2))
  "#" (@ bz/activity-set-3 (bz/activity-number-set 3))
  "$" (@ bz/activity-set-4 (bz/activity-number-set 4))
  "%" (@ bz/activity-set-5 (bz/activity-number-set 5))
  "^" (@ bz/activity-set-6 (bz/activity-number-set 6))
  "&" (@ bz/activity-set-7 (bz/activity-number-set 7))
  "*" (@ bz/activity-set-8 (bz/activity-number-set 8))
  "(" (@ bz/activity-set-9 (bz/activity-number-set 9)))


(setq bz/app-keys
      '(("b" "blueman-manager")
        ("e" "emacs")
        ("f" "firefox")
        ("g" "gparted" sudo)
        ("n" "nemo")
        ("p" "pavucontrol")
        ("v" "evince")
        ("s" "spotify")
        ;; ("j" "xhost + ; distrobox enter debian1 -e '~/Programs/intellij/idea'")
        ("w" "nm-connection-editor")
        ))

(defun bz/open-app-by-key ()
  (interactive)
  (let* ((key-desc (key-description (vector (bz/exwm-read-key "Key: "))))
         (base-key (-last-item (split-string key-desc "-")))
         (app (alist-get base-key bz/app-keys nil nil #'string=))
         (pwd (when (memq 'sudo app) (read-passwd "Password: "))))

    (cond ((null app) (message "No app found"))
          (pwd (process-send-string ($ (concat "sudo " (car app)))
                                    (concat pwd "\n")))
          (t ($ (car app))))))

(defun bz/exwm-read-key (prompt)
  (let ((old-win (selected-window)))
    (when (derived-mode-p 'exwm-mode) (select-window (minibuffer-window)))

    (unwind-protect (read-key prompt)
      (select-window old-win))))

(defun bz/app-or-window (sudo)
  (interactive "P")
  (let* ((windows (--filter (s-starts-with-p ":" it) (mapcar #'buffer-name (buffer-list))))
         (cmd (completing-read (concat (if sudo "Sudo " "") "Program/Window: ")
                               (append windows (bz/get-executables)))))
    (cond ((s-starts-with-p ":" cmd) (switch-to-buffer cmd))
          ((not sudo) ($ cmd))
          (t (let ((pwd (read-passwd "Password: "))
                   (process ($ (concat "sudo " cmd))))
               (process-send-string process (concat pwd "\n")))))))

(defun bz/read-emacs-key-sequence (command)
  (interactive
   (with-selected-window (cadr (window-tree))
     (-> (propertize "Key Sequence:" 'face 'minibuffer-prompt)
         (read-key-sequence)
         (key-binding)
         (list))))
  (call-interactively command))

(defun bz/exwm-floating-center ()
  (interactive)
  (bz/exwm-position-floating-window 360 220 960 540))


;;; Keybindings

(setq bz/gamma 1.0)
(bz/keys bz/exwm-map
  :sparse t
  :parent bz/exwm-mod-action-map

  "M-<tab>" other-frame

  "<print>" (@ bz/flameshot-gui ($ "flameshot gui"))
  "M-<print>" (@ bz/flameshot-full ($ "flameshot full"))
  "C-<print>" bz/recording-mode
  "C-S-<print>" (@ bz/recording-delete (delete-file bz/recording-file) (bz/recording-mode 0)
                                       (message "Deleted Recording"))

  "<XF86MonBrightnessUp>" desktop-environment-brightness-increment
  "<XF86MonBrightnessDown>" desktop-environment-brightness-decrement
  "S-<XF86MonBrightnessUp>" desktop-environment-brightness-increment-slowly
  "S-<XF86MonBrightnessDown>" desktop-environment-brightness-decrement-slowly
  "C-<XF86MonBrightnessUp>" (@ bz/increase-gamma (bz/change-gamma bz/gamma-increment))
  "C-<XF86MonBrightnessDown>" (@ bz/decrease-gamma (bz/change-gamma (- bz/gamma-increment)))

  "C-S-<XF86MonBrightnessUp>" bz/cpu-up
  "C-S-<XF86MonBrightnessDown>" bz/cpu-down

  "S-<XF86AudioRaiseVolume>" desktop-environment-volume-increment-slowly
  "S-<XF86AudioLowerVolume>" desktop-environment-volume-decrement-slowly
  "<XF86AudioRaiseVolume>" desktop-environment-volume-increment
  "<XF86AudioLowerVolume>" desktop-environment-volume-decrement
  "<XF86AudioMute>" (@ bz/exwm-mute (desktop-environment-volume-set 0))
  "<S-XF86AudioMute>" desktop-environment-toggle-mute

  "<XF86AudioPlay>" desktop-environment-toggle-music
  "<XF86AudioPause>" desktop-environment-toggle-music
  "<XF86AudioPrev>" desktop-environment-music-previous
  "<XF86AudioNext>" desktop-environment-music-next

  "<XF86Search>" (@ bz/exwm-connect-to-linkbuds (bz/bluetooth-connect "LinkBuds S" "F8:4E:17:83:CF:4E"))
  "<S-XF86Search>" (@ bz/exwm-connect-to-jlab (bz/bluetooth-connect "JLab JBuds Lux ANC" "90:DA:07:3A:51:27"))
  "<home>" (@ bz/exwm-connect-to-speaker (bz/bluetooth-connect "SRS-XB20" "B8:D5:0B:46:97:12"))
  )

(setq bz/bluetooth-dots 0)
(defun bz/bluetooth-connect (name mac)
  (setq bz/bluetooth-dots (mod (1+ bz/bluetooth-dots) 3))
  (message (concat (propertize "Connecting to " 'face 'bold)
                   (propertize name 'face 'help-key-binding)
                   (make-string (1+ bz/bluetooth-dots) ?.)))
  ($ "echo 'connect %s' | bluetoothctl" mac))


(bz/require recording)
(bz/require system)


;; Define global keys to be marked as exwm keys
(bz/package helpful)
(setq exwm-input-global-keys
      (--map (cons (kbd (key-description (car it))) (cadr it))
             (helpful--keymap-keys bz/exwm-map)))


;; So that changes to the exwm map take effect
(set-keymap-parent global-map bz/exwm-map)


;;; Enable EXWM

;; (bz/package exwm-systemtray)
;; (exwm-systemtray-enable)
;; ($$ "nm-applet")

;; Make full screen applications fit the emacs window
(unless (bz/required exwm)
  (bz/package exwm-randr)
  (exwm-randr-enable)

  (exwm-enable)
  (exwm-init)


  (bz/exwm-setup-displays)
  ($$ "xrandr -s 1920x1080")
  ($ "feh --bg-scale ~/Media/Wallpaper/Icetwigs.jpg")
  )

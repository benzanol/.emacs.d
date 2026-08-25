;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-functions)

(require 'dash)
(require 'exwm)
(require 'exwm-randr)


;; (require 'bz-recording)
;; (require 'bz-system)
(run-with-timer 1 nil 'require 'bz-system)


;;; Char Mode

(add-to-list 'exwm-manage-configurations '(t char-mode t))

(bz/hook (window-state-change-hook exwm-input--event-hook) bz/exwm-char-mode
  (ignore-errors
    (when (and exwm--id (eq (window-buffer) (current-buffer)))
      (switch-to-buffer (current-buffer))
      (exwm-input-release-keyboard exwm--id))))


;;; Browser
;; (bz/advise :remove browse-url-default-browser bz/browse-url-advice (&rest args)
;;   ;; When opening a browser, do it in the browser activity
;;   (bz/switch-to-activity "browser"))


;;; Floating

;; Windows that start floating often crash exwm
(add-to-list 'exwm-manage-configurations '(t floating nil))


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
  :doc "Keymap enabled in exwm floating windows."
  :sparse t
  [remap bz/window-left]               (@ bz/exwm-floating-move-left  (bz/exwm-floating-move 'left 30))
  [remap bz/window-right]              (@ bz/exwm-floating-move-right (bz/exwm-floating-move 'right 30))
  [remap bz/window-down]               (@ bz/exwm-floating-move-down  (bz/exwm-floating-move 'down 30))
  [remap bz/window-up]                 (@ bz/exwm-floating-move-up    (bz/exwm-floating-move 'up 30))

  [remap bz/split-window-left]        (@ bz/exwm-floating-move-big-left  (bz/exwm-floating-move 'left 200))
  [remap bz/split-window-right]       (@ bz/exwm-floating-move-big-right (bz/exwm-floating-move 'right 200))
  [remap bz/split-window-down]        (@ bz/exwm-floating-move-big-down  (bz/exwm-floating-move 'down 200))
  [remap bz/split-window-up]          (@ bz/exwm-floating-move-big-up    (bz/exwm-floating-move 'up 200))

  [remap bz/shrink-window-horizontal] (@ bz/exwm-shrink-window-horizontal (bz/exwm-floating-resize -8 t))
  [remap bz/grow-window-horizontal]   (@ bz/exwm-grow-window-horizontal   (bz/exwm-floating-resize +8 t))
  [remap bz/shrink-window-vertical]   (@ bz/exwm-shrink-window-vertical   (bz/exwm-floating-resize -2 nil))
  [remap bz/grow-window-vertical]     (@ bz/exwm-grow-window-vertical     (bz/exwm-floating-resize +2 nil))
  [remap bz/shrink-window-horizontal-big] (@ bz/exwm-shrink-window-horizontal-big (bz/exwm-floating-resize -32 t))
  [remap bz/grow-window-horizontal-big]   (@ bz/exwm-grow-window-horizontal-big   (bz/exwm-floating-resize +32 t))
  [remap bz/shrink-window-vertical-big]   (@ bz/exwm-shrink-window-vertical-big   (bz/exwm-floating-resize -12 nil))
  [remap bz/grow-window-vertical-big]     (@ bz/exwm-grow-window-vertical-big     (bz/exwm-floating-resize +12 nil))

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


;;; Sidebar

(defvar bz/sidebar-buffer nil)

(defun bz/sidebar-buffer ()
  (unless (buffer-live-p bz/sidebar-buffer)
    (let ((win (selected-window))
          (buf (current-buffer)))
      ($$ "chromium")
      (while (eq (window-buffer win) buf) (sit-for 0.1))

      (setq bz/sidebar-buffer (window-buffer win))
      (switch-to-buffer buf)))

  bz/sidebar-buffer)

(defun bz/open-sidebar ()
  (interactive)
  (with-current-buffer (bz/sidebar-buffer)
    (let* ((frame-h 1078) (frame-w 1900)
           (w 500) (h (- frame-h (line-pixel-height) 6))
           (x (- frame-w w)) (y 1)
           pos)
      (unless exwm--floating-frame
        (exwm-floating--set-floating exwm--id))

      (select-frame-set-input-focus exwm--floating-frame)

      (exwm-layout-hide-mode-line)

      (exwm-layout--show exwm--id)

      (unless (eq (frame-pixel-height) h)
        (set-frame-height nil h nil t))
      (unless (eq (frame-pixel-width) w)
        (set-frame-width nil w nil t))

      (setq pos (window-inside-absolute-pixel-edges))
      (unless (and (eq x (car pos)) (eq y (cadr pos)))
        (exwm-floating-move (- x (car pos)) (- y (cadr pos)))))))

(defun bz/close-sidebar ()
  (interactive)
  (when (buffer-live-p bz/sidebar-buffer)
    (with-current-buffer bz/sidebar-buffer
      (when (frame-live-p exwm--floating-frame)
        (with-selected-frame exwm--floating-frame
          (exwm-floating-hide)))))
  (select-frame-set-input-focus
   (nth exwm-workspace-current-index exwm-workspace--list)))

(defun bz/toggle-sidebar ()
  (interactive)
  (if (eq (current-buffer) bz/sidebar-buffer)
      (bz/close-sidebar)
    (bz/open-sidebar)))


;;; Monitors and Workspaces

(defun bz/exwm-set-workspace-count (num)
  (exwm-workspace-switch 0)
  (dotimes (_ (max 0 (- num (exwm-workspace--count))))
    (exwm-workspace-add))
  (mapc #'exwm-workspace-delete (nthcdr num exwm-workspace--list)))

(defun bz/list-monitors ()
  (let* ((cmd "xrandr | grep -v '^ ' | grep '\\<connected\\>' | cut -d ' ' -f 1")
         (output (shell-command-to-string cmd)))
    (--filter (not (s-blank? it)) (split-string output "\n"))))

(defun bz/get-monitor-primary ()
  (let ((monitors (bz/list-monitors)))
    (if (member "eDP-1" monitors) "eDP-1"
      (completing-read "Primary Monitor: " monitors))))

(defun bz/get-monitor-secondary (primary)
  (let ((ms (--filter (not (equal it primary)) (bz/list-monitors))))
    (pcase ms
      ('nil (error "Only one monitor connected"))
      (`(,single) single)
      (multiple (completing-read "Secondary Monitor: " multiple)))))

(defun bz/monitors-solo ()
  (interactive)
  (let* ((m1 (bz/get-monitor-primary)))
    ($$ "xrandr --output %s --pos 0x0 --primary" m1)
    (dolist (mon (bz/list-monitors))
      (unless (equal mon m1) ($$ "xrandr --output %s --off" mon)))
    (bz/exwm-set-workspace-count 1)))

(defun bz/monitors-mirror ()
  (interactive)
  (let* ((m1 (bz/get-monitor-primary)))
    ($$ "xrandr --output %s --pos 0x0 --primary" m1)
    (dolist (mon (bz/list-monitors))
      (unless (equal mon m1)
        ($$ "xrandr --output %s --auto --pos 0x0 --scale 1" mon)))
    (bz/exwm-set-workspace-count 1)))

(defun bz/monitors-stack (scale)
  (interactive "nMonitor Scale: ")
  ;; For some reason doing it twice works
  (bz/monitors-stack-1 scale)
  (bz/monitors-stack-1 scale))

(defun bz/monitors-stack-1 (scale)
  (unless (and (numberp scale) (> scale 0.5))
    (setq scale 1))

  (let* ((m1 (bz/get-monitor-primary))
         (m2 (bz/get-monitor-secondary m1))
         (m1-w (string-to-number ($$ "xrandr | awk '/^%s connected/ {match($0, /([0-9]+)x[0-9]+/, a); print a[1]}'" m1)))
         (m2-w (string-to-number ($$ "xrandr | awk '/^%s connected/ {match($0, /([0-9]+)x[0-9]+/, a); print a[1]}'" m2)))
         (m2-h (string-to-number ($$ "xrandr | awk '/^%s connected/ {match($0, /[0-9]+x([0-9]+)/, a); print a[1]}'" m2)))

         (m1-width m1-w)
         (m2-height (truncate (* m2-h scale)))
         (m2-width (truncate (* m2-w scale)))
         (m1-left (if (<= m2-width m1-width) 0 (/ (- m2-width m1-width) 2))))

    ($$ "xrandr --output %s --auto --pos 0x0 --scale %s" m2 scale)
    ($$ "xrandr --output %s --pos %sx%s --primary" m1 m1-left m2-height)

    (setq exwm-randr-workspace-monitor-plist (list 0 m1 1 m2))
    (bz/exwm-set-workspace-count 2)
    (exwm-workspace-switch 1)
    (bz/set-opacity 1)))


;;; Update border width (disabled)

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
  (let* ((class (upcase-initials exwm-class-name))
         (title (->> (or exwm-title "")
                     (replace-regexp-in-string " — Mozilla Firefox$" "")
                     (replace-regexp-in-string " - LibreOffice Writer" "")
                     (replace-regexp-in-string " - Brave" "")))
         (name (->> (format "%s: %s" class title)
                    (replace-regexp-in-string "Brave-Browser: \\(?:\\[\\([^]]+\\)\\]\\).*" "Brave<\\1>"))))
    (unless (and (string-match-p "Brave<.+>" (buffer-name))
                 (not (string-match-p "Brave<.+>" name)))
      (exwm-workspace-rename-buffer name))))

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
                                        (switch-to-buffer ":Electron:"))))))
    ("Gcr-Prompter: Unlock Login Keyring" (kill-buffer (current-buffer)))))

;; (run-with-timer 0.1 0.01 #'bz/kill-gcr-prompter)
;; (defun bz/kill-gcr-prompter ()
;;   (ignore-errors (kill-buffer "Gcr-Prompter: Unlock Login Keyring")))
;; (cancel-function-timers #'bz/kill-gcr-prompter)


;;; Simulation keys

;; (exwm-input-set-simulation-key (kbd "C-M-q") (kbd "M-S-<left>"))
;; (exwm-input-set-simulation-key (kbd "C-M-e") (kbd "M-S-<right>"))
;; (exwm-input-set-simulation-key (kbd "C-M-a") (kbd "C-M-a"))
;; (exwm-input-set-simulation-key (kbd "C-M-d") (kbd "C-<tab>"))
;; (exwm-input-set-simulation-key (kbd "C-M-s") (kbd "C-M-s"))
(setq exwm-input-simulation-keys nil)


;;; Weird frame switching hack

;; This properly focuses exwm windows when switching frames
(bz/advise :after set-window-configuration bz/exwm-refocus-window (&rest _)
  (bz/timer 0.1
    (when (and exwm--id (eq (current-buffer) (window-buffer (selected-window))))
      (let* ((win (selected-window)))
        (select-window (minibuffer-window))
        (bz/timer 0.1 (select-window win))))))

(bz/hook window-configuration-change-hook bz/exwm-refocus-window
  :remove
  ;; Get the cursor out of the stupid echo area
  ;; (bz/timer 0.3
  ;;   (when (and (not (minibuffer-window-active-p (selected-window)))
  ;;              (eq (selected-window) (cadr (window-tree nil))))
  ;;     (select-window (previous-window))))
  )

;; After upgrading to emacs 30.1, whenever any x window is visible, it
;; is very hard to switch focus back to the emacs frame. This is the
;; only way I could figure out how to do it. I think the reason it
;; works is only because it hides and shows the x window again, taking
;; its focus somehow, so the flickering is necessary.
(defun bz/exwm-force-focus ()
  (interactive)
  (when (< (exwm-workspace--count) 2) (exwm-workspace-add))
  (exwm-workspace-switch 1)
  (exwm-workspace-switch 0))

;; Nvm this fixes it
(setq x-no-window-manager t)

(bz/hook window-state-change-hook bz/exwm-force-focus-on-window-change :remove
         ;; (message "-- %s %s %s %s %s" bz/inhibit-refocus bz/last-window bz/current-window exwm--id (selected-window))
         (with-demoted-errors "%s"
           (if exwm--id
               (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (window-absolute-pixel-edges)))
                 ($ "xdotool mousemove %s %s" (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
             ($ "xdotool mousemove 0 1080"))

           ;; This is fixed by x-no-window-manager
           ;; (and (prog1 (null bz/inhibit-refocus) (setq bz/inhibit-refocus nil))
           ;;      (null exwm--id)
           ;;      (null bz/inhibit-refocus)
           ;;      (or (and (buffer-live-p (window-buffer bz/last-window))
           ;;               (buffer-local-value 'exwm--id (window-buffer bz/last-window)))
           ;;          (and (buffer-live-p (window-buffer bz/current-window))
           ;;               (buffer-local-value 'exwm--id (window-buffer bz/current-window))))
           ;;      (run-with-timer 0.1 nil 'bz/exwm-force-focus))
           ))


;;; Block windows stealing focus

(define-advice exwm--on-ClientMessage (:around (orig-fn raw-data synthetic) block-active-window)
  (let ((obj (make-instance 'xcb:ClientMessage)))
    (xcb:unmarshal obj raw-data)
    (unless (= (slot-value obj 'type) xcb:Atom:_NET_ACTIVE_WINDOW)
      (funcall orig-fn raw-data synthetic))))


;;; Action Keybindings

(bz/keys bz/exwm-action-map
  :doc "Keymap that will be prefixed with M- in the global keymap."
  :sparse t
  :parent bz/action-map
  :prefix ("s-" bz/exwm-mod-action-map)

  "o" bz/open-app-by-key
  "O" bz/app-or-window
  ;; "M-C-o" (@ bz/sudo-open-app (bz/app-or-window 'sudo))

  ;; "s" bz/read-emacs-key-sequence
  "s" (switch-to-buffer (get-buffer-create "*scratch*"))

  "a" wosp-select-or-create-template
  "A" wosp-select-or-create-template-family
  "C-S-a" wosp-customize
  "d" wosp-select-screen
  "D" wosp-open-root
  "C-S-d" wosp-unload

  "c" ignore
  "C" ignore

  "z" exwm-floating-toggle-floating
  ;; "z" execute-extended-command
  ;; "z" (@ bz/left-click ($$ "xdotool click 1"))
  "Z" bz/exwm-floating-center

  "C-S-z" (@ bz/kill-emacs ($ "ps -ef | grep emacs | awk '{print $2}' | xargs kill"))
  ;; "M-C-S-w" (@ bz/i3lock ($$ "i3lock -i ~/Media/Wallpaper/BlurredWallpaper.png"))
  "C-S-q" (@ bz/suspend-and-lock
             ($$ "systemctl suspend")
             ($$ "i3lock -i ~/Media/Wallpaper/Icetwigs.png"))
  "C-S-c" (@ bz/cinnamon ($ "cinnamon-session"))
  ;; "C-S-s" (@ bz/shutdown ($ "shutdown now"))
  ;; "C-S-r" (@ bz/reload ($ "reload"))

  "p" (@ bz/exwm-workspace-next
         (exwm-workspace-switch (mod (1+ exwm-workspace-current-index) (exwm-workspace--count))))
  "P" (@ bz/exwm-workspace-move-next
         (let ((next (mod (1+ exwm-workspace-current-index) (exwm-workspace--count))))
           (exwm-workspace-move-window next)
           (exwm-workspace-switch next)
           (run-with-timer 0.2 nil #'select-window (selected-window))))

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

  (?0 ?9) wosp-keyspace-jump
  ")" (@ wosp-assign-0 (wosp-keyspace-root-assign ?0))
  "!" (@ wosp-assign-1 (wosp-keyspace-root-assign ?1))
  "@" (@ wosp-assign-2 (wosp-keyspace-root-assign ?2))
  "#" (@ wosp-assign-3 (wosp-keyspace-root-assign ?3))
  "$" (@ wosp-assign-4 (wosp-keyspace-root-assign ?4))
  "%" (@ wosp-assign-5 (wosp-keyspace-root-assign ?5))
  "^" (@ wosp-assign-6 (wosp-keyspace-root-assign ?6))
  "&" (@ wosp-assign-7 (wosp-keyspace-root-assign ?7))
  "*" (@ wosp-assign-8 (wosp-keyspace-root-assign ?8))
  "(" (@ wosp-assign-9 (wosp-keyspace-root-assign ?9))

  "<XF86AudioMute>" wosp-workspace-1
  "<XF86AudioLowerVolume>" wosp-workspace-2
  "<XF86AudioRaiseVolume>" wosp-workspace-3
  "<XF86AudioPrev>" wosp-workspace-4
  "<XF86AudioPlay>" wosp-workspace-5
  "<XF86AudioNext>" wosp-workspace-6
  "S-<XF86AudioMute>" wosp-set-workspace-1
  "S-<XF86AudioLowerVolume>" wosp-set-workspace-2
  "S-<XF86AudioRaiseVolume>" wosp-set-workspace-3
  "S-<XF86AudioPrev>" wosp-set-workspace-4
  "S-<XF86AudioPause>" wosp-set-workspace-5
  "S-<XF86AudioNext>" wosp-set-workspace-6
  )


(let ((prog-bin (expand-file-name "~/Programs/bin")))
  (unless (member prog-bin exec-path)
    (setenv "PATH" (concat prog-bin ":" (getenv "PATH")))
    (add-to-list 'exec-path (expand-file-name "~/Programs/bin"))))

(defvar
  bz/app-keys
  '(("a" "arandr")
    ("b" "brave --silent-debugger-extension-api")
    ("e" "emacs")
    ("f" "firefox")
    ("g" "gparted" sudo)
    ("m" "blueman-manager")
    ("n" "nemo")
    ("p" "pavucontrol")
    ("v" "evince")
    ("s" "cinnamon-settings")
    ;; ("j" "xhost + ; distrobox enter debian1 -e '~/Programs/intellij/idea'")
    ("w" "nm-connection-editor")
    ))

(defun bz/open-app-by-key ()
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))

    (let* ((key-desc (key-description (vector (bz/exwm-read-key "Key: "))))
           (base-key (-last-item (split-string key-desc "-")))
           (app (alist-get base-key bz/app-keys nil nil #'string=))
           (pwd (when (memq 'sudo app) (read-passwd "Password: ")))
           (default-directory (expand-file-name "~")))

      (cond ((null app) (message "No app found"))
            (pwd (process-send-string ($ (concat "sudo " (car app)))
                                      (concat pwd "\n")))
            (t ($ (car app)))))))

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
  :doc "Exwm keys"
  :sparse t
  :parent bz/exwm-mod-action-map

  "s-<tab>" (@ bz/other-exwm-frame
               (let* ((next (next-frame (selected-frame)))
                      (buf (get-buffer (frame-parameter next 'name))))
                 (if (and buf (buffer-local-value 'exwm--floating-frame buf))
                     (progn (select-frame-set-input-focus next)
                            (message "Floating: %s" (propertize (buffer-name) 'face 'help-key-binding)))
                   (select-frame-set-input-focus
                    (nth exwm-workspace-current-index exwm-workspace--list))
                   (message "Home: %s" (propertize (buffer-name) 'face 'help-key-binding)))))
  "M-<tab>" bz/other-exwm-frame

  "<print>" (@ bz/screenshot
               (let* ((file (expand-file-name (format-time-string "~/Screenshots/%Y-%m-%d_%H:%M:%S.png"))))
                 ($& _ ["shutter -s -e -o %s" file]
                     ($& _ ["setsid xclip -selection clipboard -t image/png -i %s" file]
                         (message "Copied to clipboard: %s" file)))))
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

  "<XF86Search>" (@ bz/exwm-connect-to-linkbuds (bz/bluetooth-connect "LinkBuds S" "90:DA:07:3A:51:27"))
  "<S-XF86Search>" (@ bz/exwm-connect-to-jlab (bz/bluetooth-connect "JLab JBuds Lux ANC" "90:DA:07:3A:51:27"))
  ;; "<home>" (@ bz/exwm-connect-to-speaker (bz/bluetooth-connect "SRS-XB20" "B8:D5:0B:46:97:12"))

  "<f6>" bz/mcsr-thin
  ;; "<mouse-9>" nil
  ;; "<drag-mouse-9>" nil

  ;; Mouse buttons
  ;; "<mouse-9>" ($ "xdotool key F3+F")
  ;; "<drag-mouse-9>" ($ "xdotool key F3+F")

  ;; "<mouse-8>" ($ "xdotool key Shift+F3+F")
  ;; "<drag-mouse-8>" ($ "xdotool key Shift+F3+F")

  ;; "S-<mouse-8>" ($ "xdotool key Shift+F3")
  ;; "S-<drag-mouse-8>" ($ "xdotool key Shift+F3")
  )

(setq bz/bluetooth-dots 0)
(defun bz/bluetooth-connect (name mac)
  (setq bz/bluetooth-dots (mod (1+ bz/bluetooth-dots) 3))
  (message (concat (propertize "Connecting to " 'face 'bold)
                   (propertize name 'face 'help-key-binding)
                   (make-string (1+ bz/bluetooth-dots) ?.)))
  ($ "echo 'connect %s' | bluetoothctl" mac))


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
(unless (and nil (bz/required exwm))
  (bz/package exwm-randr)
  (exwm-randr-enable)

  (exwm-enable)
  (exwm-init)


  ;; (bz/exwm-setup-displays)
  ($$ "xrandr --output eDP-1 --mode 1920x1080 --scale 1")
  ;; ($$ "xrandr --output eDP-1 --mode 3840x2160 --scale 1")
  ;; ($ "xrandr -s 1920x1080")
  ($ "feh --bg-scale ~/Media/Wallpaper/Icetwigs.jpg")

  (run-with-timer 2 nil #'set-frame-width nil 1904 nil t)
  (run-with-timer 2 nil #'set-frame-height nil 1080 nil t)
  (run-with-timer 2 nil #'set-frame-position nil 0 0)
  )


;;; Provide

(provide 'bz-exwm)

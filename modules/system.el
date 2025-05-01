;;; Keyboard settings

($ "xset r rate 250 20")

;; Only setup keyboard on first launch
(unless (boundp 'bz/is-keyboard-setup)
  (setq bz/is-keyboard-setup t)

  ;; Swap windows and alt
  ($ "setxkbmap -option altwin:swap_alt_win")
  ;; Set caps lock to control
  (run-with-timer 2 nil (lambda () ($ "xmodmap ~/.Xmodmap"))))

;; ~/.Xmodmap
;; remove Lock = Caps_Lock
;; keycode 0x42 = Control_L
;; add Control = Control_L
;; keycode 64 =


;;; Don't turn off screen

;; Doesn't work
($ "xset -dpms")

($ "while true; do\nxdotool mousemove_relative -- 1 0\nsleep 290\ndone")

;;; Transparency

(defun bz/set-opacity (opacity)
  (set-frame-parameter (selected-frame) 'alpha (cons opacity opacity))
  (add-to-list 'default-frame-alist (cons 'alpha (cons opacity opacity)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(bz/set-opacity 88)
($ "compton")


;;; Brightness/Gamma

(setq desktop-environment-brightness-small-increment "1%+")
(setq desktop-environment-brightness-small-decrement "1%-")

(defun desktop-environment-brightness-set (value)
  "Set brightness to VALUE."
  (desktop-environment--shell-command-to-string (format desktop-environment-brightness-set-command value))
  (message "%s %s"
           (propertize "Brightness:" 'face 'bold)
           (desktop-environment-brightness-get)))


(setq bz/gamma 1)
(setq bz/gamma-increment 0.2)

(defun bz/change-gamma (amt)
  (setq bz/gamma (max 1 (* 0.1 (round (+ bz/gamma amt) 0.1))))
  ($ "xgamma -gamma %s" bz/gamma)
  (message "%s %.1f" (propertize "Gamma:" 'face 'bold) bz/gamma))


;;; Audio Settings

(desktop-environment-volume-set 0)
(setq desktop-environment-volume-normal-decrement "5%-")
(setq desktop-environment-volume-normal-increment "5%+")
(setq desktop-environment-volume-small-decrement "1%-")
(setq desktop-environment-volume-small-increment "1%+")

(defun bz/current-pa-sink-description ()
  (let* ((sink-name (string-trim (shell-command-to-string "pactl get-default-sink")))
         (name-line (concat "\tName: " sink-name))
         (lines (split-string (shell-command-to-string "pactl list sinks") "\n")))
    (while (and lines (not (string= (pop lines) name-line))))
    (when (and lines (string-match "\tDescription: \\(.*\\)" (car lines)))
      (match-string 1 (car lines)))))

(setq desktop-environment-volume-get-regexp "\\([0-9]+\\) \\[[0-9]+%")
(bz/advise :override desktop-environment-volume-set bz/desktop-environment-volume-set (value)
  (desktop-environment--shell-command-to-string (format desktop-environment-volume-set-command value))
  ;; Round up to the nearest 5%
  (let ((volume (/ (string-to-number (desktop-environment-volume-get)) (/ 65536 100))))
    (message "%s %s%%  %s" (propertize "Volume:" 'face 'bold) volume
             (propertize (format "(%s)"(bz/current-pa-sink-description)) 'face 'bz/message-box)
             )))
;; (bz/face bz/message-box :b (:line-width (1 . -1) :color "gray") :fg "#4A708B" :s italic)
(bz/face bz/message-box :fg "#4A708B" :s italic) ; SkyBlue4


;;; Displays

(defun bz/exwm-setup-displays ()
  (interactive)
  ($$ "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0")
  ;; Sometimes the monitor is DP-1, sometimes DP-2
  ($$ "xrandr --output DP-1 --mode 1920x1080 --pos 0x0")
  ($$ "xrandr --output DP-2 --mode 1920x1080 --pos 0x0")

  ;; (set-frame-size (selected-frame) 800 600 t)
  (set-frame-size (selected-frame) 1905 1080 t)

  ;; By setting the y position to -1, x windows go right to the top of the screen
  (set-frame-position (selected-frame) 0 -1))

;;; Xhost
($ "xhost +")
;;; CPU

;; Make cpupower available to all users:
;; sudo visudo
;; Add line to extra rules:
;; %wheel ALL=(ALL) NOPASSWD: /nix/store/....../bin/cpupower *

(setq bz/cpu-level 1)
(setq bz/cpu-level-names '("Low" "Save" "Mid" "High"))

(defun bz/cpu-up (n)
  (interactive (list 1))
  (setq bz/cpu-level (max 1 (min 4 (floor (+ bz/cpu-level n)))))
  (let ((frq (nth (1- bz/cpu-level) '(1 1.8 2.5 4.7)))
        (gov (if (<= bz/cpu-level 2) "powersave" "performance")))
    (message ($$ "sudo cpupower frequency-set -u %sGHz" frq))
    (message ($$ "sudo cpupower frequency-set -g %s" gov))
    (message "CPU=%s!" (nth (1- bz/cpu-level) bz/cpu-level-names))))

(defun bz/cpu-down (n)
  (interactive (list 1))
  (bz/cpu-up (- n)))

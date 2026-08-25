;; -*- lexical-binding: t; -*-
(defun bz/two-screens ()
  (interactive)
  ($$ "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x1080 --output DP-2 --mode 1920x1080 --pos 0x0")
  (set-frame-height exwm--frame (* 1080 2) nil 'pixels))


(defun bz/one-screen ()
  (interactive)
  ($$ "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --output DP-2 --off")
  (set-frame-height exwm--frame (* 1080 1) nil 'pixels))


;;; Provide

(provide 'bz-screens)
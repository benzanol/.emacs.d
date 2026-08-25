;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'desktop-environment)


;;; Keyboard settings

($ "xset r rate 250 20")


;; All of this has been replaced by keyd in nixos config

;; Only setup keyboard on first launch
(defvar bz/is-keyboard-setup nil)
;; (unless bz/is-keyboard-setup
;;   (setq bz/is-keyboard-setup t)

;;   ($ "while true; do\nxdotool mousemove_relative -- 1 0\nsleep 290\ndone")

;;   ;; Swap windows and alt
;;   ($ "setxkbmap -option altwin:swap_alt_win")
;;   ;; Set caps lock to control
;;   (run-with-timer 2 nil (lambda () ($ "xmodmap ~/.Xmodmap"))))

;; ~/.Xmodmap
;; remove Lock = Caps_Lock
;; keycode 0x42 = Control_L
;; add Control = Control_L
;; keycode 64 =

;; Interpret space when held as alt

;; ($$ "xmodmap -e 'keycode 65 = Alt_L'")
;; ($$ "xcape -e 'Alt_L=space'")


;;; Don't turn off screen

;; Doesn't work
($ "xset -dpms")


;;; Transparency

(defvar bz/opacity 1)
(defun bz/set-opacity (opacity)
  (setq bz/opacity (setq opacity (float opacity)))
  (set-frame-parameter (selected-frame) 'alpha (cons opacity opacity))
  (add-to-list 'default-frame-alist (cons 'alpha (cons opacity opacity)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(defun bz/toggle-transparent ()
  (interactive)
  (if (= bz/opacity 1)
      (bz/set-opacity 0.85)
    (bz/set-opacity 1)))

(bz/set-opacity bz/opacity)
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


(defvar bz/gamma 1)
(defvar bz/gamma-increment 0.1)

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


;;; Xhost

($ "xhost +")


;;; CPU

;; Make cpupower available to all users:
;; sudo visudo
;; Add line to extra rules:
;; %wheel ALL=(ALL) NOPASSWD: /nix/store/....../bin/cpupower *

(defvar bz/cpu-level 1)
(defvar bz/cpu-level-names '("Low" "Save" "Mid" "High"))

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


;;; Provide

(provide 'bz-system)

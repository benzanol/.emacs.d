;; -*- lexical-binding: t; -*-
(defun bz/mouse-position ()
  (let ((out ($$ "xdotool getmouselocation")))
    (unless (string-match "\\`x:\\([0-9]+\\) y:\\([0-9]+\\)"  out)
      (error "Unable to get mouse location: %s" out))
    (cons (string-to-number (match-string 1 out))
          (string-to-number (match-string 2 out)))))

(defun bz/trap ()
  (interactive)
  (run-with-timer 0 0.2 #'bz/trap--check-mouse
                  (bz/mouse-position) #'bz/trap-trigger))

(defun bz/trap--check-mouse (old-pos trigger)
  (let* ((new-pos (bz/mouse-position)))
    (unless (equal old-pos new-pos)
      (funcall trigger))))

(defun bz/trap-cancel ()
  (interactive)
  (cancel-function-timers #'bz/trap--check-mouse))

(defun bz/trap-trigger ()
  (delete-other-windows)
  (switch-to-buffer "*new*")
  (setq-local mode-line-format nil)
  (ignore-errors (echo-bar-mode 0))
  (buffer-face-set :height 0.5)
  (display-line-numbers-mode 0)
  (insert "Message: The application encountered an unexpected exception while processing the request.\nStatus: Connectivity timed out during the handshake protocol.\nRecommendation: Please restart the service and verify your network configuration.")
  (set-left-margin (point-min) (point-max) (round (* 0.7 (window-width) )))

  (redisplay)
  ($$ "sleep 3")
  (kill-buffer "*new*")

  (bz/trap-cancel)
  ($$ "rm -rf /tmp/eltrap")
  ($$ "mkdir /tmp/eltrap")
  ($$ "fswebcam -r 1920x1080 --png 7 /tmp/eltrap/1.png")
  (desktop-environment-brightness-set "0%")
  ($$ "convert /tmp/eltrap/1.png -resize 1920x1080^ /tmp/eltrap/2.png")
  ($$ (concat
       "convert /tmp/eltrap/2.png"
       " -gravity North -font Calibri -undercolor white"
       " -pointsize 90 -annotate +0+0"
       " 'This person tried to access your computer' /tmp/eltrap/3.png"))
  (desktop-environment-brightness-set "100%")
  ($$ "i3lock -i /tmp/eltrap/3.png"))


;;; Provide

(provide 'bz-trap)

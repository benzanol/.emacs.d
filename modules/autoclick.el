(defun bz/click5 ()
  (dotimes (time 4)
    (run-with-timer (* time 1.5) nil (lambda () ($$ "xdotool click 1")))))

(run-with-timer 10 50 #'bz/click5)
(cancel-function-timers #'bz/click5)

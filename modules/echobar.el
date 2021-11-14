(setq echo-bar-function 'qv/echo-bar-function)

(defun qv/colorize ()
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*"))
    (with-current-buffer buf
      (buffer-face-set 'mode-line))))
(setq-default mode-line-format nil)

(require 'battery)
(defun qv/echo-bar-function ()
  (propertize
   (format "  %s  │  %s  │  %s  "
           (qv/battery-format)
           (format-time-string "  %b %d")
           (format-time-string "  %H:%M"))
   'face 'fixed-pitch :foreground "#99AAB8"))

(defun qv/battery-format ()
  (let* ((status (funcall battery-status-function))
         (percent (round (string-to-number (battery-format "%p" status))))
         (power-method (battery-format "%L" status)))
    (format "%s %s   %s%%"
            (if (string= power-method "AC") "⚡" "")
            (cond ((>= percent 95) "")
                  ((>= percent 70) "")
                  ((>= percent 50) "")
                  ((>= percent 15) "")
                  (t ""))
            percent)))

(with-current-buffer " *Echo Area 0*" (buffer-face-set 'mode-line))

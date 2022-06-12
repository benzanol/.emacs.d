(qv/package echo-bar)

(echo-bar-enable)

(setq echo-bar-function 'qv/echo-bar-function)
(setq echo-bar-update-interval 1)
(setq echo-bar-right-padding 4)
(setq qv/echo-bar-height 1.2)

(defun qv/colorize-echo-bar ()
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*" " *Minibuf-1*"))
    (ignore-errors
      (with-current-buffer buf
        (buffer-face-set 'mode-line)))))

(add-hook 'post-command-hook 'qv/colorize-echo-bar)

(qv/face qv/icons :family "all-the-icons")

(defun qv/echo-bar-function ()
  (propertize
   (format "%s%s %s%s%s  │  %s%s"
           (propertize " " 'display `(height ,qv/echo-bar-height))
           (or (ignore-errors (qv/activity-string)) "")
           (or (ignore-errors (qv/battery-format)) "")
           ""
           (format-time-string "  %b %d")
           ""
           (format-time-string "  %H:%M:%S"))))

(require 'battery)
(defun qv/battery-format ()
  (when-let* ((func battery-status-function)
              (status (funcall func))
              (percent (round (string-to-number (battery-format "%p" status))))
              (power-method (battery-format "%L" status)))
    (format "%s %s   %s%%   |  "
            (if (string= power-method "AC") "⚡" "")
            (cond ((>= percent 95) "")
                  ((>= percent 70) "")
                  ((>= percent 50) "")
                  ((>= percent 15) "")
                  (t ""))
            percent)))

(load-file "~/.emacs.d/my-packages/echo-bar.el/echo-bar.el")

(echo-bar-enable)

(with-current-buffer " *Echo Area 0*" (buffer-face-set 'mode-line))

(setq echo-bar-show-in-minibuffer nil)

(setq echo-bar-function 'qv/echo-bar-function)

(defun qv/colorize ()
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*"))
    (with-current-buffer buf
      (buffer-face-set 'mode-line))))
(setq-default mode-line-format nil)

(defun qv/enlarge-icon (str)
  (propertize str 'face '(:slant italic)))

(defun qv/echo-bar-function ()
  (propertize
   (format "%s%s  %s  │  %s%s  │  %s%s  "
           #("   " 0 3 (invisible t))
           #(" " 0 1 (display ((height 1.0) (raise 0.0))))
           (qv/battery-format)
           (qv/enlarge-icon "")
           (format-time-string "  %b %d")
           (qv/enlarge-icon "")
           (format-time-string "  %H:%M"))))

(require 'battery)
(defun qv/battery-format ()
  (let* ((status (funcall battery-status-function))
         (percent (round (string-to-number (battery-format "%p" status))))
         (power-method (battery-format "%L" status)))
    (format "%s %s   %s%%"
            (if (string= power-method "AC") "⚡" "")
            (qv/enlarge-icon
             (cond ((>= percent 95) "")
                   ((>= percent 70) "")
                   ((>= percent 50) "")
                   ((>= percent 15) "")
                   (t "")))
            percent)))

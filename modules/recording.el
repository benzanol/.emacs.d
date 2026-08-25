;; -*- lexical-binding: t; -*-

(require 'bz-base)


;; On nixos, install nixos.ffmpeg-full
(defvar bz/recording-file nil)
(define-minor-mode bz/recording-mode "Recording the screen"
  :global t
  :init-value nil
  :group nil

  (if (not bz/recording-mode) (progn ($ "pkill ffmpeg") (message "Stopped recording"))
    (dolist (exec '("ffmpeg" "slop" "pkill"))
      (unless (executable-find exec) (setq bz/recording-mode nil) (error "%s not installed" exec)))
    (let* ((output ($$ "slop -f '%%x %%y %%w %%h'"))
           (nums (when (string-match "[0-9]+ [0-9]+ [0-9]+ [0-9]+" output)
                   (mapcar #'string-to-number (split-string (match-string 0 output) " ")))))
      (if (not nums) (progn (setq bz/recording-mode nil) (error "Slop output malformed"))
        (message "%s" nums)
        (message "ffmpeg -f x11grab -video_size %sx%s -framerate 25 -i :0.0+%s,%s -c:v libx264 -preset ultrafast %s"
                 (nth 2 nums) (nth 3 nums)
                 (nth 0 nums) (nth 1 nums)
                 (setq bz/recording-file
                       (format "~/Media/Recordings/recording-%s.mp4" (format-time-string "%Y-%m-%d-%T"))))
        ($ "ffmpeg -f x11grab -video_size %sx%s -framerate 25 -i :0.0+%s,%s -c:v libx264 -preset ultrafast %s"
           (nth 2 nums) (nth 3 nums)
           (nth 0 nums) (nth 1 nums)
           (setq bz/recording-file
                 (expand-file-name
                  (format "~/Media/Recordings/recording-%s.mp4"
                          (format-time-string "%Y-%m-%d-%T")))))
        (message "Recording %s" bz/recording-file)))))


;;; Provide

(provide 'bz-recording)

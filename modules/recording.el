(define-minor-mode bz/recording-mode "Recording the screen"
  :global t
  :init-value nil

  (if (not bz/recording-mode) (progn ($ "pkill ffmpeg") (message "Stopped recording"))
    (let* ((output ($$ "slop -f '%%x %%y %%w %%h'"))
           (nums (when (string-match-p "[0-9]+ [0-9]+ [0-9]+ [0-9]+" output)
                   (mapcar #'string-to-number (split-string output " ")))))
      (if (not nums) (progn (setq bz/recording-mode nil) (message "Recording Cancelled"))
        ($ "ffmpeg -f x11grab -video_size %sx%s -framerate 25 -i :0.0+%s,%s -c:v libx264 -preset ultrafast %s"
           (nth 2 nums) (nth 3 nums)
           (nth 0 nums) (nth 1 nums)
           (setq bz/recording-file
                 (format "~/Media/Recordings/recording-%s.mp4" (format-time-string "%Y-%m-%d-%T"))))
        (message "Recording %s" bz/recording-file)))))

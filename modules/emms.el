(qv/package emms)

(setq emms-directory (expand-file-name "~/.emacs.d/.cache/emms"))

(emms-standard)
(emms-default-players)
(emms-mode-line-mode 0)

(qv/hook emms-playlist-mode-hook nil
  (display-line-numbers-mode 0))

(qv/face emms-playlist-track-face)
(qv/face emms-playlist-selected-face highlight)

(defun qv/emms-play-file (file)
  "Play a file, defaulting to the Media/Music directory"
  (interactive (list (read-file-name "Play File: " "~/Media/Music/")))
  (setq emms-repeat-track t)
  (emms-play-file file))

(setq emms-repeat-track t)

;; Record actions in a log file, to view later which songs were listened too for how long
(defvar qv/emms-log-file "~/Media/Music/log.txt")

(defun qv/emms-record (str)
  (shell-command-to-string
   (format "echo '%s %s' >> '%s'" str
           (format-time-string "%Y-%m-%d %H:%M:%S [%s]")
           (expand-file-name qv/emms-log-file))))

(defun qv/emms-record-stop ()
  (qv/emms-record "STOPPING"))

(defun qv/emms-record-pause ()
  (qv/emms-record (if emms-player-paused-p "RESUMING" "PAUSING")))

(defun qv/emms-record-dired-start ()
  (when (string-match "/Music/" (dired-get-filename))
    (qv/emms-record (concat "PLAYING " (file-name-base (dired-get-filename))))))

;; Record pauses and plays before actually doing the action
(advice-add 'emms-pause :before 'qv/emms-record-pause)
(advice-add 'emms-play-dired :before 'qv/emms-record-dired-start)

;; Record stops, but only when stop is called interactively
(defun emms-stop ()
  "Stop any current EMMS playback."
  (interactive (ignore (qv/emms-record-stop)))
  (when emms-player-playing-p
    (emms-player-stop)))

;; Parse the logfile
(defun qv/emms-parse-logs (&optional str)
  (setq str (or str (with-temp-buffer (insert-file-contents "~/Media/Music/log.txt") (buffer-string))))

  (let (songs time prev start)
    (dolist (a (--map (split-string it " ") (split-string str "\n" t)))
      (setq time (string-to-number (substring (car (last a)) 1 -1)))

      ;; Record the stop time of the previous song
      (when start
        (if (assoc prev songs)
            (setcdr (assoc prev songs) (+ (- time start) (cdr (assoc prev songs))))
          (push (cons prev (- time start)) songs)))

      (cond ((string= (nth 0 a) "PLAYING")
             (setq prev (nth 1 a) start time))
            ((string= (nth 0 a) "RESUMING")
             (when prev (setq start time)))
            ((string= (nth 0 a) "PAUSING")
             (setq start nil))
            ((string= (nth 0 a) "STOPPING")
             (setq prev nil start nil))))
    (map-apply
     (lambda (a b)
       (list a (ignore-errors (/ b (qv/emms-get-length a)))
             (format "%s:%s:%s" (/ b 3600) (% (/ b 60) 60) (% b 60))))
     (sort songs (lambda (a b) (> (cdr a) (cdr b)))))))

(defun qv/emms-get-length (name)
  (let ((split
         (split-string
          (shell-command-to-string
           (format "%s ; find | grep '%s' | xargs -I{} ffmpeg -i {} 2>&1 | %s"
                   "cd ~/Media/Music" name
                   "grep -oE \"[0-9]{1}:[0-9]{2}:[0-9]{2}\""))
          ":")))
    (+ (* 3600 (string-to-number (nth 0 split)))
       (* 60 (string-to-number (nth 1 split)))
       (string-to-number (nth 2 split)))))


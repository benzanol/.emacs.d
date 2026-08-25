;; -*- lexical-binding: t; -*-
(bz/package emms)

(setq emms-directory (expand-file-name "~/.emacs.d/.cache/emms"))

(emms-standard)
(emms-default-players)
(emms-mode-line-mode 0)

(bz/keys *
  "C-x C-p C-e" emms-pause)

(bz/hook emms-playlist-mode-hook bz/disable-line-numbers
  (display-line-numbers-mode 0))

(bz/face emms-playlist-track-face)
(bz/face emms-playlist-selected-face highlight)

(defun bz/emms-play-file (file)
  "Play a file, defaulting to the Media/Music directory"
  (interactive (list (read-file-name "Play File: " "~/Media/Music/")))
  (setq emms-repeat-track t)
  (emms-play-file file))

(setq emms-repeat-track t)

;; Record actions in a log file, to view later which songs were listened too for how long
(defvar bz/emms-log-file "~/Media/Music/log.txt")

(defun bz/emms-record (str)
  ($ "echo '%s %s' >> '%s'" str
     (format-time-string "%Y-%m-%d %H:%M:%S [%s]")
     (expand-file-name bz/emms-log-file)))

(defun bz/emms-record-stop ()
  (bz/emms-record "STOPPING"))

(defun bz/emms-record-pause ()
  (bz/emms-record (if emms-player-paused-p "RESUMING" "PAUSING")))

(defun bz/emms-record-dired-start ()
  (when (string-match "/Music/" (dired-get-filename))
    (bz/emms-record (concat "PLAYING " (file-name-base (dired-get-filename))))))

;; Record pauses and plays before actually doing the action
(bz/advise :before emms-pause bz/emms-record-pause)
(bz/advise :before emms-play-dired bz/emms-record-dired-start)

;; Record stops, but only when stop is called interactively
(defun emms-stop ()
  "Stop any current EMMS playback."
  (interactive (ignore (bz/emms-record-stop)))
  (when emms-player-playing-p
    (emms-player-stop)))

;; Parse the logfile
(defun bz/emms-parse-logs (&optional str)
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
       (list a (ignore-errors (/ b (bz/emms-get-length a)))
             (format "%s:%s:%s" (/ b 3600) (% (/ b 60) 60) (% b 60))))
     (sort songs (lambda (a b) (> (cdr a) (cdr b)))))))

(defun bz/emms-get-length (name)
  (let ((split
         (split-string
          ($$ "cd ~/Media/Music ; find | grep '%s' | xargs -I{} ffmpeg -i {} 2>&1 | grep -oE \"[0-9]{1}:[0-9]{2}:[0-9]{2}\"" name)
          ":")))
    (+ (* 3600 (string-to-number (nth 0 split)))
       (* 60 (string-to-number (nth 1 split)))
       (string-to-number (nth 2 split)))))


;;; Provide

(provide 'bz-emms)
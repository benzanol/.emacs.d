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
(qv/keys * "C-x C-p" qv/emms-play-file)

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
  (qv/emms-record (concat "PLAYING " (file-name-base (dired-get-filename)))))

;; Record pauses and plays before actually doing the action
(advice-add 'emms-pause :before 'qv/emms-record-pause)
(advice-add 'emms-play-dired :before 'qv/emms-record-dired-start)

;; Record stops, but only when stop is called interactively
(defun emms-stop ()
  "Stop any current EMMS playback."
  (interactive (ignore (qv/emms-record-stop)))
  (when emms-player-playing-p
    (emms-player-stop)))

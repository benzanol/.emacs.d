(defun ytdlp-download (url &optional format)
  (interactive
   (let* ((url (read-string "Url: "))
          (url-arg (shell-quote-argument url))
          (cmd (format "yt-dlp -F '%s'" url-arg))
          ;; (lines (split-string (shell-command-to-string cmd) "\n"))
          (lines (split-string ytdlp-output "\n"))
          (_ (message "%s" lines))
          (_ (while (not (string-match-p "^─+$" (pop lines))))) ; Remove up to the header line
          (format-line (completing-read "Format: " lines))
          (format (car (split-string format-line " "))))
     (list url format)))
  (let ((ytdl-args (if format (list "-f" format url) (list url))))
    (apply #'start-process "yt-dlp" "*ytdlp*" "ytdlp" ytdl-args)))


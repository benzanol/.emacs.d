;; import java.io.{File, FileOutputStream}

(defvar ob-scala-session nil
  "Stores the scala repl process for executing scala code.")

(defvar ob-scala-output-directory "/tmp/ob-scala"
  "Directory for storing the output of org babel scala blocks.")

(defvar ob-scala-output-buffer nil
  "Buffer that reads the output from executed scala code.")

(defun ob-scala-start-session ()
  (when (process-live-p ob-scala-session)
    (kill-process ob-scala-session))
  (setq ob-scala-session (start-process-shell-command "scala" "*ob-scala*" "scala")))

(defun ob-scala-execute (string process &optional results name)
  (message "RESULTS:%s" results)
  (unless name (setq name (number-to-string (abs (random)))))
  (unless results (setq results 'value))
  (let ((out-file (format "%s/%s" ob-scala-output-directory name))
        (null-file "/dev/null")
        (print-end "print(\"----END----\")"))
    (if (or (eq results 'value) (eq results 'output))
        (prog1 out-file
          (unless (file-regular-p out-file) (make-empty-file out-file))
          (process-send-string
           process
           (format "Console.withOut(new java.io.FileOutputStream(new java.io.File(\"%s\"))){%s\n%s}\n"
                   (if (eq results 'value) out-file null-file)
                   (format "print(Console.withOut(new java.io.FileOutputStream(new java.io.File(\"%s\"))){%s})"
                           (if (eq results 'output) out-file null-file)
                           (if (eq results 'output) (format "%s\n%s" string print-end) string))
                   print-end)))
      (process-send-string
       process (format "Console.withOut(new java.io.FileOutputStream(new java.io.File(\"%s\"))){%s}\n"
                       null-file string))
      nil)))

(defun ob-scala-execute-block ()
  (interactive)
  (when-let* ((info (org-babel-get-src-block-info))
              (props (nth 2 info))
              (file (ob-scala-execute
                     (cadr info)
                     ob-scala-session
                     (alist-get :result-type props)
                     (nth 4 info)))
              (marker (set-marker (make-marker) (point)))
              (org-buf (current-buffer))
              (output-buf
               (or (get-file-buffer file)
                   (save-window-excursion (find-file file)))))
    (when (buffer-live-p ob-scala-output-buffer)
      (kill-buffer ob-scala-output-buffer))
    (setq ob-scala-output-buffer output-buf)
    (org-babel-remove-result)
    (org-babel-insert-result "Working...")
    (with-current-buffer output-buf
      (rename-buffer (concat " " (file-name-base buffer-file-name)))
      (auto-revert-mode)
      (setq-local
       after-revert-hook
       `(lambda ()
          (with-current-buffer ,output-buf
            (when (string= "----END----"(buffer-substring (- (point-max) 11) (point-max)))
              (ob-scala-display-result (buffer-substring 1 (- (point-max) 11)) ,marker)
              (kill-buffer ,output-buf))))))))

(defun ob-scala-display-result (result marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (org-babel-remove-result)
      (org-babel-insert-result result))))

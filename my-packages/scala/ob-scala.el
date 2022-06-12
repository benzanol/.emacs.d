;;; Code:

(defvar ob-scala-process nil
  "Stores the scala repl process for executing scala code.")

(defvar ob-scala-output-file "/tmp/ob-scala-output"
  "File for storing the code output.")

(defvar ob-scala-directory "/tmp/ob-scala"
  "Directory to store scala scripts.")

(defvar ob-scala-output-buffer nil
  "Buffer that reads the output from executed scala code.")

(defvar ob-scala-queue nil
  "List of currently running or waiting scala processes.")

(defvar ob-scala-use-ammonite nil
  "Use ammonite instead of the default scala shell.")

(defvar-local ob-scala-last-end-pos nil
  "The position in the output buffer where the last output ended.")

(defun ob-scala-start-process ()
  "Create a new ammonite session in a buffer called *ob-scala*."
  (interactive)
  ;; Kill the previous process if its still running
  (when (process-live-p ob-scala-process)
    (kill-process ob-scala-process))

  ;; Reset the queue
  (setq ob-scala-queue nil)

  ;; Prepare the output buffer
  (setq ob-scala-output-buffer
        (save-window-excursion (find-file ob-scala-output-file)))
  (with-current-buffer ob-scala-output-buffer
    (auto-revert-mode 0)
    (delete-region (point-min) (point-max))
    (write-file ob-scala-output-file)
    (auto-revert-mode 1)
    (add-hook 'after-revert-hook 'ob-scala-check-output nil t))

  ;; Start the scala session
  (setq ob-scala-process
        (start-process-shell-command
         "scala" "*ob-scala*"
         (format "%s > '%s'" (if ob-scala-use-ammonite "amm" "scala")
                 ob-scala-output-file)))

  ;; If using normal scala, add the :silent option to remove uneccessary output
  (unless ob-scala-use-ammonite
    (process-send-string ob-scala-process ":silent\n")))

(defun ob-scala-check-output ()
  "Check the output buffer to check if an actual output was produced."
  (with-current-buffer ob-scala-output-buffer
    ;; Locate the most recent :END: line, if there is one
    (goto-char (point-max))

    (when (search-backward-regexp "^:END:[0-9]+$" ob-scala-last-end-pos t)
      ;; Check if actually at an :END: line
      (let* ((end-pos (1- (point)))
             (id-str (buffer-substring (+ (point) 5) (line-end-position)))
             (assoc (assoc (string-to-number id-str) ob-scala-queue))
             contents)
        (when assoc

          ;; Remove the call with the id from the list of calls
          (setq ob-scala-queue (delq assoc ob-scala-queue))

          ;; Parse the contents of the output
          (when (and (functionp (cdr assoc))
                     (search-backward (format "\n:BEGIN:%s\n" id-str) nil t))
            (forward-line 2)
            (setq contents (replace-regexp-in-string
                            "\n+\\'" "" (buffer-substring (point) end-pos))))

          ;; Call the function
          (when contents (funcall (cdr assoc) contents))

          ;; Update the new end position to be the end of the buffer
          (setq ob-scala-last-end-pos (point-max)))))))

(defun ob-scala-execute (string &optional out-func)
  "Execute STRING, and then call OUT-FUNC with the output.

OUT-FUNC should be a function that takes one argument.
The string form of the final value (not the entire output) will be
passed as the argument."

  (unless (process-live-p ob-scala-process)
    (ob-scala-start-process))

  (let* ((id (abs (random)))
         (filename (format "%s/%s.scala" ob-scala-directory id))
         (code (format "print(\"\\n%s%s\\n\")\n%s\nprint(\"\\n%s%s\\n\")"
                       ":BEGIN:" id string ":END:" id)))
    (make-directory ob-scala-directory t)
    (if ob-scala-use-ammonite
        (process-send-string ob-scala-process (format "{\n%s\n}\n" code))

      (if t
          ;; Execute by creating and loading a temporary script file
          (progn (with-temp-buffer (insert code) (write-file filename))
                 (process-send-string ob-scala-process (format ":load %s\n" filename)))

        ;; Execute by using the :paste option to insert multiple lines
        ;; Doesn't usually work for some reason
        (process-send-string ob-scala-process (format ":paste\n%s\n" code))
        (process-send-string ob-scala-process (kbd "C-d"))))

    (push (cons id out-func) ob-scala-queue)
    id))

(defun ob-scala-display-result-func (marker)
  "Return a function to insert a result at MARKER"
  `(lambda (result)
     (with-current-buffer (marker-buffer ,marker)
       (save-excursion
         (goto-char (marker-position ,marker))
         (org-babel-remove-result)
         (org-babel-insert-result result)))))

(defun org-babel-execute:scala (body params)
  "Execute a block of scala code with org babel."
  (interactive)
  (let* ((m (set-marker (make-marker) (point)))
         (r (alist-get :results params)))
    (if (or (string= r "none") (string= r "silent"))
        (ob-scala-execute body)
      (ob-scala-execute body (ob-scala-display-result-func m)))
    nil))

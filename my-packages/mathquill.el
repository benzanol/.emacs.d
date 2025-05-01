(setq mq-buffer "*mathquill*")
(setq mq-process-bufferj "*mathquill-process*")

(setq mq-program-dir "~/Test/Node/desmos")
(setq mq-output-filename "screenshot.png")

(setq mq-display-delay 0.025)

(setq mq-latex nil)


;;; Displaying equation

(defun mq-start-process ()
  (interactive)

  (let ((prc (get-buffer-process (get-buffer-create mq-process-buffer))))
    ;; Trigger a graceful exit, killing the chrome process
    (when prc (process-send-string prc "exit\n")))

  (with-current-buffer mq-process-buffer
    (setq-local default-directory (expand-file-name mq-program-dir))
    (let ((prc (make-process
                :name "Mathquill"
                :buffer mq-process-buffer
                :command '("distrobox" "enter" "ubuntu1")
                :filter #'mq-process-filter)))
      (process-send-string prc "node ./build/main.js\n")
      prc)))

(defun mq-process-filter (process output)
  (push output ls)
  (let ((str (string-trim output))
        (lines (->> (split-string output "\n" t)
                    (--map (if (s-ends-with-p "" it) (substring it 0 -1) it))
                    (--filter (not (string= it "")))))
        (latex-prefix "<<LATEX>>")
        (image-prefix (concat (string 4194185) "PNG"))
        (image-postfix "<</SCREENSHOT>>")
        line)

    (while lines
      (setq line (pop lines))
      (cond ((s-starts-with-p latex-prefix line)
             (setq mq-latex (substring line (length latex-prefix))))

            ((s-starts-with-p image-prefix line)
             (let* ((image-end (--find-index (string= image-postfix it) lines))
                    (image-data (s-join "\n" (cons line (-slice lines 0 image-end)))))
               (setq lines (when image-end (nthcdr (1+ image-end) lines)))
               (with-current-buffer mq-buffer
                 (delete-region (point-min) (point-max))
                 (insert-image (create-image (string-as-unibyte image-data) nil t)))))

            (t (message "Mathquill said: %s" line))))))

(defun mq-continue-video ()
  (interactive)

  (cancel-function-timers #'mq-continue-video)

  ;; Include the current time into the image spec so that emacs
  ;; doesn't use a cached version of the image
  (let ((img (create-image (concat mq-program-dir "/" mq-output-filename)
                           nil nil :time (current-time))))

    (with-current-buffer (get-buffer-create mq-buffer)
      (save-excursion

        (delete-region (point-min) (point-max))
        (insert (propertize " " 'display img))))

    (run-with-timer mq-display-delay nil #'mq-continue-video)))


;;; Sending keys

(defun mq-send-key (key)
  (interactive (list (string last-input-event)))
  (mq-send-keys (list key)))

(defun mq-send-keys (keys)
  ;; Send "Space" instead of a raw space
  (setq keys (--map (if (string= it " ") "Space" it) keys))

  (let ((prc (or (get-buffer-process mq-process-buffer) (mq-start-process))))
    (process-send-string prc (concat (s-join " " keys) "\n"))))

;; Shift/Control key events don't seem to work
(qv/keys mq-map
  :full t
  "<left>" (mq-send-key "ArrowLeft")
  "<right>" (mq-send-key "ArrowRight")
  "<up>" (mq-send-key "ArrowUp")
  "<down>" (mq-send-key "ArrowDown")

  [remap qvk-left] (mq-send-key "ArrowLeft")
  [remap qvk-right] (mq-send-key "ArrowRight")
  [remap qvk-up] (mq-send-key "ArrowUp")
  [remap qvk-down] (mq-send-key "ArrowDown")
  [remap qvk-left4] (mq-send-keys (make-list 4 "ArrowLeft"))
  [remap qvk-right4] (mq-send-keys (make-list 4 "ArrowRight"))
  [remap qvk-up4] (mq-send-keys (make-list 4 "ArrowUp"))
  [remap qvk-down4] (mq-send-keys (make-list 4 "ArrowDown"))

  "<return>" (mq-send-key "Enter")
  "<backspace>" (mq-send-key "Backspace")
  "<deletechar>" (mq-send-key "Delete")
  [remap qvk-delete-backward-char] (mq-send-key "Backspace")
  [remap qvk-delete-forward-char] (mq-send-key "Delete")

  [remap self-insert-command] mq-send-key

  "C-s" (mq-send-keys (list "\\" "s" "q" "r" "t" " "))
  )

(define-minor-mode mq-mode "Mathquill"
  :keymap mq-map
  )

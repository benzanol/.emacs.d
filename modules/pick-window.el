;; No q
(setq bz/pick-window-chars "asdfghjklwertyuiopzxcvbnm1234567890")

;; List of windows
(setq bz/pick-window-list nil)

(bz/face bz/pick-window-modeline :fg orange :w bold)

(defun bz/pick-window-push-buffer ()
  "Move the current buffer to another window"
  (interactive)

  (let ((buf (current-buffer))
        (win (selected-window)))
    (bz/pick-window)

    (unless (eq win (selected-window))
      (switch-to-buffer buf)

      (with-selected-window win
        (previous-buffer)))))

(defun bz/pick-window-pull-buffer ()
  "Move the buffer in a window to the current window"
  (interactive)

  (let ((orig-win (selected-window))
        picked-buf)

    (bz/pick-window)
    (setq picked-buf (current-buffer))

    (previous-buffer)

    (select-window orig-win)
    (switch-to-buffer picked-buf)))


(defun bz/pick-window-swap-buffers ()
  "Swap the current buffer with a buffer in another window"
  (interactive)

  (let ((orig-win (selected-window))
        (orig-buf (current-buffer))
        new-buf)

    (bz/pick-window)
    (setq new-buf (current-buffer))

    (switch-to-buffer orig-buf)

    (with-selected-window orig-win
      (switch-to-buffer new-buf))))

(defun bz/pick-window-display-buffer (buffer alist)
  (bz/pick-window)
  (window--display-buffer buffer (selected-window) 'reuse alist))

(defun bz/pick-window-mode-line ()
  (-if-let* ((n (-elem-index (selected-window) bz/pick-window-list)))
      (propertize (format "  %s  " (string (aref bz/pick-window-chars n)))
                  'face 'bz/pick-window-modeline)
    "*Invalid Window!*"))

(defun bz/pick-window ()
  (interactive)
  (setq bz/pick-window-list (window-list))

  (let ((mode-lines
         (--map
          (with-current-buffer it
            (prog1 (cons it mode-line-format)
              (setq mode-line-format (list '(:eval (bz/pick-window-mode-line)) mode-line-format))
              (force-mode-line-update)))
          (-uniq (mapcar 'window-buffer bz/pick-window-list)))))

    ;; Find the index of the window
    (unwind-protect
        (if-let ((char (read-char ""))
                 (index (s-index-of (string char) bz/pick-window-chars))
                 (window (nth index bz/pick-window-list)))
            (select-window window)
          (message "Not a valid window"))

      (setq bz/pick-window-list nil)

      ;; Reset the mode line formats
      (dolist (b mode-lines)
        (with-current-buffer (car b)
          (setq mode-line-format (cdr b))
          (force-mode-line-update))))))

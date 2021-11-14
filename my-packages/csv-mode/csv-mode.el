(defun qv/align-csv ()
  (interactive)
  (remove-overlays)
  (save-excursion
    (let* ((face (if buffer-face-mode buffer-face-mode-face 'default))
           (text-scale text-scale-mode-amount)
           (tab tab-width)
           (csv (mapcar
                 (lambda (s) (split-string s ","))
                 (split-string
                  (buffer-substring
                   (point-min) (point-max))
                  "\n")))
           (tabs (make-list (length csv) nil)))
      (message (format "%s" csv))

      (with-temp-buffer
        ;; If the temp buffer isn't also selected, lining up tabs doesn't workd
        (switch-to-buffer (current-buffer))
        (buffer-face-set face)
        (unless (zerop text-scale)
          (text-scale-set text-scale))
        (setq-local tab-width tab)

        ;; Loop through each column of the spreadsheet
        (dotimes (col (length (car csv)))
          (delete-region (point-min) (point-max))

          ;; Insert each row of the current column
          (dotimes (row (length csv))
            (insert (propertize "," 'face '(:height 0.01)))
            (insert "│ ")
            (when (nth col (nth row csv))
              (insert (nth col (nth row csv))))
            (unless (eq row (1- (length csv))) (newline)))

          ;; Align the rows of the current column by inserting the correct number of tabs
          (qv/align-column 1 (line-number-at-pos (point-max)) "")

          ;; Count how many tabs were inserted for each row
          (dotimes (row (length csv))
            (goto-line (1+ row))
            (setf (nth row tabs)
                  (append (nth row tabs)
                          (list (if (search-forward
                                     "\t" (line-end-position) 'noerror)
                                    (1+ (- (line-end-position) (point))) 0)))))))

      ;; Add overlays to the original buffer
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (remove-overlays)
        (remove-text-properties (point-min) (point-max)
                                '(modification-hooks t face t))
        (dotimes (row (length csv))
          (goto-line (1+ row))
          (beginning-of-line)
          (overlay-put (make-overlay (point) (1+ (point)))
                       'before-string "│ ")
          (dotimes (col (length (nth row csv)))
            (search-forward-regexp ",\\|$" (line-end-position))
            (when (string= (buffer-substring (1- (point)) (point)) ",")
              (put-text-property (1- (point)) (point) 'modification-hooks t)
              (put-text-property (1- (point)) (point) 'face '(:height 0.1)))
            (let ((overlay (make-overlay (1- (point)) (point)))
                  (string (concat (make-string (nth col (nth row tabs)) 9) "│ ")))
              (if (eq col (1- (length (nth row csv))))
                  (overlay-put overlay 'after-string string)
                (overlay-put overlay 'after-string string)))))))))

(defvar-local qv/csv-mode nil "Whether csv mode is active")
(defvar-local qv/csv-timer nil "The timer for updating the buffer")

(defun qv/csv-mode ()
  (interactive)
  (setq-local qv/csv-mode t)
  (buffer-face-set 'default)
  (qv/align-csv)

  (setq-local after-change-functions '(qv/csv-modified-function))

  (local-set-key (kbd "C-c C-i") 'qv/csv-insert-column)
  (local-set-key (kbd "C-c C-a") 'qv/csv-append-column)
  (local-set-key (kbd "C-c C-d") 'qv/csv-delete-column)
  (local-set-key (kbd "C-c C-r") 'qv/csv-delete-row)
  (local-set-key (kbd "C-c C-u") 'qv/undo)
  (local-set-key (kbd "M-h") 'qv/csv-move-left)
  (local-set-key (kbd "M-l") 'qv/csv-move-right)
  (evil-define-key 'normal 'local "j" 'qv/csv-move-down)
  (evil-define-key 'normal 'local "k" 'qv/csv-move-up))

(defun qv/csv-modified-function (a b c)
  (when qv/csv-timer
    (cancel-timer qv/csv-timer))
  (setq-local
   qv/csv-timer
   (run-at-time 1 nil
                'qv/csv-update-function
                (current-buffer))))

(defun qv/csv-update-function (buffer)
  (ignore-errors
    (with-current-buffer buffer
      (setq-local qv/csv-timer nil)
      (qv/align-csv))))

(defun qv/csv-undo ()
  (interactive)
  (let ((inhibit-modification-hooks t))
    (with-silent-modifications
      (undo)
      (qv/align-csv))))

(defun qv/csv-current-column ()
  (1+ (length
       (replace-regexp-in-string
        "[^,]" "" (buffer-substring-no-properties
                   (line-beginning-position)
                   (min (line-end-position) (1+ (point))))))))

(defun qv/csv-insert-column (&optional col)
  (interactive)
  (unless col (setq col (qv/csv-current-column)))
  (let ((marker (set-marker (make-marker) (point))))
    (with-silent-modifications
      (dotimes (i (line-number-at-pos (point-max)))
        (goto-line (1+ i))
        (beginning-of-line)
        (if (eq col 1)
            (insert " ,")
          (when (search-forward-regexp ",\\|$" (line-end-position) 'noerror (1- col))
            (unless (eolp) (backward-char))
            (insert ", "))))
      (qv/align-csv)
      (goto-char (marker-position marker))))
  (qv/align-csv))

(defun qv/csv-append-column (&optional col)
  (interactive)
  (unless col (setq col (qv/csv-current-column)))
  (let ((marker (set-marker (make-marker) (point))))
    (with-silent-modifications
      (dotimes (i (line-number-at-pos (point-max)))
        (goto-line (1+ i))
        (when (search-forward-regexp ",\\|$" (line-end-position) 'noerror col)
          (unless (eolp) (backward-char))
          (insert ", ")))
      (qv/align-csv)
      (goto-char (marker-position marker))))
  (qv/align-csv))

(defun qv/csv-delete-column (&optional col)
  (interactive)
  (unless col (setq col (qv/csv-current-column)))
  (let ((marker (set-marker (make-marker) (point))))
    (with-silent-modifications
      (dotimes (i (line-number-at-pos (point-max)))
        (goto-line (1+ i))
        (if (eq col 1)
            (delete-region (line-beginning-position)
                           (search-forward-regexp ",\\|$" (line-end-position) t))
          (when (search-forward "," (line-end-position) t (1- col))
            (delete-region (1- (point))
                           (if (search-forward "," (line-end-position) t)
                               (1- (point)) (line-end-position))))))
      (qv/align-csv)
      (goto-char (marker-position marker))))
  (qv/align-csv))

(defun qv/csv-delete-row (&optional row)
  (interactive)
  (unless row (setq row (qv/csv-current-column)))
  (let ((col (qv/csv-current-column))
        (beg (line-beginning-position))
        (end (min (point-max) (1+ (line-end-position)))))
    (with-silent-modifications
      (remove-overlays beg end)
      (delete-region beg end)
      (beginning-of-line)
      (search-forward "," (line-end-position) t (1- col))))
  (qv/align-csv))

(defun qv/csv-move-left ()
  (interactive)
  (unless (eobp) (forward-char))
  (search-backward-regexp ",\\|^" (line-beginning-position) t 2)
  (unless (bolp) (forward-char)))

(defun qv/csv-move-right ()
  (interactive)
  (forward-char)
  (search-forward-regexp "," (line-end-position) t))

(defun qv/csv-move-up ()
  (interactive)
  (let ((col (qv/csv-current-column)))
    (previous-line) (beginning-of-line)
    (unless (eq col 1)
      (search-forward-regexp ",\\|$" (line-end-position) t (1- col)))))

(defun qv/csv-move-down ()
  (interactive)
  (let ((col (qv/csv-current-column)))
    (next-line) (beginning-of-line)
    (unless (eq col 1)
      (search-forward-regexp ",\\|$" (line-end-position) t (1- col)))))

(defvar-local just-tracked nil
  "Whether or not the last command involved tracking the cursor")
(defvar-local tracked-column 0
  "The column of the cursor to adjust")
(defvar-local tracked-point 0
  "The last recorded point position in a buffer")

(defvar track-column-use-pixels nil)

(defcustom track-column-vertical-functions
  '(next-line previous-line)
  "Vertical movement functions that should keep the column.")

(defun track-column-get ()
  (if track-column-use-pixels
      (car (window-text-pixel-size
            nil (save-excursion (beginning-of-visual-line))
            (point)))
    (current-column)))

(defun track-column-advice (&rest args)
  (setq just-tracked t)
  (unless (eq (line-beginning-position) (line-end-position))
    (while (and (< (track-column-get) tracked-column)
                (not (eolp)))
      (forward-char 1))))

(defun track-column-postcmd ()
  (if just-tracked
      (setq just-tracked nil)
    (unless (eq (point) tracked-point)
      (setq tracked-column (track-column-get))))
  (setq tracked-point (point)))

(define-minor-mode track-column-mode
  "Vertical movement keeps the cursor in the same column."
  nil
  :global t
  (progn
    (if track-column-mode
        (add-hook 'post-command-hook 'track-column-postcmd)
      (remove-hook 'post-command-hook 'track-column-postcmd))
    (dolist (f track-column-vertical-functions)
      (if track-column-mode
          (advice-add f :after 'track-column-advice)
        (advice-remove f 'track-column-advice)))))

(track-column-mode 1)

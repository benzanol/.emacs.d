;;; multiplecursors2 --- Multiple cursors but better -*- lexical-binding: t; -*-

(qv/package dash)
(qv/package seq)

;;; Variables
(defvar-local qvmc-mode nil
  "Can be either nil, `all`, `one`, or `none`")

(defvar-local qvmc-cursors nil
  "List of fake cursors.")

(defvar qvmc-run-once nil ; Doesn't work
  "Can be set to non-nil by a command to only run it once.")

(defvar qvmc-cursor-number nil
  "Buffer index of the cursor currently being executed.")

(defvar qvmc-default-mode 'all
  "Default mode to enter when enabling multiple cursors.
Can be either `all`, `one`, or `none`.")

(defvar qvmc-cursor-number nil
  "The index of the cursor in the cursor list.")

(defvar qvmc-postpone-commands
  '(execute-extended-command)
  "Don't run these commands for all cursors, but if this command
invokes another command, run that command for all cursors.")

;;(defvar qvmc-separate-commands
(setq qvmc-separate-commands
      '(undo undo-tree-undo undo-tree-redo)
      )
;;"Run these commands as if multiple cursors was disabled.")

;;; Keymap

(qv/keys qvk-visual-map
  "A" qvmc-add-lines
  "C-n" qvmc-add-next-match)

(qv/keys qvmc-map
  :sparse t
  "C-q" qvmc-disable
  "C-v" qvmc-all
  "C-o" qvmc-one
  "C-s" qvmc-none ; Separate

  "C-d" qvmc-delete-cursor
  "C-c" qvmc-condensed-mode
  "C-r" qvmc-insert-range
  "C-SPC" qvmc-align-cursors

  "C-j" qvmc-forward-cursor
  "C-k" qvmc-backward-cursor

  "C-a" qvmc-add-cursor
  "RET" qvmc-add-next-line
  "C-n" qvmc-add-next-match
  "C-m" qvmc-add-all-matches)

(qv/key * "C-v" ,qvmc-map)

(qv/keys qvmc-minor-mode-map
  :sparse t
  "C-q" qvmc-disable
  "C-o" qvmc-one
  "C-s" qvmc-none
  "C-j" qvmc-forward-cursor
  "C-k" qvmc-backward-cursor
  "C-a" qvmc-add-cursor
  "C-n" qvmc-add-next-match)

(push (cons 'qvmc-mode qvmc-minor-mode-map)
      minor-mode-map-alist)


;;; Minor modes

(defun qvmc-toggle ()
  (interactive)
  (if qvmc-mode (qvmc-disable) (qvmc-enable)))

(defun qvmc-start ()
  "Reset all variables and enter the default multicursors mode."
  (interactive)
  (setq qvmc-mode qvmc-default-mode)

  (setq qvmc-cursors nil)
  (qvmc-add-cursor (point)))

(defun qvmc-all ()
  "Mirror actions to all cursors"
  (interactive)
  (unless qvmc-mode (qvmc-start))
  (setq qvmc-mode 'all)
  (qvmc-setup-cursor (car qvmc-cursors)))

(defun qvmc-one ()
  "Mirror actions just to the current cursor."
  (interactive)
  (unless qvmc-mode (qvmc-start))
  (setq qvmc-mode 'one)
  (qvmc-setup-cursor (car qvmc-cursors)))

(defun qvmc-none ()
  "Don't mirror any actions."
  (interactive)
  (unless qvmc-mode (qvmc-start))
  (setq qvmc-mode 'none))

(defun qvmc-disable ()
  "Disable multiple cursors."
  (interactive)
  (setq qvmc-mode nil
        qvmc-cursors nil)

  (qvmc-condensed-mode 0)

  (remove-overlays nil nil 'qvmc t)
  (deactivate-mark))

;;; Hide unmatched lines

(define-minor-mode qvmc-condensed-mode
  "Only show lines that contain a cursor."
  :init-value nil
  :global nil
  (if qvmc-condensed-mode
      (qvmc-hide-unmatched-lines)
    (remove-overlays nil nil 'qvmc-unmatched t)))

(defun qvmc-hide-unmatched-lines ()
  "Hide lines that don't contain a cursor."
  (remove-overlays nil nil 'qvmc-unmatched t)
  (let ((separator (propertize "..." 'face 'font-lock-comment-face))
        (lines
         (->> qvmc-cursors
              (-map 'overlay-start)
              (-map 'line-number-at-pos)
              (cons (+ 2 (line-number-at-pos (point-max))))
              (cons -1)
              (-sort '<)
              (-uniq)))
        l1 l2 o)
    (save-excursion
      (while (cdr lines)
        (setq l1 (pop lines) l2 (car lines))
        (when (> (- l2 l1) 2)
          (setq o (make-overlay (progn (goto-line (+ 1 l1)) (point))
                                (progn (goto-line (- l2 0)) (1- (point)))))
          (overlay-put o 'qvmc t)
          (overlay-put o 'qvmc-unmatched t)
          (overlay-put o 'invisible t)
          (overlay-put o 'after-string separator)
          )))))


;;; Executing commands

(defvar qvmc-running nil
  "Set to t if currently running a command for all cursors.")

(advice-add 'command-execute :around 'qvmc--command-execute-advice)
(defun qvmc--command-execute-advice (exec &rest args)
  ;; Temporarily show all lines in condensed mode
  (when qvmc-condensed-mode
    (remove-overlays nil nil 'qvmc-unmatched t))


  ;; Run the main command
  (if (or qvmc-running (memq qvmc-mode '(nil none))
          (memq this-command qvmc-postpone-commands))

      (apply exec args)

    (let ((qvmc-running t))
      (if (or (eq qvmc-mode 'none)
              (memq this-command qvmc-separate-commands))

          (apply exec args)

        (qvmc--command-execute-for-cursors exec args))))


  ;; Restore the unmatched lines
  (when (and qvmc-mode qvmc-condensed-mode)
    (qvmc-hide-unmatched-lines))

  ;; Make sure all the cursors cover one character
  (when qvmc-mode (qvmc-update-cursor-overlays))

  ;; Make sure the point is at the current cursor
  (when (memq qvmc-mode '(all one))
    (goto-char (overlay-start (car qvmc-cursors)))))

(defun qvmc--command-execute-for-cursors (exec args)
  ;; Remove dead cursors
  (setq qvmc-cursors (seq-filter 'overlay-buffer qvmc-cursors))
  (unless qvmc-cursors (qvmc-disable) (error "No cursors found"))

  (let ((cursor-order (qvmc-cursor-position-order)))
    (setq qvmc-cursor-number (seq-position cursor-order (car qvmc-cursors)))

    ;; Run the command only once at first, with the primary cursor
    ;; and remember its arguments by advising the function
    (fset 'qvmc-this-command (car args))
    (setcar args 'qvmc-this-command)
    (advice-add 'qvmc-this-command :filter-args 'qvmc-remember-args-advice)

    (and (qvmc-cursor-execute (car qvmc-cursors) exec args)

         (eq qvmc-mode 'all)

         (progn
           ;; Instead of running the original command, run `qvmk-execute-this-command`
           ;; to wrap the original command with the same arguments for each call
           (setcar args 'qvmc-execute-this-command)

           ;; Run the command for all remaining cursors
           (setq qvmc-cursor-number -1)
           (dolist (c cursor-order)
             (setq qvmc-cursor-number (1+ qvmc-cursor-number))
             (unless (eq c (car qvmc-cursors))
               (qvmc-cursor-execute c exec args)))))

    (when qvmc-cursors
      ;; Load the variables of the current cursor, so the mark is in the correct place
      (qvmc-load-vars (overlay-get (car qvmc-cursors) 'qvmc-vars)))

    (setq qvmc-cursor-number nil)))

;;; Remembering args

(defun qvmc-this-command ()
  "The current raw command executed by `command-execute`")

(defun qvmc-remember-args-advice (args)
  (setq qvmc-this-command-args args))


(defvar-local qvmc-this-command-args nil
  "Argument list used for the last command.")

(defun qvmc-execute-this-command ()
  "Hack to call the current command with advice."
  (interactive)
  (apply 'funcall-interactively
         (symbol-function 'qvmc-this-command)
         qvmc-this-command-args))


;;; Execute a single cursor

(defun qvmc-cursor-execute (o func args)
  ;; Keep track of state before executing
  (let* ((mode qvmc-mode)
         (cursors qvmc-cursors)
         (win-layout (window-tree))
         (buf-contents (buffer-string))
         (old-vs (overlay-get o 'qvmc-vars))
         (new-vs nil))
    (setq qvmc-run-once nil)

    ;; Prepare for the current cursor
    (qvmc-setup-cursor o)

    (apply func args)

    ;; Only update cursor variables when:
    (when (and
           ;; Don't run for all if the command altered the state of the mode
           (eq mode qvmc-mode)
           (eq cursors qvmc-cursors)
           (not qvmc-run-once)

           ;; Don't run if the command changed the buffer or window layotu
           (eq (current-buffer) (overlay-buffer o))
           (equal win-layout (window-tree))

           ;; Don't run unless something has changed about the buffer
           (or (not (equal old-vs (setq new-vs (qvmc-collect-vars))))
               (not (eq (overlay-start o) (point)))
               (not (string= buf-contents (buffer-string)))))

      ;; Move the cursor to the new position
      (move-overlay o (point) (1+ (point)))

      ;; Save the updated variable values
      (overlay-put o 'qvmc-vars new-vs)

      ;; Highlight the region
      (move-overlay (overlay-get o 'qvmc-mark-overlay)
                    (point) (if (or t mark-active) (mark) (point)))
      (overlay-put (overlay-get o 'qvmc-mark-overlay)
                   'face (if mark-active 'region nil))

      t)))


;;; Variables

(defun qvmc-setup-cursor (o)
  "Prepare the point and variables for a certain cursor."
  (goto-char (overlay-start o))
  (qvmc-load-vars (overlay-get o 'qvmc-vars)))

(defvar qvmc-cursor-specific-vars
  '((qvmc-get-mark . qvmc-set-mark)
    mark-active
    kill-ring kill-ring-yank-pointer
    register-alist)
  "List of variables to store for each cursor.")

(defvar qvmc-original-vars nil
  "Alist of original variable values.")

(defun qvmc-load-vars (alist)
  "Load the variables in ALIST into the environment"

  (dolist (a alist)
    (let ((var (car a)) (val (cdr a)))
      (if (consp var)
          (funcall (cdr var) val)
        (set var val)))))

(defun qvmc-collect-vars (&optional vs)
  "Save variables in VS into an alist."

  (--map (cons it
               (if (consp it)
                   (funcall (car it))
                 (symbol-value it)))
         (--filter
          (or (consp it) (boundp it))
          (or vs qvmc-cursor-specific-vars))))

(defun qvmc-set-mark (m)
  "Silently move the mark to a new marker position."
  (when (markerp m) (setq m (marker-position m)))
  (set-marker (mark-marker) m))

(defun qvmc-get-mark ()
  "Silently save the marker position"
  (copy-marker (mark-marker)))


;;; Update cursor overlays

(defvar qvmc-bar-cursor-string
  #(" " 0 1 (face cursor display (height 0.1)))
  "Text to display as the cursor when `cursor-type` is `bar`.")

(defun qvmc-update-cursor-overlays ()
  "Make sure each cursor always has a width of 1."
  (dolist (o qvmc-cursors)
    ;; Make sure the overlay is exactly one character wide
    (when (and (overlay-buffer o)
               (not (= (overlay-end o) (1+ (overlay-start o)))))
      (move-overlay o (1- (overlay-end o)) (overlay-end o))))
  (qvmc-update-cursor-type))

(add-variable-watcher 'cursor-type 'qvmc-update-cursor-type)

(defun qvmc-update-cursor-type (&rest args)
  "Modify the cursor overlays to mimick the current cursor type."
  (when (and (display-graphic-p)
             (memq qvmc-mode '(one all)))
    (let ((props
           (pcase (if args (nth 1 args) cursor-type)
             ((or `(bar . ,_) 'bar) (list nil qvmc-bar-cursor-string))
             ('hollow '((:box t) nil))
             (_ '(cursor nil)))))

      (dolist (o (if (eq qvmc-mode 'one) (list (car qvmc-cursors)) qvmc-cursors))
        (when (overlay-buffer o)
          (overlay-put o 'face (nth 0 props))
          (overlay-put o 'before-string (nth 1 props)))))))

;;; Adding cursors

(defun qvmc-add-cursor (&optional pos mark)
  "Add a new cursor at a specific position."
  (interactive)

  (unless (numberp pos) (setq pos (point)))

  (when (qvmc-cursor-at-pos pos)
    (error "There is already a cursor at position `%s`" pos))

  ;; When adding a cursor from nothing, start separated to allow you
  ;; to add more cursors easily
  (unless qvmc-mode (setq qvmc-mode 'none))

  (let ((c (make-overlay pos (1+ pos)))
        (mo (make-overlay pos pos))
        (m (mark)))

    (overlay-put c 'face 'cursor)
    (overlay-put c 'qvmc t)

    ;; Create the overlay for the region
    (overlay-put c 'qvmc-mark-overlay mo)
    (overlay-put mo 'face (when mark-active 'region))
    (overlay-put mo 'qvmc t)

    ;; Store variable values
    (qvmc-set-mark (or mark (and mark-active (mark)) pos))
    (move-overlay mo (point) (mark))
    (overlay-put c 'qvmc-vars (qvmc-collect-vars))
    (qvmc-set-mark m)

    (push c qvmc-cursors)
    (goto-char (overlay-start c))

    c))

(defun qvmc-add-next-line ()
  "Add a new cursor the line below the last cursor added."
  (interactive)

  (unless qvmc-mode (qvmc-start))

  (qvmc-add-cursor
   (save-excursion
     (goto-char (overlay-start (car qvmc-cursors)))
     (next-line) (point))))

(defun qvmc-add-lines ()
  "Add a new cursor on each line of the region."
  (interactive)

  (let ((b (min (point) (mark)))
        (e (max (point) (mark)))
        (col (current-column)))
    (deactivate-mark)

    (save-excursion
      (goto-char b) (move-to-column col)
      ;; Only enter all mode if multiple cursors is not already activated
      (unless qvmc-mode (qvmc-all))

      (while (progn (forward-line) (<= (point) e))
        (move-to-column col)
        (qvmc-add-cursor)))))

;;; Adding by search
(defun qvmc-add-next-match (text &optional regexp)
  (interactive
   (list (buffer-substring
          (point) (if mark-active (mark) (1+ (point))))))

  ;; Make sure the point is ahead of the mark
  (when (and mark-active (> (mark) (point)))
    (exchange-point-and-mark))

  (unless qvmc-mode (qvmc-start))

  ;; Figure out which function to use for searching
  (let ((search-func (if regexp 'search-forward-regexp 'search-forward))
        cycled success)

    (save-excursion
      ;; Keep searching forward until finding a match without an existing cursor
      (while (and (setq success
                        (or (funcall search-func text nil 'noerror)
                            (and (not cycled)
                                 (progn (setq cycled t)
                                        (beginning-of-buffer)
                                        (funcall search-func text nil 'noerror)))))
                  (qvmc-cursor-at-pos (point))))

      ;; Make sure it found a valid match without a cursor
      (if (not success)
          (error "No more matches found: `%s`" text)

        (qvmc-add-cursor (point) (match-beginning 0))
        (qvmc-all)))))

(defun qvmc-add-all-matches (text &optional regexp)
  (interactive
   (list (buffer-substring
          (point) (if mark-active (mark) (1+ (point))))))

  (qvmc-disable)

  ;; Figure out which function to use for searching
  (let ((search-func (if regexp 'search-forward-regexp 'search-forward))
        (p (point)) (m (mark)))
    (beginning-of-buffer)

    ;; Keep searching forward until finding a match without an existing cursor
    (while (funcall search-func text nil 'noerror)
      (let ((mark-active t))
        (qvmc-add-cursor (point) (match-beginning 0))))

    ;; Try to get the cursor at the original location
    (unless (null qvmc-cursors)
      (qvmc-select-cursor
       (or (qvmc-cursor-at-pos p)
           (qvmc-cursor-at-pos m)
           (car qvmc-cursors))))

    ;; Select all cursors
    (qvmc-all)))

;;; Remove cursor

(defun qvmc-delete-cursor (&optional c)
  "Remove the cursor C, or the current cursor."
  (interactive)

  (setq c (or c (and (eq qvmc-mode 'none)
                     (qvmc-cursor-at-pos))
              (car qvmc-cursors)))

  ;; Remove the cursor from the list and delete the overlay
  (delete-overlay (overlay-get c 'qvmc-mark-overlay))
  (delete-overlay c)
  (setq qvmc-cursors (remove c qvmc-cursors))

  ;; Disable multiple cursors if the cursor list is empty
  (unless qvmc-cursors (qvmc-disable)))

(defun qvmc-delete-cursor-at-point ()
  "Remove the cursor at the point."
  (interactive)
  (dolist (c qvmc-cursors)
    (when (= (overlay-start c) (point))
      (qvmc-delete-cursor c))))

;;; Cycling cursors

(defun qvmc-select-cursor (&optional cursor)
  "Set the cursor at point, or CURSOR, to be the current cursor."
  (interactive)
  (unless cursor (setq cursor (qvmc-cursor-at-pos (point))))

  (unless (and cursor (memq cursor qvmc-cursors))
    (error "No cursor at point"))

  (setq qvmc-cursors (cons cursor (remove cursor qvmc-cursors)))

  (goto-char (overlay-start (car qvmc-cursors))))

(defun qvmc-cycle-cursors (n)
  "Cycle N cursors through the cursor list."

  (interactive "P")

  (let* ((order (qvmc-cursor-position-order))
         (idx (seq-position order (car qvmc-cursors)))
         (new-idx (mod (+ idx (or n 1)) (length order)))
         (newc (nth new-idx order)))
    (qvmc-select-cursor newc)))

(defun qvmc-forward-cursor (&optional n)
  (interactive "P")
  (qvmc-cycle-cursors (if (numberp n) n 1)))

(defun qvmc-backward-cursor (&optional n)
  (interactive "P")
  (qvmc-cycle-cursors (if (numberp n) (- n) -1)))

;;; Miscellaneous
(defun qvmc-cursor-at-pos (&optional pos)
  "Get the cursor at point, or the position POS."
  (--first (= (overlay-start it) (or pos (point)))
           qvmc-cursors))

(defun qvmc-cursor-position-order ()
  "Return a list of cursors in order by position."
  (--sort (< (overlay-start it) (overlay-start other))
          qvmc-cursors))

(defun qvmc-insert-range (&optional n)
  "Insert an increasing range of numbers for each cursor."
  (interactive)
  (insert (number-to-string (+ (or n 1) (or qvmc-cursor-number 0)))))

(defun qvmc-align-cursors (&optional char)
  "Align all cursors to the same column."
  (interactive)
  (setq qvmc-run-once t)
  (save-excursion
    (let* ((cols (--map (progn (goto-char (overlay-start it))
                               (current-column))
                        qvmc-cursors))
           (max (apply 'max cols)))
      (dolist (c qvmc-cursors)
        (goto-char (overlay-start c))
        (insert (make-string (- max (current-column))
                             (or char ?\s)))))))

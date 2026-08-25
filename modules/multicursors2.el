;;; multiplecursors2 --- Multiple cursors but better -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-functions)

(require 'dash)
(require 'ox)


;;; Variables

(bz/face mc2-cursor :bg purple :fg bg)

(defvar-local mc2-mode nil
  "Can be either nil, `all`, `one`, or `none`")

(defvar-local mc2-cursors nil
  "List of fake cursors.")

(defvar mc2-cursor-number nil
  "Buffer index of the cursor currently being executed.")

(defvar mc2-default-mode 'all
  "Default mode to enter when enabling multiple cursors.
Can be either `all`, `one`, or `none`.")

(defvar mc2-cursor-number nil
  "The index of the cursor in the cursor list.")

(defvar mc2-postpone-commands
  '(execute-extended-command)
  "Don't run these commands for all cursors, but if this command
invokes another command, run that command for all cursors.")

(defvar mc2-separate-commands
  '(undo undo-tree-undo undo-tree-redo mc2-align-cursors)
  "Run these commands as if multiple cursors was disabled.")

(defvar mc2-always-execute-all t
  "Run a command for all cursors even if the first cursor doesn't move")

(defvar mc2-run-once nil
  "Set this in commands you want to only be run once")

(defvar mc2-ignore-condensed-mode-commands
  '(mc2-forward-cursor mc2-backward-cursor)
  "When condensed mode is active, unmatched lines are restored
before running the command, and then hidden again after running
the command. For these commands, don't bother doing this.")

(defvar mc2-cursor-results nil
  "Execution result for each cursors.")

(defvar mc2-char-case nil
  "The capitalization type of each cursor.
Can be one of `nil', `upper', `lower'.")


;;; Minor modes

(defun mc2-enable ()
  "Reset all variables and enter the default multicursors mode."
  (interactive)
  (setq mc2-mode mc2-default-mode)

  (setq mc2-cursors nil)
  (mc2-add-cursor (point)))

(defun mc2-disable ()
  "Disable multiple cursors."
  (interactive)

  ;; Copy the combination of all the kills appended together
  ;; Don't if its too long (this has caused problems)
  (let ((rings (--map (alist-get 'kill-ring (overlay-get it 'mc2-vars))
                      (mc2-cursor-position-order)))
        kill)
    (unless (--every (eq it (car rings)) rings)
      (setq kill (mapconcat #'car rings "\n")))

    (setq mc2-mode nil
          mc2-cursors nil
          mc2-char-case nil)

    (mc2-condensed-mode 0)

    (remove-overlays nil nil 'mc2 t)
    (deactivate-mark)

    (when kill (kill-new kill))))

(defun mc2-all ()
  "Mirror actions to all cursors"
  (interactive)
  (unless mc2-mode (mc2-enable))
  (unless (eq mc2-mode 'all)
    (setq mc2-mode 'all)
    (mc2-setup-cursor (car mc2-cursors))))

(defun mc2-one ()
  "Mirror actions just to the current cursor."
  (interactive)
  (unless mc2-mode (mc2-enable))
  (setq mc2-mode 'one)
  (mc2-setup-cursor (car mc2-cursors)))

(defun mc2-none ()
  "Don't mirror any actions."
  (interactive)
  (unless mc2-mode (mc2-enable))
  (setq mc2-mode 'none))


;;; Hide unmatched lines

(define-minor-mode mc2-condensed-mode
  "Only show lines that contain a cursor."
  :init-value nil
  :global nil
  (if mc2-condensed-mode
      (mc2-hide-unmatched-lines)
    (remove-overlays nil nil 'mc2-unmatched t)))

(defun mc2-hide-unmatched-lines ()
  "Hide lines that don't contain a cursor."
  (remove-overlays nil nil 'mc2-unmatched t)
  (let ((separator (propertize "..." 'face 'font-lock-comment-face))
        (lines
         (->> mc2-cursors
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
          (setq o (make-overlay (progn (goto-char (point-min)) (forward-line l1) (point))
                                (progn (goto-char (point-min)) (forward-line (1- l2)) (1- (point)))))
          (overlay-put o 'mc2 t)
          (overlay-put o 'mc2-unmatched t)
          (overlay-put o 'invisible t)
          (overlay-put o 'after-string separator))))))


;;; Executing commands

(defvar mc2-running nil
  "Set to t if currently running a command for all cursors.")

(bz/advise :around command-execute mc2--command-execute-advice (exec &rest args)
  ;; If mc2 mode is disabled or none, run normally
  (if (or mc2-running (null mc2-mode) (eq mc2-mode 'none)
          (null this-command) (memq this-command mc2-postpone-commands))
      ;; Just run the command normally
      (apply exec args)

    (let ((mc2-running t)
          (buf (current-buffer))
          (ignore-condensed (memq this-command mc2-ignore-condensed-mode-commands)))

      ;; Temporarily show all lines in condensed mode
      (when (and mc2-condensed-mode (not ignore-condensed))
        (remove-overlays nil nil 'mc2-unmatched t))

      ;; If the current command is specified not to run for all
      (if (memq this-command mc2-separate-commands)
          (apply exec args)

        (mc2--command-execute-for-cursors exec args))

      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Hide all unmatched lines
          (when (and mc2-mode mc2-condensed-mode (not ignore-condensed))
            (mc2-hide-unmatched-lines))

          ;; Make sure all the cursor overlays cover one character
          (when mc2-mode (mc2-update-cursor-overlays))

          ;; Make sure the point is at the current cursor
          (when (memq mc2-mode '(all one))
            (goto-char (overlay-start (car mc2-cursors)))))))))

(defun mc2--command-execute-for-cursors (exec args)
  "Execute the current command for all cursors.

If the command ends up changing the window layout or the state of
the multicursors plugin after the first cursor, skip the
remaining cursors."

  ;; Remove dead cursors
  (setq mc2-cursors (-filter #'overlay-buffer mc2-cursors))
  (unless mc2-cursors (mc2-disable) (error "No cursors found"))

  ;; Sort the cursors by their position in the buffer
  (let ((cursor-order (mc2-cursor-position-order)))
    (setq mc2-cursor-number (-elem-index (car mc2-cursors) cursor-order))

    (setq mc2-cursor-results nil)

    ;; Run the command only once at first, with the primary cursor
    ;; and remember its arguments by advising the function
    (fset 'mc2-this-command (car args))
    (setcar args 'mc2-this-command)
    (advice-add 'mc2-this-command :filter-args 'mc2-remember-args-advice)

    (and (mc2-first-cursor-execute (car mc2-cursors) exec args)

         (eq mc2-mode 'all)

         (progn
           ;; Instead of running the original command, run `mc2-execute-this-command`
           ;; to wrap the original command with the same arguments for each call
           (setcar args 'mc2-execute-this-command)

           ;; Run the command for all remaining cursors
           (setq mc2-cursor-number -1)
           (dolist (c cursor-order)
             (setq mc2-cursor-number (1+ mc2-cursor-number))
             (unless (eq c (car mc2-cursors))
               (mc2-cursor-execute c exec args)))))

    (when mc2-cursors
      ;; Load the variables of the current cursor, so the mark is in the correct place
      (mc2-load-vars (overlay-get (car mc2-cursors) 'mc2-vars)))

    (setq mc2-cursor-number nil)))


;;; Remembering args

(defvar-local mc2-this-command-args nil
  "Argument list used for the last command.")

(defun mc2-this-command ()
  "The current raw command executed by `command-execute`")

(defun mc2-remember-args-advice (args)
  (setq mc2-this-command-args args))


(defun mc2-execute-this-command ()
  "Hack to call the current command with advice."
  (interactive)
  (apply 'funcall-interactively
         (symbol-function 'mc2-this-command)
         mc2-this-command-args))


;;; Execute a single cursor

(defun mc2-cursor-execute (o func args)
  "Execute a single cursor without any conditions attached."

  ;; Prepare for the current cursor
  (mc2-setup-cursor o)

  (push (apply func args) mc2-cursor-results)

  ;; Move the cursor to the new position
  (move-overlay o (point) (1+ (point)))

  ;; Save the updated variable values
  (overlay-put o 'mc2-vars (mc2-collect-vars))

  ;; Highlight the region
  (if (not mark-active)
      (overlay-put (overlay-get o 'mc2-mark-overlay) 'face nil)
    (move-overlay (overlay-get o 'mc2-mark-overlay) (point) (mark))
    (overlay-put (overlay-get o 'mc2-mark-overlay) 'face 'region)))

(defun mc2-first-cursor-execute (o func args)
  "Executes the first cursor and returns whether to continue.

Keep track of various state before and after execution in order
to determine whether to execute the remaining cursors.  A non-nil
return means the remaining cursors should be executed.

O is the cursor overlay, FUNC is the function to execute, and
ARGS are the arguments to pass to the function. "

  ;; Keep track of state before executing
  (setq mc2-run-once nil)
  (let* ((vars '(mc2-mode mc2-cursors mc2-cursor-number mc2-condensed-mode mc2-run-once))
         (vals (mapcar 'symbol-value vars))
         (win-layout (window-tree))
         (buf-contents (buffer-string))
         (old-vs (overlay-get o 'mc2-vars)))

    ;; Prepare and execute the current cursor
    (mc2-setup-cursor o)
    (setq mc2-cursor-results (list (apply func args)))

    ;; Only update cursor variables when:
    (and
     ;; Don't run for all if the command altered the state of the mode or added/removed cursors
     (equal vals (mapcar 'symbol-value vars))

     ;; Don't run if the command changed the buffer or window layout
     (eq (current-buffer) (overlay-buffer o))
     ;; The previous window-tree always `eq` the current one, but doesn't always `equal`
     (equal win-layout (window-tree))

     ;; Don't run unless something has changed about the buffer, or
     ;; mc2-always-execute-all says to run regardless
     (or mc2-always-execute-all
         (not (equal old-vs (mc2-collect-vars)))
         (not (eq (overlay-start o) (point)))
         (not (string= buf-contents (buffer-string))))

     (progn
       ;; Move the cursor to the new position
       (move-overlay o (point) (1+ (point)))

       ;; Save the updated variable values
       (overlay-put o 'mc2-vars (mc2-collect-vars))

       ;; Highlight the region
       (if (not mark-active)
           (overlay-put (overlay-get o 'mc2-mark-overlay) 'face nil)
         (move-overlay (overlay-get o 'mc2-mark-overlay) (point) (mark))
         (overlay-put (overlay-get o 'mc2-mark-overlay) 'face 'region))

       t))))


;;; Variables

(defun mc2-setup-cursor (o)
  "Prepare the point and variables for a certain cursor."
  (goto-char (overlay-start o))
  (mc2-load-vars (overlay-get o 'mc2-vars)))

;; goal-column doesn't function right
(defvar mc2-cursor-specific-vars
  '((mc2-get-mark . mc2-set-mark)
    mark-active
    kill-ring kill-ring-yank-pointer
    mark-ring
    mc2-char-case
    bz/last-kill)
  "List of variables to store for each cursor.")

(defvar mc2-original-vars nil
  "Alist of original variable values.")

(defun mc2-load-vars (alist)
  "Load the variables in ALIST into the environment"

  (dolist (a alist)
    (let ((var (car a)) (val (cdr a)))
      (if (consp var)
          (funcall (cdr var) val)
        (set var val)))))

(defun mc2-collect-vars (&optional vs)
  "Save variables in VS into an alist."

  (--map (cons it
               (if (consp it)
                   (funcall (car it))
                 (symbol-value it)))
         (--filter
          (or (consp it) (boundp it))
          (or vs mc2-cursor-specific-vars))))

(defun mc2-set-mark (m)
  "Silently move the mark to a new marker position."
  (when (markerp m) (setq m (marker-position m)))
  (set-marker (mark-marker) m))

(defun mc2-get-mark ()
  "Silently save the marker position"
  (copy-marker (mark-marker)))


;;; Update cursor overlays

(defvar mc2-bar-cursor-string
  #(" " 0 1 (face mc2-cursor display (height 0.1)))
  "Text to display as the cursor when `cursor-type` is `bar`.")

(defun mc2-update-cursor-overlays ()
  "Make sure each cursor always has a width of 1."
  (dolist (o mc2-cursors)
    ;; Move the end to right after the start
    (when (and (overlay-buffer o)
               (not (= (overlay-end o) (1+ (overlay-start o)))))
      (move-overlay o (overlay-start o) (1+ (overlay-start o))))

    ;; If the cursor is at the end of the buffer
    (overlay-put o 'after-string (when (eq (overlay-start o) (point-max))
                                   (propertize " " 'face 'mc2-cursor))))
  (mc2-update-cursor-type))

(add-variable-watcher 'cursor-type 'mc2-update-cursor-type)

(defun mc2-update-cursor-type (&rest args)
  "Modify the cursor overlays to mimick the current cursor type."
  (when (and (display-graphic-p)
             (memq mc2-mode '(one all)))
    (let ((props
           (pcase (if args (nth 1 args) cursor-type)
             ((or `(bar . ,_) 'bar) (list nil mc2-bar-cursor-string))
             ('hollow '((:box t) nil))
             (_ '(mc2-cursor nil)))))

      (dolist (o (if (eq mc2-mode 'one) (list (car mc2-cursors)) mc2-cursors))
        (when (overlay-buffer o)
          (overlay-put o 'face (nth 0 props))
          (overlay-put o 'before-string (nth 1 props)))))))


;;; Adding cursors

(defun mc2-add-cursor (&optional pos mark)
  "Add a new cursor at a specific position."
  (interactive)

  (unless (numberp pos) (setq pos (point)))

  (when (mc2-cursor-at-pos pos)
    (error "There is already a cursor at position `%s`" pos))

  ;; When adding a cursor from nothing, start separated to allow you
  ;; to add more cursors easily
  (unless mc2-mode (setq mc2-mode 'none))

  (let ((c (make-overlay pos (1+ pos)))
        (mo (make-overlay pos pos))
        (m (mark)))

    (overlay-put c 'face 'mc2-cursor)
    (overlay-put c 'mc2 t)

    ;; Create the overlay for the region
    (overlay-put c 'mc2-mark-overlay mo)
    (overlay-put mo 'face (when mark-active 'region))
    (overlay-put mo 'mc2 t)

    ;; Store variable values
    (mc2-set-mark (or mark (and mark-active (mark)) pos))
    (move-overlay mo (point) (mark))
    (overlay-put c 'mc2-vars (mc2-collect-vars))
    (mc2-set-mark m)

    (push c mc2-cursors)
    (goto-char (overlay-start c))

    c))

(defun mc2-add-next-line ()
  "Add a new cursor the line below the last cursor added."
  (interactive)

  (unless mc2-mode (mc2-enable))

  (mc2-add-cursor
   (save-excursion
     (goto-char (overlay-start (car mc2-cursors)))
     (forward-line) (point))))

(defun mc2-add-lines (col beg end)
  "Add a new cursor on each line of the region."
  (interactive
   (list (current-column)
         (min (point) (mark))
         (max (point) (mark))))

  (let ((chars (eq (line-number-at-pos beg) (line-number-at-pos end))))
    (when chars (setq end (1- end)))

    (deactivate-mark)

    (save-excursion
      (goto-char beg) (unless chars (move-to-column col))
      ;; Only enter all mode if multiple cursors is not already activated
      (unless mc2-mode (mc2-all))

      (while (progn (if chars (forward-char) (forward-line)) (<= (point) end))
        (unless chars (move-to-column col))
        (mc2-add-cursor)))))


;;; Adding by selection

(defun mc2-add-next-word ()
  (interactive)
  (forward-word)
  (mc2-add-cursor))

(defun mc2-add-next-match (text &optional regexp back)
  (interactive (list (buffer-substring (point) (mark))))
  (unless mark-active (user-error "No active selection"))

  ;; Make sure the point is ahead of the mark
  (when (and mark-active (> (mark) (point)))
    (exchange-point-and-mark))

  (unless mc2-mode (mc2-add-cursor))

  ;; Figure out which function to use for searching
  (let ((search-func (if back (if regexp #'search-backward-regexp #'search-backward)
                       (if regexp #'search-forward-regexp #'search-forward)))
        cycled success)

    ;; Keep searching forward until finding a match without an existing cursor
    (save-excursion
      (while (and (setq success
                        (or (funcall search-func text nil 'noerror)
                            (and (not cycled)
                                 (progn (setq cycled t)
                                        (goto-char (point-min))
                                        (funcall search-func text nil 'noerror)))))
                  (mc2-cursor-at-pos (match-end 0)))))

    ;; Make sure it found a valid match without a cursor
    (if (not success) (user-error "No more matches found: `%s`" text)

      (goto-char (match-beginning 0))
      (mc2-add-cursor (match-end 0) (match-beginning 0))
      (dolist (ol (overlays-in (match-beginning 0) (match-end 0)))
        (when (overlay-get ol 'invisible)
          (overlay-put ol 'mc2-invisible (overlay-get ol 'invisible))
          (overlay-put ol 'invisible nil)))
      (mc2-all))))

(defun mc2-add-previous-match (text)
  (interactive (list (buffer-substring (point) (mark))))
  (mc2-add-next-match text nil 'back))


(defun mc2-add-next-word-match (text)
  (interactive (list (buffer-substring (point) (mark))))
  (mc2-add-next-match (format "\\<%s\\>" (regexp-quote text)) 're nil))

(defun mc2-add-previous-word-match (text)
  (interactive (list (buffer-substring (point) (mark))))
  (mc2-add-next-match (format "\\<%s\\>" (regexp-quote text)) 're 'back))

(defun mc2-add-next-skip ()
  (interactive)
  (let ((c (car mc2-cursors)))
    (call-interactively #'mc2-add-next-match)
    (mc2-delete-cursor c)))

(defun mc2-add-previous-skip ()
  (interactive)
  (let ((c (car mc2-cursors)))
    (call-interactively #'mc2-add-previous-match)
    (mc2-delete-cursor c)))

(defun mc2-add-next-word-skip ()
  (interactive)
  (let ((c (car mc2-cursors)))
    (call-interactively #'mc2-add-next-word-match)
    (mc2-delete-cursor c)))

(defun mc2-add-previous-word-skip ()
  (interactive)
  (let ((c (car mc2-cursors)))
    (call-interactively #'mc2-add-previous-word-match)
    (mc2-delete-cursor c)))


;;; Add all matches

(defun mc2-add-all-word-matches (string)
  (interactive (list (buffer-substring
                      (point) (if mark-active (mark) (forward-sexp) (point)))))
  (mc2-add-all-matches (format "\\<%s\\>" (regexp-quote string)) 'REGEXP))

(defun mc2-add-all-matches (text &optional regexp beg end)
  (interactive
   (list (buffer-substring
          (point) (if mark-active (mark) (forward-sexp) (point)))))

  (unless regexp
    (setq text (regexp-quote text)))

  (mc2-disable)

  ;; Figure out which function to use for searching
  (let ((active mark-active)
        (p (point)) (m (mark)))

    ;; Go to the start of the search region
    (goto-char (or beg (point-min)))

    ;; Keep searching forward until finding a match without an existing cursor
    (while (search-forward-regexp text end 'NOERR)
      (let ((mark-active active))
        (mc2-add-cursor (point) (match-beginning 0))))

    ;; Try to get the cursor at the original location
    (goto-char p)
    (cond ((null mc2-cursors))
          ((eq 1 (length mc2-cursors)) (mc2-disable))
          (t
           (mc2-select-cursor
            (or (mc2-cursor-at-pos p)
                (mc2-cursor-at-pos m)
                (car mc2-cursors)))
           (mc2-all)))))


;;; Adding by search

(defun mc2-add-search (text)
  (interactive "sAdd matches: ")
  (if (not mark-active) (mc2-add-all-matches text)
    (let ((beg (min (mark) (point))) (end (max (mark) (point))))
      (deactivate-mark)
      (mc2-add-all-matches text nil beg end))))

(defun mc2-add-regexp-search (text)
  (interactive "sAdd regexp: ")
  (if (not mark-active) (mc2-add-all-matches text 'regexp)
    (let ((beg (min (mark) (point))) (end (max (mark) (point))))
      (deactivate-mark)
      (mc2-add-all-matches text 'REGEXP beg end))))


;;; Remove cursor

(defun mc2-delete-cursor (&optional c)
  "Remove the cursor C, or the current cursor."
  (interactive)

  (setq c (or c (and (eq mc2-mode 'none)
                     (mc2-cursor-at-pos))
              (car mc2-cursors)))

  ;; Re-enable invisible overlays
  (let ((m (overlay-get c 'mc2-mark-overlay)))
    (dolist (ol (overlays-in (overlay-start m) (overlay-end m)))
      (overlay-put ol 'invisible (overlay-get ol 'mc2-invisible))))

  ;; Remove the cursor from the list and delete the overlay
  (delete-overlay (overlay-get c 'mc2-mark-overlay))
  (delete-overlay c)
  (setq mc2-cursors (remove c mc2-cursors))

  ;; Disable multiple cursors if the cursor list is empty
  (unless mc2-cursors (mc2-disable)))

(defun mc2-delete-cursor-at-point ()
  "Remove the cursor at the point."
  (interactive)
  (dolist (c mc2-cursors)
    (when (= (overlay-start c) (point))
      (mc2-delete-cursor c))))


;;; Cycling cursors

(defun mc2-reverse-cursors ()
  "Reverse the order of the cursors"
  (interactive)

  (setq mc2-run-once t)

  (let* ((cs (mc2-cursor-position-order)))
    (dotimes (i (/ (length cs) 2))
      (let* ((c1 (nth i cs)) (c2 (nth (- (length cs) 1 i) cs))
             (v1 (overlay-get c1 'mc2-vars)) (v2 (overlay-get c2 'mc2-vars)))
        (overlay-put c1 'mc2-vars v2)
        (overlay-put c2 'mc2-vars v1)))))


(defun mc2-select-cursor (&optional cursor)
  "Set the cursor at point, or CURSOR, to be the current cursor."
  (interactive)
  (unless cursor (setq cursor (mc2-cursor-at-pos (point))))

  (unless (and cursor (memq cursor mc2-cursors))
    (error "No cursor at point"))

  (setq mc2-cursors (cons cursor (remove cursor mc2-cursors)))

  (goto-char (overlay-start (car mc2-cursors))))

(defun mc2-cycle-cursors (n)
  "Cycle N cursors through the cursor list."

  (interactive "P")

  (let* ((order (mc2-cursor-position-order))
         (idx (-elem-index (car mc2-cursors) order))
         (new-idx (mod (+ idx (or n 1)) (length order)))
         (newc (nth new-idx order)))
    (mc2-select-cursor newc)))

(defun mc2-forward-cursor (&optional n)
  (interactive "P")
  (mc2-cycle-cursors (if (numberp n) n 1)))

(defun mc2-backward-cursor (&optional n)
  (interactive "P")
  (mc2-cycle-cursors (if (numberp n) (- n) -1)))


;;; Conditional Capitalization

(bz/advise :around self-insert-command mc2-self-insert-advice (cmd n &optional char)
  (cond ((or (null mc2-char-case) (not (numberp char))) (funcall cmd n char))
        ((and (eq mc2-char-case 'upper) (>= char ?a) (<= char ?z))
         (funcall cmd n (- char 32)))
        ((and (eq mc2-char-case 'lower) (>= char ?A) (<= char ?Z))
         (funcall cmd n (+ char 32)))
        (t (funcall cmd n char))))

(defun mc2-remember-case ()
  (interactive)
  (let* ((str (buffer-substring (point) (if mark-active (mark) (1+ (point)))))
         (case-fold-search nil)
         (lower (string-match-p "[a-z]" str))
         (upper (string-match-p "[A-Z]" str)))
    (setq mc2-char-case (when (xor upper lower) (if upper 'upper 'lower)))))

(defun mc2-forget-case ()
  (interactive)
  (setq mc2-char-case nil))


;;; Miscellaneous

(defun mc2-cursor-at-pos (&optional pos)
  "Get the cursor at point, or the position POS."
  (--first (= (overlay-start it) (or pos (point)))
           mc2-cursors))

(defun mc2-cursor-position-order ()
  "Return a list of cursors in order by position."
  (--sort (< (overlay-start it) (overlay-start other))
          mc2-cursors))

(defun mc2-insert-range (&optional arg)
  "Insert an increasing range of numbers for each cursor.

A numeric prefix arg sets the starting number. A single prefix
arg sets the starting number to 0."
  (interactive "P")
  (let ((n (or mc2-cursor-number 0)))
    (pcase arg
      ('(4) (insert (aref "abcdefghijklmnopqrstuvwxyz" n)))
      ('(16) (insert (downcase (org-export-number-to-roman (1+ n)))))
      ((guard (or (null arg) (numberp arg)))
       (insert (number-to-string (+ n (or arg 1))))))))

(defun mc2-align-cursors (&optional char)
  "Align all cursors to the same column."
  (interactive)
  (save-excursion
    (let* ((cols (--map (progn (goto-char (overlay-start it))
                               (current-column))
                        mc2-cursors))
           (max (apply 'max cols)))
      (dolist (c mc2-cursors)
        (goto-char (overlay-start c))
        (insert (make-string (- max (current-column))
                             (or char ?\s)))))))


;;; Provide

(provide 'bz-multicursors2)

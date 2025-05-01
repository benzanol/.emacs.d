(require 'subr-x)
(require 'dash)
(require 's)
(require 'avy)

(defvar-local flow-boxes nil
  "Boxes used in the current flowchart.")

(defvar-local flow-arrows nil
  "Cached arrows for the current flowchart.

Each arrow has the form ((start-box . end-box) (points....) props...)")

(defvar-local flow-width nil
  "Width of the current flowchart")

(defvar-local flow-height nil
  "Height of the current flowchart")

(defvar flow--arrow-chars '(">" "<" "↓" "↑")
  "Arrow characters to use")

(setq flow--arrow-chars '("→" "←" "↓" "↑"))
(setq flow--arrow-chars '(">" "<" "v" "∧"))

;;; Major mode

(defvar flow-mode-map (make-sparse-keymap)
  "Local keymap for `flow-mode`.")
(define-key flow-mode-map (kbd "C-c C-b") 'flow-add-box)
(define-key flow-mode-map (kbd "C-c C-d") 'flow-delete-box)
(define-key flow-mode-map (kbd "C-c C-a") 'flow-add-arrow)
(define-key flow-mode-map (kbd "C-c C-w") 'flow-delete-arrow)
(define-key flow-mode-map (kbd "C-c C-c") 'flow-edit-box)
(define-key flow-mode-map (kbd "M-<right>") (lambda () (interactive) (flow-move-current-box +1 0)))
(define-key flow-mode-map (kbd "M-<left>") (lambda () (interactive) (flow-move-current-box -1 0)))
(define-key flow-mode-map (kbd "M-<down>") (lambda () (interactive) (flow-move-current-box 0 +1)))
(define-key flow-mode-map (kbd "M-<up>") (lambda () (interactive) (flow-move-current-box 0 -1)))

(define-derived-mode flow-mode fundamental-mode "Flow"
  "Major mode for working with flowcharts."

  (use-local-map flow-mode-map)

  (add-hook 'before-save-hook 'flow--before-save nil 'local)
  (add-hook 'after-save-hook 'flow--after-save nil 'local)

  (add-hook 'post-command-hook 'flow--update-highlight nil 'local)

  ;; Read the boxes from file
  (unless flow-boxes
    (let ((bs (ignore-errors (read (buffer-string)))))
      ;; Make sure it really is a boxes object
      (when (and (listp bs) (-all-p 'listp bs) (--all-p (>= (length it) 2) bs)
                 (--all-p (and (numberp (car it)) (eq 'box (cadr it))) bs))
        (setq flow-boxes bs))))

  ;; Generate the arrows
  (setq flow-arrows nil)
  (dolist (b1 flow-boxes)
    (dolist (id (plist-get b1 :arrows))
      (when-let ((b2 (assq id flow-boxes)))
        (push (list (cons b1 b2) (flow--generate-arrow b1 b2)) flow-arrows))))

  (flow-reload))


;;; Saving the flowchart

(defvar-local flow--before-save-state nil
  "Saved state from before saving a flowchart.
Has format (TEXT POINT-X POINT-Y)")

(defun flow--before-save ()
  "Before saving, insert the flowchart data into the buffer."
  (setq flow--before-save-state
        (list (buffer-string)
              (current-column)
              (1- (line-number-at-pos))))

  (dolist (b flow-boxes)
    (plist-put b :box-face (list :inherit nil))
    (plist-put b :arrow-face (list :inherit nil)))

  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert (prin1-to-string flow-boxes))))

(defun flow--after-save ()
  "After saving, restore the original flowchart state."

  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert (car flow--before-save-state)))

  (apply 'flow--goto-char (cdr flow--before-save-state)))

;;; Fancy colors

(defface flow-current-box '((default (:foreground "SkyBlue1")))
  "Face for highlighting the box under the cursor.")

(defface flow-current-arrows '((default (:foreground "LightGoldenrod1")))
  "Face for highlighting arrows coming from the current box.")

(defcustom flow-highlight-current-box nil
  "If non-nil, highlight the current box under the cursor."
  :type 'boolean)

(defun flow-highlight-box (&optional box)
  "Highlight the box under the cursor."
  (interactive)
  (unless box (setq box (flow--box-at-point)))

  (dolist (b flow-boxes)
    (when-let ((bf (plist-get b :box-face)))
      (plist-put bf :inherit (and (eq b box) '(flow-current-box))))

    (when-let ((af (plist-get b :arrow-face)))
      (plist-put af :inherit (and (eq b box) '(flow-current-arrows)))))

  (dolist (b (--map (assq it flow-boxes) (plist-get box :arrows)))
    (when-let ((f (plist-get b :box-face)))
      (plist-put f :inherit '(flow-current-arrows)))))

(defun flow--update-highlight ()
  "Called by `post-command-hook` to maybe highlight the current box."
  (when flow-highlight-current-box
    (flow-highlight-box)))


;;; Utility funcitons

(defun flow--goto-char (x y)
  "Go to a certain x and y position in a flowchart."
  (goto-char (+ 1 x (* y (1+ flow-width)))))

(defun flow--box-at-point (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (when-let*
        ((x (current-column)) (y (1- (line-number-at-pos)))
         (box (--first (let ((bx (plist-get it :x)) (by (plist-get it :y)))
                         (and (>= x bx) (< x (+ bx (plist-get it :w)))
                              (>= y by) (< y (+ by (plist-get it :h)))))
                       flow-boxes)))
      box)))

(defun flow--max-width (bs)
  "The maximum x position of any box in BS."
  (if (null bs) 0
    (apply 'max (--map (+ (plist-get it :x) (plist-get it :w)) bs))))

(defun flow--max-height (bs)
  "The maximum y position of any box in BS."
  (if (null bs) 0
    (apply 'max (--map (+ (plist-get it :y) (plist-get it :h)) bs))))

;;; Drawing the flowchart

(defun flow-reload ()
  "Draw the complete flowchart object, FLOWCHART, in the buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (boxes flow-boxes)
         (point-x (current-column))
         (point-y (1- (line-number-at-pos)))
         prev-dir arrow-start face)

    ;; Prepare the buffer
    (setq flow-width (flow--max-width boxes)
          flow-height (flow--max-height boxes))

    (setq-local truncate-lines t)
    (delete-region (point-min) (point-max))
    (remove-overlays)

    ;; Fill the buffer with spaces
    (dotimes (y flow-height)
      (insert (make-string flow-width ?\s))
      (newline))

    ;; Draw the boxes (first in the list on top)
    (dolist (box (reverse boxes))
      (when-let*
          ((x (plist-get box :x))
           (y (plist-get box :y))
           (w (plist-get box :w))
           (h (plist-get box :h))
           (face (plist-get box :box-face))
           (text (plist-get box :text))
           ;; Get the list of lines from the box function
           (lines (funcall flow-box-function text w h)))

        ;; Insert each line as generated by the box function
        (dotimes (i h)
          (flow--goto-char x (+ y i))
          (delete-region (point) (+ w (point)))
          (insert (propertize (nth i lines) 'face face)))))

    ;; Draw the arrows (first in the list on top)
    (dolist (arrow (-filter 'cadr flow-arrows))
      (setq arrow-start t
            face (cons (plist-get (caar arrow) :box-face)
                       (plist-get (caar arrow) :arrow-face))
            arrow (cadr arrow))
      (while (>= (length arrow) 2)
        (when-let*
            ((p1 (pop arrow)) (x1 (car p1)) (y1 (cdr p1))
             (p2 (car arrow)) (x2 (car p2)) (y2 (cdr p2))
             (one-equals (or (= x1 x2) (= y1 y2)))
             ;; Right=0 Left=1 Down=2 Up=3
             (dir (cond ((> x2 x1) 0) ((< x2 x1) 1)
                        ((> y2 y1) 2) ((< y2 y1) 3)))
             ;; How much to change the x and y values for each new char
             (dx (nth dir '(1 -1 0 0)))
             (dy (nth dir '(0 0 1 -1)))
             ;; Characters to use for the line and corner
             (char (string (aref "──││" dir)))
             (first-char (string (aref "├┤┬┴" dir)))
             (corner (if (not prev-dir) char
                       (string (aref "──╮╯──╭╰╰╯││╭╮││" (+ (* 4 prev-dir) dir))))))
          (setq prev-dir dir)

          ;; Add the corner char
          (flow--goto-char x1 y1)
          (if arrow-start
              (flow--insert-arrow-char first-char (car face))
            (flow--insert-arrow-char corner (cdr face)))

          (setq arrow-start nil)

          ;; Keep adding line characters until the target position is reached
          (setq x1 (+ x1 dx) y1 (+ y1 dy))
          (while (not (and (= x1 x2) (= y1 y2)))
            (flow--goto-char x1 y1)
            (flow--insert-arrow-char char (cdr face))
            (setq x1 (+ x1 dx) y1 (+ y1 dy)))))

      ;; Add the arrowhead
      (let ((char (nth prev-dir flow--arrow-chars)))
        (flow--goto-char (caar arrow) (cdar arrow))
        (flow--insert-arrow-char char (cdr face))))

    (flow--goto-char point-x point-y)))

(defun flow--box-char-union (c1 c2)
  ;; Right=0 Left=1 Down=2 Up=3
  (cond
   ((member c1 flow--arrow-chars) c1)
   ((member c2 flow--arrow-chars) c2)
   (t (let* ((cs '(("─" 0 1) ("│" 2 3)
                   ("├" 0 2 3) ("┤" 1 2 3)
                   ("┬" 0 1 2) ("┴" 0 1 3)
                   ("╮" 1 2) ("╯" 1 3)
                   ("╭" 0 2) ("╰" 0 3)
                   ("┼" 0 1 2 3)))
             (ds1 (cdr (assoc c1 cs)))
             (ds2 (cdr (assoc c2 cs)))
             (ds (sort (-uniq (append ds1 ds2 nil)) '<)))
        (car (--find (equal (cdr it) ds) cs))))))

(defun flow--insert-arrow-char (c face)
  (let ((cur (buffer-substring (point) (1+ (point)))))
    (when (or (not (member cur '("→" "←" "↓" "↑")))
              (member c '("→" "←" "↓" "↑")))
      (delete-forward-char 1)
      (insert
       (if (not (string-match cur "─│├┤┬┴╭╮╰╯→←↓↑"))
           (propertize c 'face (list face))
         (propertize
          (or (flow--box-char-union c cur) c)
          'face (cons face (get-text-property 0 'face cur))))))))


(setq flow-box-function 'flow--box-with-border)

(defun flow--box-default (text w &optional h)
  "Generate a list of lines, W long, padded with spaces from TEXT.
The text will be shortened or padded to H lines if H is non-nil."

  (let ((split (--map (split-string it " ") (split-string text "\n")))
        lines words line)
    (while (and (or h split) (or (not h) (< (length lines) h)))
      (setq line "" words (car split))

      (when words
        (if (<= (length (car words)) w)
            (setq line (pop words))

          ;; If the word is too long to fit on a single line, split it
          (setq line (substring (car words) 0 w))
          (setcar words (substring (car words) w)))

        ;; Make sure the next word will fit on the line with a space
        (while (and words (<= (+ 1 (length line) (length (car words))) w))
          (setq line (concat line " " (pop words)))))

      ;; Update the split structure with the new words, or remove the
      ;; first line if all the words were used.
      (if words (setcar split words) (pop split))

      ;; Pad the line with spaces
      (setq line (s-pad-right w " " line))
      (push line lines))

    ;; Add a ... when there wasn't enough room for everything
    (when split (setcar lines (concat (substring (car lines) 0 -3) "...")))
    (reverse lines)))

(defun flow--box-with-border (text w &optional h)
  (append (list (concat "╭" (make-string (- w 2) ?─) "╮"))
          (--map (concat "│" it "│") (flow--box-default text (- w 2) (when h (- h 2))))
          (list (concat "╰" (make-string (- w 2) ?─) "╯"))))

;;; Generating arrows

(defun flow--update-arrows (&optional box)
  "Update the paths of the arrows in `flow-arrows`.
If BOX is non-nil, update arrows connected to or overlapping it."

  (dolist (a flow-arrows)
    (when (or (null box) (null (nth 1 a))
              (eq box (caar a)) (eq box (cdar a))
              (flow--box-overlaps-arrow box a))
      (setf (nth 1 a)
            (ignore-errors
              (flow--generate-arrow
               (caar a) (cdar a)))))))

(defun flow--box-overlaps-arrow (b a)
  "Return non-nil if box B is overlapping arrow A."
  (let* ((x1 (plist-get b :x)) (x2 (+ -1 x1 (plist-get b :w)))
         (y1 (plist-get b :y)) (y2 (+ -1 y1 (plist-get b :h)))
         (ps (nth 1 a))
         x3 x4 y3 y4)
    (while (and (listp ps) (>= (length ps) 2))
      (setq x3 (caar ps) y3 (cdar ps) x4 (caadr ps) y4 (cdadr ps))
      (if (if (= y3 y4)
              (and (>= y3 y1) (<= y3 y2)
                   (or (and (>= x3 x1) (<= x3 x2))
                       (and (>= x4 x1) (<= x4 x2))
                       (and (<= x1 (max x3 x4)) (>= x1 (min x3 x4)))))
            (and (>= x3 x1) (<= x3 x2)
                 (or (and (>= y3 y1) (<= y3 y2))
                     (and (>= y4 y1) (<= y4 y2))
                     (and (<= y1 (max y3 y4)) (>= y1 (min y3 y4))))))
          (setq ps t)
        (pop ps)))
    (eq ps t)))

(defun flow--generate-arrow (b1 b2)
  "Generate an arrow pointing from box B1 to box B2.

GRID is a list of bool vectors representing locations of boxes,
where t is a location in the buffer that is taken up by a box."

  (let* ((boxes flow-boxes) (arrows flow-arrows)
         (w (flow--max-width boxes)) (h (flow--max-height boxes))
         (x1 (plist-get b1 :x)) (y1 (plist-get b1 :y))
         (w1 (plist-get b1 :w)) (h1 (plist-get b1 :h))
         (x2 (plist-get b2 :x)) (y2 (plist-get b2 :y))
         (w2 (plist-get b2 :w)) (h2 (plist-get b2 :h))
         (grid (make-vector h nil))
         q1 q2 q3 final prev cur path n n2 x y nx ny nn dx dy)
    (setq myg grid)

    ;; Setup the vector
    (dotimes (i h) (aset grid i (make-vector w nil)))

    ;; Set boxes to t in the grid
    (dolist (b boxes)
      (let ((x (plist-get b :x)) (y (plist-get b :y))
            (w (plist-get b :w)) (h (plist-get b :h)))
        (dolist (i (number-sequence y (+ -1 y h)))
          (dolist (j (number-sequence x (+ -1 x w)))
            (aset (aref grid i) j 'box)))))

    ;; Set existing arrows to 'a in the grid
    (dolist (a arrows)
      ;; Skip the arrow that is being generated
      (unless (equal (car a) (cons b1 b2))
        (let ((ps (cadr a)) p1 p2)
          ;; Loop through each pair of points in the list
          (while (>= (length ps) 2)
            ;; Fill in the line between the first two points
            (setq p1 (pop ps) p2 (car ps))
            (dolist (x (number-sequence (min (car p1) (car p2)) (max (car p1) (car p2))))
              (dolist (y (number-sequence (min (cdr p1) (cdr p2)) (max (cdr p1) (cdr p2))))
                ;; Only set the box when it is equal to nil
                (unless (aref (aref grid y) x)
                  (aset (aref grid y) x 'a))))))))

    ;; Add extra padding around the end box to make sure there is room for the arrow
    (dolist (i (number-sequence (1- y2) (+ y2 h2)))
      (dolist (j (number-sequence (1- x2) (+ x2 w2)))
        (ignore-errors (aset (aref grid i) j 'box))))

    ;; Set centers of sides of ending boxes to 'end
    (dolist (q (list (cons (+ x2 (ash w2 -1)) (1- y2))
                     (cons (+ x2 (ash w2 -1)) (+ y2 h2))
                     (cons (1- x2) (+ y2 (ash h2 -1)))
                     (cons (+ x2 w2) (+ y2 (ash h2 -1)))))
      (ignore-errors (aset (aref grid (cdr q)) (car q) 'end)))

    (setq myg1 (make-vector (length myg) nil))
    (dotimes (i (length myg))
      (aset myg1 i (make-vector (length (aref myg i)) nil))
      (dotimes (j (length (aref myg i)))
        (aset (aref myg1 i) j (aref (aref myg i) j))))

    ;; Prepare the queue
    (setq q1 (list (cons 0 (cons (+ x1 (ash w1 -1)) y1))
                   (cons 0 (cons (+ x1 (ash w1 -1)) (+ -1 y1 h1)))
                   (cons 0 (cons x1 (+ y1 (ash h1 -1))))
                   (cons 0 (cons (+ -1 x1 w1) (+ y1 (ash h1 -1))))))

    (dolist (q q1) (aset (aref grid (cddr q)) (cadr q) 0))

    ;; Make sure x and y are in range

    ;; Generate a lattice of the distance of points from the center
    (while (and (null final) (or q1 q2 q3))
      (unless q1 (setq q1 q2 q2 q3 q3 nil))

      (setq cur (car q1))
      (setq n (car cur) x (cadr cur) y (cddr cur))

      ;; Loop through the possible directions from the current location
      (dolist (d '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
        (setq nn (1+ n) nx (+ x (car d)) ny (+ y (cdr d)))

        ;; Make sure the new location is inside of the grid
        (unless (or (< ny 0) (< nx 0) (>= ny (length grid))
                    (>= nx (length (aref grid 0))))

          ;; Figure out what to do based on the value at that location
          (setq prev (aref (aref grid ny) nx))
          (cond ((eq prev 'end)
                 (aset (aref grid ny) nx nn)
                 (setq final (cons nx ny)))
                ((eq prev 'a)
                 (aset (aref grid ny) nx (1+ nn))
                 (push (cons (1+ nn) (cons nx ny)) q3))
                ((null prev)
                 (aset (aref grid ny) nx nn)
                 (push (cons nn (cons nx ny)) q2)))))

      (pop q1))

    (unless final (error "No possible path found."))

    ;; Figure out the path back to the starting location
    (setq x (car final) y (cdr final)
          n (aref (aref grid y) x)
          path (list (cons x y)))

    (while (> n 0)
      ;; Figure out the direction necessary to get closer to the start
      (setq dx nil dy nil)
      (dolist (d '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
        (setq n2 (ignore-errors (aref (aref grid (+ y (cdr d))) (+ x (car d)))))
        (when (and (numberp n2) (< n2 n))
          (setq dx (car d) dy (cdr d) n n2)))
      (setq x (+ x dx) y (+ y dy))

      (unless (and dx dy)
        (error "Could not get back to beginning"))

      ;; Move in that direction until it isn't possible anymore
      (while (progn (setq n2 (ignore-errors (aref (aref grid (+ y dy)) (+ x dx))))
                    (and (> n 0) (numberp n2) (< n2 n)))
        (setq n n2 x (+ x dx) y (+ y dy)))
      (push (cons x y) path))

    path))

;;; User functions

(defun flow-move-all-boxes (dx dy)
  "Move all boxes by DX and DY."
  (when (not (= dx 0))
    (dolist (b flow-boxes)
      (plist-put b :x (+ (plist-get b :x) dx)))
    (dolist (a flow-arrows)
      (dolist (p (cadr a))
        (setcar p (+ (car p) dx)))))

  (when (not (= dy 0))
    (dolist (b flow-boxes)
      (plist-put b :y (+ (plist-get b :y) dy)))
    (dolist (a flow-arrows)
      (dolist (p (cadr a))
        (setcdr p (+ (cdr p) dy))))))

(defun flow-move-current-box (dx dy)
  "Move the box the cursor is in DX columns right and DY rows down."
  (when-let* ((x (current-column))
              (y (1- (line-number-at-pos)))
              (box (flow--box-at-point))
              (newx (+ dx (plist-get box :x)))
              (newy (+ dy (plist-get box :y))))

    (plist-put box :x newx)
    (plist-put box :y newy)

    (setq flow-boxes (cons box (delq box flow-boxes)))

    ;; If it went too far left or up, move all the boxes
    (when (< newx 0) (flow-move-all-boxes (- newx) 0))
    (when (< newy 0) (flow-move-all-boxes 0 (- newy)))

    (flow--update-arrows box)
    (flow-reload)
    (flow--goto-char
     (+ x dx (if (< newx 0) (- newx) 0))
     (+ y dy (if (< newy 0) (- newy) 0)))))

(defvar flow-default-width 15
  "Default width for new boxes.")

(defun flow-add-box (text width height)
  "Create a new box containing TEXT with dimensions WIDTH x HEIGHT."
  (interactive
   (let* ((text (read-string "Text: "))
          (width (read-number (format "Width (length=%s): " (length text))
                              (min (+ 2 (length text)) flow-default-width))))
     (list text width (read-number "Height: " (length (funcall flow-box-function text width))))))

  (let ((id (abs (random))))

    (push (list id 'box
                :x (current-column) :y (1- (line-number-at-pos))
                :w width :h height :text text :arrows nil
                :box-face (list :inherit nil) :arrow-face (list :inherit nil))
          flow-boxes)

    (flow--update-arrows (car flow-boxes))
    (flow-reload)))

(defun flow-edit-box (box text width height)
  "Edit the box under the cursor."
  (interactive
   (if-let* ((box (flow--box-at-point)))
       (list box
             (read-string "Text: " (plist-get box :text))
             (read-number "Width: " (plist-get box :w))
             (read-number "Height: " (plist-get box :h)))
     (error "No box at point.")))

  (plist-put box :text text)
  (plist-put box :w width)
  (plist-put box :h height)

  (flow--update-arrows (car flow-boxes))
  (flow-reload))

(defun flow-delete-box (box)
  "Delete the box under the cursor."
  (interactive
   (list (or (flow--box-at-point)
             (error "No box at point."))))

  (when (y-or-n-p (->> (plist-get box :text)
                       (replace-regexp-in-string "\n" "  ")
                       (s-truncate 40)
                       (format "Delete box \"%s\"?")))

    ;; Remove all arrows that involved the box
    (setq flow-arrows (--remove (or (eq box (caar it)) (eq box (cdar it)))
                                flow-arrows))

    ;; Remove the box from the list of boxes
    (setq flow-boxes (delq box flow-boxes))

    (flow-reload)))

(defun flow-avy-select-box (&optional boxes)
  "Prompt the user to select a box using avy.
If BOXES is non-nil, only select from the given list."

  (interactive)
  (let ((ps (--map (cons (+ 1 (plist-get it :x)
                            (* (plist-get it :y) (1+ flow-width)))
                         it)
                   (or boxes flow-boxes))))
    (alist-get (avy-process (mapcar 'car ps)) ps)))

(defun flow-add-arrow (b1 b2)
  "Create a new arow from box B1 to box B2."
  (interactive
   (let* ((b1 (or (flow--box-at-point)
                  (progn (message "Select starting box:")
                         (flow-avy-select-box))))
          (b2 (progn (message "Select target box:")
                     (flow-avy-select-box (remove b1 flow-boxes)))))
     (list b1 b2)))

  ;; Check if the arrow already exists
  (if (or (memq (car b2) (plist-get b1 :arrows))
          (memq (car b1) (plist-get b2 :arrows)))
      (message "There is already an arrow between these boxes.")

    (plist-put b1 :arrows (cons (car b2) (plist-get b1 :arrows)))
    (push (list (cons b1 b2) (ignore-errors (flow--generate-arrow b1 b2)))
          flow-arrows)
    (flow-reload)))

(defun flow-delete-arrow (b1 b2)
  "Delete the arrow from box B1 to box B2."
  (interactive
   (if-let* ((b1 (or (flow--box-at-point)
                     (progn (message "Select starting box:")
                            (flow-avy-select-box))))
             (arrows (remove nil (--map (assq it flow-boxes) (plist-get b1 :arrows))))
             (b2 (progn (message "Select target box:")
                        (flow-avy-select-box arrows))))
       (list b1 b2)
     (error "Starting box doesn't have any arrows coming from it.")))

  ;; Remove the selected arrow, and any nonexistant arrows
  (plist-put b1 :arrows
             (--filter (assq it flow-boxes)
                       (remove (car b2) (plist-get b1 :arrows))))
  (setq flow-arrows (delete (assoc (cons b1 b2) flow-arrows) flow-arrows))
  (flow-reload))

(qv/package dash)

;;; Motions/Operators
;;;; Operators
(defmacro qvk-defop (name custom &rest cmds)
  "Define an operator that acts using variables BEG and END.
Custom is a plist of strings (keys) and functions (motions) to be
specific motion keys for this operator only."
  (declare (indent 2))
  `(defun ,name (motion)
     (interactive
      (list (if mark-active 'ignore
              (apply 'qvk-read-motion ',custom))))
     (save-excursion
       (let ((start (point)))
         ;; Only read a motion if the mark isn't active
         (unless mark-active
           (push-mark)
           (call-interactively motion)
           (unless mark-active (activate-mark)))

         ;; Execute operator specific commands
         (unwind-protect (progn ,@cmds)
           (when mark-active (deactivate-mark)))))))

;;;; Motions
(qv/keys qvk-motion-map
  :sparse t)

(defmacro qvk-defmotion (name beg &rest end)
  "A simplified way to define a two sided motion."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     ,beg
     (set-mark-command nil)
     ,@end))

(defun qvk-read-motion (&rest custom)
  (let ((cmap (make-sparse-keymap)))
    (while (cdr custom)
      (define-key cmap (kbd (pop custom)) (pop custom)))
    (setq minor-mode-map-alist
          `((t . ,cmap) (t . ,qvk-motion-map)
            . ,minor-mode-map-alist))
    (unwind-protect (key-binding (read-key-sequence ""))
      (setq minor-mode-map-alist (cddr minor-mode-map-alist)))))

;;;; Define motions

(qvk-defmotion qvk-inside-line
  (beginning-of-line) (end-of-line))

(qvk-defmotion qvk-around-line
  (beginning-of-visual-line)
  (end-of-visible-line)
  (goto-char (min (point-max) (1+ (point)))))

(qvk-defmotion qvk-inside-word
  (progn (forward-char) (backward-word))
  (progn (forward-word)))

(qvk-defmotion qvk-around-word
  (progn (forward-char) (backward-word 2) (forward-word))
  (progn (forward-word 2) (backward-word)))

(qvk-defmotion qvk-around-sexp
  (if (in-string-p)
      (while (in-string-p) (search-backward-regexp "[\"']"))
    (let ((level 0)
          (d (save-excursion (beginning-of-defun) (point))))
      (while (and (>= level 0) (> (point) d))
        (search-backward-regexp "[])}[({]")
        (unless (in-string-p)
          (setq level (+ level (if (looking-at-p "[[({]") -1 1)))))))
  (forward-sexp))

(defun qvk-inside-sexp ()
  (interactive)
  (qvk-around-sexp)
  (set-mark (1+ (mark)))
  (backward-char))

(defun up-sexp ()
  (interactive)
  (if (in-string-p)
      (and (search-backward-regexp "[^\\][\"']" nil t)
           (forward-char))
    (let ((pos (point)))
      (while (ignore-errors (or (backward-sexp) (not (bobp)))))
      (if (bobp) (goto-char pos) (search-backward-regexp "[({[]" nil t)))))


;;; Foundation
;;;; Get a value from a keymap

(defun qvk-keymap-get (keymap key)
  "Return a list of things bound to KEY in MAP"
  (let ((binds (cdr keymap))
        b f)
    (while (and binds (null f))
      (setq b (pop binds)
            f (and (consp b)
                   (or (and (eq (car b) key) (cdr b))
                       (and (eq (car b) 'keymap)
                            (qvk-keymap-get b key))))))
    f))


;;;; Defining keymodes

(defvar qvk-keymode-alist nil
  "Alist mapping keymode names to a plist of properties.
Properties can include:
:maps - A list of names of keymaps
:cursor - Value of `cursor-type` to be used in the keymode
:hooks - A list of functions to call when entering the keymode")

(defvar-local qvk-keymode 'normal
  "The name of the current keymode.")

(defmacro qvk-defkeymode (keymode maps &rest args)
  (declare (indent 2))
  `(progn (setq qvk-keymode-alist
                (cons '(,keymode :maps ,maps . ,args)
                      (assq-delete-all ',keymode qvk-keymode-alist)))
          (defun ,(intern (format "qvk-%s-keymode" keymode)) ()
            (interactive)
            (qvk-keymode ',keymode))))


;;;; Switching to keymodes

(defun qvk-keymode (keymode)
  (let ((next (or (alist-get keymode qvk-keymode-alist)
                  (error "Could not find keymode `%s`." keymode))))

    (setq-local qvk-keymode keymode)
    
    ;; Set the first element of the global keymap to a function symbol
    ;; instead of to a keymap, because if it is set to a keymap, then
    ;; new global definitions get stored in it
    (fset 'qvk-global-keymode `(keymap . ,(mapcar 'symbol-value (plist-get next :maps))))
    (use-global-map `(keymap qvk-global-keymode ,global-map))

    ;; Set major mode keybindings
    (setq minor-mode-overriding-map-alist nil)
    (when-let ((bind (qvk-keymap-get (current-local-map) keymode)))
      (push (cons t bind) minor-mode-overriding-map-alist))

    ;; Set minor mode keybindings
    (dolist (e minor-mode-map-alist)
      (when-let ((bind (qvk-keymap-get (cdr e) keymode)))
        (push (cons (car e) (list 'keymap bind (cdr e)))
              minor-mode-overriding-map-alist)))

    ;; Run hooks
    (run-hook-with-args (plist-get next :hooks))

    ;; Set the custom cursor shape
    (setq cursor-type (or (plist-get next :cursor) t))))


;;;; Update keymode

(defvar qvk-last-buffer (current-buffer)
  "The buffer the cursor was in after the last command.")

(add-hook 'post-command-hook 'qvk-update-keymode)
(add-hook 'minibuffer-setup-hook 'qvk-insert-keymode)

(defun qvk-update-keymode ()
  "Update the keymode."

  ;; Update the keymode if the region is active/inactive
  ;; If you use the mark hooks, then you will get switched to normal mode
  ;; whenever running a function that temporary activates the mark
  (run-with-timer
   0 nil
   (lambda ()
     (when (and mark-active (not (eq qvk-keymode 'visual)))
       (qvk-keymode 'visual))
     (when (and (not mark-active) (eq qvk-keymode 'visual))
       (qvk-keymode 'normal))))

  ;; Update the keymode if switching buffers
  (when (not (eq (current-buffer) qvk-last-buffer))
    (setq qvk-last-buffer (current-buffer))
    (qvk-keymode qvk-keymode)))


;;; Keymaps
;;;; Normal map
(qv/keys qvk-normal-map
  :full t
  :prefix ("M-" qvk-M-normal-map)
  "h" (@ qvk-left (qvk-stay-on-line (backward-char)))
  "l" (@ qvk-right (qvk-stay-on-line (forward-char)))
  "k" (@ qvk-up (previous-line))
  "j" (@ qvk-down (next-line))
  "H" (@ qvk-left4 (qvk-stay-on-line (backward-char 4)))
  "L" (@ qvk-right4 (qvk-stay-on-line (forward-char 4)))
  "K" (@ qvk-up4 (previous-line 4))
  "J" (@ qvk-down4 (next-line 4))
  "f h" (@ qvk-big-left (qvk-stay-on-line (backward-char 40)))
  "f l" (@ qvk-big-right (qvk-stay-on-line (forward-char 40)))
  "f k" (@ qvk-big-up (previous-line 20))
  "f j" (@ qvk-big-down (next-line 20))
  "f m" (@ qvk-middle-of-line (beginning-of-line) (forward-char (/ (- (line-end-position) (point)) 2)))
  "g j" end-of-buffer
  "g k" beginning-of-buffer
  "g h" beginning-of-visual-line
  "g l" move-end-of-line
  "g H" beginning-of-line-text
  "g L" end-of-line

  "`" (@ qvk-run-last-command
         (setq this-command last-command)
         (call-interactively last-command))
  "e" (qvk-stay-on-line (forward-word))
  "w" (qvk-stay-on-line (forward-to-word 1))
  "b" (qvk-stay-on-line (backward-word))
  "W" up-sexp
  "E" forward-sexp
  "B" backward-sexp
  "0" beginning-of-line
  "^" beginning-of-line-text
  "$" end-of-line
  "i" (@ qvk-insert-before-char (qvk-insert-keymode))
  "a" (@ qvk-insert-after-char (forward-char) (qvk-insert-keymode))
  "I" (@ qvk-insert-beginning-of-line
         (beginning-of-line-text) (qvk-insert-keymode))
  "A" (@ qvk-insert-end (end-of-line) (qvk-insert-keymode))
  "o" (@ qvk-open-below (end-of-visible-line) (newline) (qvk-insert-keymode))
  "O" (@ qvk-open-above (beginning-of-line) (newline) (forward-line -1)
                        (qvk-insert-keymode))
  "r" qvk-replace-char
  "R" qvk-replace-mode
  "v" set-mark-command
  "V" qvk-visual-line
  "g v" (activate-mark)
  "g p" pop-to-mark-command
  "d" ,(qvk-defop qvk-delete ("d" qvk-inside-line)
         (delete-region (mark) (point)))
  "y" ,(qvk-defop qvk-copy ("y" qvk-inside-line)
         (copy-region-as-kill (mark) (point)))
  "s" ,(qvk-defop qvk-kill ("s" qvk-inside-line)
         (kill-region (mark) (point)))
  "c" ,(qvk-defop qvk-change ("c" qvk-inside-line)
         (delete-region (mark) (point))
         (qvk-insert-keymode))
  "Y" (@ qvk-copy-line (qvk-copy 'qvk-around-line))
  "S" (@ qvk-kill-line (qvk-kill 'qvk-around-line))
  "D" (@ qvk-delete-line (qvk-delete 'qvk-around-line))
  "C" (@ qvk-change-line (qvk-change 'qvk-inside-line))
  "x" (@ qvk-delete-forward-char (qvk-delete 'forward-char))
  "z" (@ qvk-delete-backward-char (qvk-delete 'backward-char))
  "X" (@ qvk-delete-forward-word (qvk-delete 'forward-word))
  "Z" (@ qvk-delete-backward-word (qvk-delete 'backward-word))
  "p" (@ qvk-paste-after
         (let ((start (point)))
           (qvk-open-below) (qvk-normal-keymode) (yank)
           (ignore-errors (delete-region (point) (search-backward-regexp "\n+\\=" start)))))
  "P" yank
  "u" undo
  "/" nonincremental-search-forward
  "?" nonincremental-search-backward
  "g /" nonincremental-re-search-forward
  "g ?" nonincremental-re-search-backward
  "t" qvk-forward-to-letter
  "T" qvk-backward-to-letter
  "f /" consult-line
  "f ?" qv/grep
  "n" nonincremental-repeat-search-forward
  "N" nonincremental-repeat-search-backward
  "m" point-to-register
  "`" jump-to-register
  "~" (@ qvk-toggle-case (if (memq (aref (buffer-substring (point) (1+ (point))) 0) (number-sequence ?A ?Z))
                             (downcase-region (point) (1+ (point))) (upcase-region (point) (1+ (point))))
                         (forward-char))
  "g u" qvk-downcase
  "g U" qvk-upcase
  "g =" (@ qvk-indent-buffer (indent-region (point-min) (point-max)))
  "]" scroll-up
  "[" scroll-down
  "}" scroll-left
  "{" scroll-right
  ;; Personal Preferences
  "M-q" (@ qv/keyboard-quit
           (if (active-minibuffer-window)
               (with-selected-window (active-minibuffer-window)
                 (keyboard-escape-quit))
             (eval (list (key-binding (kbd "C-g"))))))
  "g f" fill-paragraph
  "M" (@ qvk-merge-lines (next-line) (join-line))

  ";" (@ qvk-comment-line (save-excursion (comment-line 1)))
  ":" (@ qvk-comment-expr (comment-region (point) (save-excursion (forward-sexp) (point))))
  "g :" (@ qvk-uncomment-block
           (save-excursion
             (beginning-of-line)
             (let ((start (point)))
               (while (looking-at (format "[ \t]*\\(\n\\|%s\\)" (regexp-quote comment-start)))
                 (forward-line 1))
               (uncomment-region start (line-end-position)))))

  "." (@ qvk-shift-right
         (save-excursion
           (beginning-of-line)
           (insert (make-string tab-width ?\s)))
         (when (bolp) (forward-char tab-width)))
  "," (@ qvk-shift-left
         (save-excursion
           (beginning-of-line)
           (when (looking-at (make-string tab-width ?\s))
             (delete-forward-char tab-width))))

  "g w" nil
  "g w h" windmove-left
  "g w l" windmove-right
  "g w k" windmove-up
  "g w j" windmove-down
  "g w H" (split-window nil nil 'right)
  "g w L" (split-window nil nil 'left)
  "g w K" (split-window nil nil 'below)
  "g w J" (split-window nil nil 'above)

  "g W h" (window-resize nil -4 t)
  "g W l" (window-resize nil +4 t)
  "g W j" (window-resize nil -1 nil)
  "g W k" (window-resize nil +1 nil)
  "g W H" (window-resize nil -16 t)
  "g W L" (window-resize nil +16 t)
  "g W J" (window-resize nil -5 nil)
  "g W K" (window-resize nil +5 nil))

;;;; Insert map
(qv/keys qvk-insert-map
  :sparse t
  "M-q" qvk-normal-keymode

  "C-M-l" end-of-line
  "C-M-h" beginning-of-line
  "C-M-j" end-of-buffer
  "C-M-k" beginning-of-buffer

  "M-(" ((insert "()") (backward-char))
  "M-{" ((insert "{}") (backward-char))
  "M-[" ((insert "[]") (backward-char))
  "M-'" ((insert "''") (backward-char))
  "M-`" ((insert "``") (backward-char))
  "M-\"" ((insert "\"\"") (backward-char))
  "M-*" ((insert "**") (backward-char))
  "M-/" ((insert "//") (backward-char)))

;;;; Motion Map
(qv/keys qvk-motion-map
  :sparse t
  "x" end-of-line
  "z" beginning-of-line
  "v" qvk-inside-word
  "a s" qvk-around-sexp
  "i s" qvk-inside-sexp
  "a w" qvk-around-word
  "i w" qvk-inside-word
  "a l" qvk-around-line
  "i l" qvk-inside-line)

;;;; Visual map
(qv/keys qvk-visual-map
  :sparse t
  :parent qvk-motion-map
  "M-q" (deactivate-mark)
  "o" exchange-point-and-mark
  "I" qvk-macro-on-region

  ":" comment-region
  "g :" uncomment-region

  ">" (qvk-run-on-lines qvk-shift-right (mark) (point))
  "<" (qvk-run-on-lines qvk-shift-left (mark) (point))

  "D" delete-rectangle
  "S" kill-rectangle
  "Y" copy-rectangle-as-kill)

;;;; Action map
(qv/keys qvk-action-map
  :sparse t
  :prefix ("M-" qvk-M-action-map)
  "f" find-file
  "e" eval-expression)

;;; Keymodes

(qvk-defkeymode normal
    (qvk-normal-map qvk-M-action-map)
  :cursor box)

(qvk-defkeymode insert
    (qvk-insert-map qvk-M-normal-map)
  :cursor (bar . 1))

(qvk-defkeymode visual
    (qvk-visual-map qvk-normal-map qvk-M-action-map)
  :cursor (bar . 3))

(qvk-keymode qvk-keymode)

;;; Extra
;;;; Replacing
(defun qvk-replace-char (char &optional count)
  (interactive
   (list (read-char "Replace: ")
         (if (numberp current-prefix-arg)
             current-prefix-arg nil)))

  (when (characterp char)
    (delete-char (or count 1))
    (insert (make-string (or count 1) char))))

(defun qvk-replace-mode ()
  (interactive)
  (let ((cursor-type 'hbar)
        char)
    (while (characterp (setq char (read-char "Replace: ")))
      (delete-char 1)
      (if (eq char 13) (newline)
        (insert (string char))))))

;;;; Linewise Visual Mode
(setq qvk-visual-line-mode t)

(defun qvk--visual-line-update ()
  (rectangle-mark-mode 0)
  (let ((point-after (> (point) (mark))))
    (remove-overlays (point-min) (point-max) 'qvk-visual-line t)
    (unless point-after (exchange-point-and-mark))
    (end-of-visual-line)
    (exchange-point-and-mark)
    (beginning-of-visual-line)
    (when point-after (exchange-point-and-mark))
    (let ((o (make-overlay (max (point) (mark)) (1+ (max (point) (mark))))))
      (overlay-put o 'face 'region)
      (overlay-put o 'qvk-visual-line t))))

(defun qvk--visual-line-disable ()
  (setq qvk-visual-line-mode nil)
  (remove-overlays (point-min) (point-max) 'qvk-visual-line t)
  (remove-hook 'post-command-hook 'qvk--visual-line-update t)
  (remove-hook 'deactivate-mark-hook 'qvk--visual-line-disable t))

(defun qvk-visual-line ()
  (interactive)
  (setq qvk-visual-line-mode t)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (add-hook 'post-command-hook 'qvk--visual-line-update nil t)
  (add-hook 'deactivate-mark-hook 'qvk--visual-line-disable nil t))

;;;; Run macro on region
(defun qvk-macro-on-region ()
  (interactive)
  (let ((p (point)) (m (mark)))
    (deactivate-mark)

    (kmacro-start-macro nil)
    (recursive-edit)
    (kmacro-end-macro nil)

    (goto-char p)
    ;; Don't include the current line of the point
    (if (> p m) (beginning-of-line) (forward-line))

    (apply-macro-to-region-lines
     (min m (point)) (max m (point)))))

;;;; Special Mode Map
(qv/keys special-mode-map
  :sparse t
  "r" revert-buffer
  "q" (@ qvk-kill-buffer
         (let ((buf (current-buffer)))
           (bury-buffer) (kill-buffer buf))))

;;;; Jump Line/Column
(setq qvk-jump-chars "asdfghjkl;")

(defun qvk-jump-column ()
  (interactive)
  (let* ((chars qvk-jump-chars)
         (c (read-char "Line position: "))
         (dist (- (line-end-position) (line-beginning-position)))
         (idx (s-index-of (string c) chars))
         (col (round (* dist (/ (float idx) (float (1- (length chars))))))))
    (beginning-of-line)
    (forward-char col)))


(defun qvk-jump-line ()
  (interactive)
  (let* ((chars qvk-jump-chars)
         (c (read-char "Line position: "))
         (dist (window-height))
         (idx (s-index-of (string c) chars))
         (line (round (* dist (/ (float idx) (float (1- (length chars))))))))
    (move-to-window-line 0)
    (next-line line)))

;;;; Jump to letter
(defun qvk-forward-to-letter (c)
  (interactive (list (read-char "Forward to letter: ")))
  (goto-char (1- (search-forward (string c) (line-end-position)))))

(defun qvk-backward-to-letter (c)
  (interactive (list (read-char "Backward to letter: ")))
  (goto-char (1+ (search-backward (string c) (line-beginning-position)))))

;;;; Keep Column
(qv/hook post-command-hook qvk-cursor-column
  (if (not truncate-lines) (setq goal-column nil)
    (when (boundp 'qvk-last-pos)
      (when (or (not (eolp))
                (and (eq (car qvk-last-pos) (line-number-at-pos))
                     (not (eq (cdr qvk-last-pos) (current-column)))))
        (setq-local goal-column (current-column))))
    (when track-eol (end-of-line))

    (set-face-attribute 'cursor nil :background
                        (if track-eol (qv/color blue) "white"))

    (setq-local qvk-last-pos (cons (line-number-at-pos) (current-column)))))

;;;; Stay on line
(defmacro qvk-stay-on-line (&rest exprs)
  "Advises around a function to make sure that it stays on the same line"
  `(let ((pos (point))
         (line-beg (line-beginning-position))
         (line-end (line-end-position)))
     ,@exprs
     (goto-char (max line-beg (min line-end (point))))))

;;;; Run for each line
(defmacro qvk-run-on-lines (func beg end)
  `(let ((b ,beg) (e ,end)
         (m (make-marker)))
     (set-marker m (max b e))
     (save-excursion
       (goto-char (min b e))
       (beginning-of-line)
       (while (and (not (eobp))
                   (<= (point) (marker-position m)))
         (funcall ',func)
         (forward-line 1)))))

;;;; Customize newline function
(advice-add 'newline :around 'qvk-newline-advice)

(defun qvk-newline-advice (newline &rest args)
  (when (and (not (bobp)) (not (eobp))
             (member (buffer-substring (1- (point)) (1+ (point)))
                     '("()" "[]" "{}" "''" "``" "\"\"")))
    (save-excursion (apply newline args)))

  (apply newline args))

;;;; Find character
(defun qvk-find-letter (char &optional back to)
  (when to (if back (backward-char) (forward-char)))
  (funcall (if back 'search-backward 'search-forward)
           (string char)
           (if back (line-beginning-position) (line-end-position)))
  (when to (if back (forward-char) (backward-char))))

(dolist (back '(nil t))
  (dolist (to '(nil t))
    (let ((bw (if back "Backward" "Forward")) (tw (if to "To" "Find")))
      (eval `(defun ,(intern (downcase (format "qvk-%s-%s-letter" bw tw))) (char)
               (interactive (list (read-char ,(format "%s %s: " bw tw))))
               (qvk-find-letter char ,back ,to))))))


;;; Global Map
(qv/keys *
  "C-x C-h" (if hs-minor-mode
                (progn (outline-minor-mode) (message "Outline mode"))
              (hs-minor-mode) (message "Hideshow mode")))

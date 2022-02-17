(require 'dash)

;;; Convenient Functions
;;;; Keybinding Macro
(defvar modeal-keybinding-abbrevs
  '(* (global-set-key)
      ~ (local-set-key)
      exwm (exwm-input-set-key))
  "Plist of abbreviations mapped to forms that will bind a key when 2 arguments are added.")

(defmacro modeal-key (map key binding)
  (declare (indent 2))
  (cond
   ((eq key :parent) `(set-keymap-parent ,map ,(if (keymapp binding) (list 'quote binding) binding)))
   ((eq key :sparse) (if (and (boundp map) (keymapp (eval map)))
                         `(setcdr ,map nil) `(setq ,map (make-sparse-keymap))))
   ((eq key :full) (if (and (boundp map) (keymapp (eval map)))
                       `(setcdr ,map (cdr (make-keymap))) `(setq ,map (make-sparse-keymap))))
   (t (setq key (cond ((stringp key) (kbd key)) ((numberp key) (vector key)) (t key)))
      `(,@(or (plist-get modeal-keybinding-abbrevs map) (list 'define-key map)) ,key
        ,(cond ((and (listp binding) (eq (car binding) '\,)) (cadr binding))
               ((or (atom binding) (functionp binding) (keymapp binding)) (list 'quote binding))
               ((memq (car binding) '(defun defmacro lambda)) binding)
               ((eq (car binding) '@) `(defun ,(cadr binding) () (interactive) . ,(cddr binding)))
               ((listp (car binding)) `(lambda () (interactive) . ,binding))
               (t (eval `(lambda () (interactive) ,binding))))))))

(defmacro modeal-keys (map &rest forms)
  (declare (indent 1))
  (cons 'progn (mapcar (lambda (n) (macroexpand `(modeal-key ,map ,(nth n forms) ,(nth (1+ n) forms))))
                       (number-sequence 0 (1- (length forms)) 2))))

;;;; Add a key prefix
(defun modeal-add-keymap-prefix (keymap prefix &optional all-levels)
  "Return a new keymap containing each of the keys in KEYMAP
modified by PREFIX.

PREFIX should be a string to add before the string representation of
a keymap to modify it, for example \"M-\" would add the meta modifier
to a key sequence."

  (when (symbolp keymap) (setq keymap (symbol-value keymap)))
  (unless (keymapp keymap) (error "Not a valid keymap: %s" keymap))
  ;; Make the new keymap the same type as the input keymap
  (let ((new-map (if (char-table-p (cadr keymap))
                     (make-keymap)
                   (make-sparse-keymap))))
    ;; If `PREFIX` doesn't end in a space or dash, add a space
    (setq prefix (replace-regexp-in-string "[^ -]$" "\\& " prefix))
    ;; Add each modified binding to the new keymap
    (map-keymap
     (lambda (key binding)
       (define-key new-map
         (if (and (vectorp key) (eq (aref key 0) 'remap)) key
           (kbd (concat prefix (key-description
                                (if (vectorp key)
                                    key (vector key))))))
         (if (and all-levels (keymapp binding))
             (modeal--add-keymap-prefix binding prefix t)
           binding)))
     keymap)
    new-map))

;;;; Defining Operators
(defmacro defoperator (name extra-motions &rest body)
  "Define a command called NAME, with one argument, `(beg . end)`.

If a visual selection is active when the function is called, `beg`
and `end` will be the point and mark. Otherwise, the user will be
prompted for a motion. If this motion returns a cons cell, the car
and cdr will be `beg` and `end`. Otherwise, the position of the
point before and after running the motion will be `beg` and `end`.

EXTRA-MOTIONS should either be a keymap, a cons cell of the form
`(KEY . BINDING)`, or a symbol holding either. It can also be a
list of any number of said elements. These keys and keymaps will
be used as additional options for motions.

BODY should be made up of lisp expressions which use the values of
`beg` and `end` to perform an operation on the text between them.

Note that `beg` will sometimes come after `end`; this is intended.
They represent the beginning and end of the motion, not the
beginning and end of the text in respect to the document."

  (declare (indent 2))

  ;; Macro expansion: the operator function definition
  `(defun ,name (motion)
     (interactive
      (list
       (save-excursion
         (if mark-active
             (if modeal-visual-line-mode
                 (cons (min (point) (mark)) (max (point) (mark)))
               (cons (mark) (point)))

           ;; Compose EXTRA-MOTIONS into a single keymap
           (let ((motion-list ',extra-motions)
                 (extra-map (make-sparse-keymap)))
             (when (or (keymapp motion-list) (not (listp motion-list))) 
               (setq motion-list (list motion-list)))
             (dolist (x motion-list)
               (when (symbolp x) (setq x (eval x)))
               (cond ((keymapp x)
                      (setq extra-map (make-composed-keymap (list extra-map x))))
                     ((consp x) (define-key extra-map
                                  (if (stringp (car x)) (kbd (car x)) (car x))
                                  (cdr x)))))
             ;; Read and execute the operator
             (let* ((prev-point (point))
                    (prev-buffer (current-buffer))
                    (minor-mode-map-alist
                     (cons (cons t extra-map)
                           minor-mode-map-alist))
                    (motion (key-binding (read-key-sequence "Motion: ")))
                    ;; Run the motion
                    (output (when motion (call-interactively motion))))
               ;; Interperet the output of the motion
               (if (eq prev-buffer (current-buffer))
                   (cond ((consp output) (cons (max (point-min) (car output))
                                               (min (point-max) (cdr output))))
                         ((numberp output) (cons prev-point output))
                         (t (cons prev-point (point))))
                 (user-error "Invalid motion `%s`, unable to run operator `%s`" motion ',name))))))))
     (unless (consp motion) (error "`MOTION` not a valid cons: `%s`" motion))
     (let ((beg (car motion))
           (end (cdr motion)))
       (unless (numberp beg) (error "`BEG` not a valid number: `%s`" beg))
       (unless (numberp end) (error "`END` not a valid number: `%s`" end))

       ;; Execute the operator specific code
       ,(cons 'progn body))))

;;;; Defining Motions
(defmacro defmotion (name &rest body)
  "Define a motion with the name NAME.

If you intend to define a motion that starts at the current point,
you do not need this function; operators will interperet any
function which moves the point as a motion. For example, \"around
sexp\" would use this function but \"forward sexp\" would not.

BODY should be made up of lisp expressions which return a cons cell
with the car as the beginning of the motion, and the cdr as the end
of the motion. The rest of the logic will be handled automatically."

  (declare (indent 1))

  `(defun ,name ()
     (interactive)
     (let ((motion ,(cons 'progn body)))
       (when mark-active
         (push-mark (car motion))
         (goto-char (cdr motion)))
       motion)))

;;; Extra Editing Commands
;;;; Motions
(defmotion modeal-inside-line
  (cons (line-beginning-position)
        (line-end-position)))

(defmotion modeal-around-line
  (let ((col (current-column))
        (line (line-number-at-pos)))
    (prog1 (cons (progn (beginning-of-line) (point))
                 (or (ignore-errors (end-of-line) (next-line 1) (line-beginning-position))
                     (point-max)))
      (if (< (line-number-at-pos (point-max)) line)
          (end-of-buffer)
        (goto-line line)
        (goto-char (min (+ (point) col) (line-end-position)))))))

(defmotion modeal-inside-word
  (save-excursion
    (cons (progn (forward-char) (backward-word) (point))
          (progn (forward-word) (point)))))

(defmotion modeal-around-word
  (save-excursion
    (cons (progn (forward-char) (backward-word 2) (forward-word) (point))
          (progn (forward-word 2) (backward-word) (point)))))

;;;; Operators
(defoperator modeal-copy
    (modeal-motion-map
     ("y" . modeal-inside-line))
  (kill-ring-save beg end))

(defoperator modeal-kill
    (modeal-motion-map
     ("s" . modeal-inside-line))
  (kill-region beg end))

(defoperator modeal-delete
    (modeal-motion-map
     ("d" . modeal-inside-line))
  (delete-region beg end))

(defoperator modeal-change
    (modeal-motion-map
     ("c" . modeal-inside-line))
  (delete-region beg end)
  (insert-keymode))

;;;; Newlines
(defvar-local modeal-newline-function 'newline-and-indent
  "Function to use for opening new lines.")

(defun modeal-newline ()
  (interactive)
  (funcall modeal-newline-function))

;;;; Entering Insert Mode
(defun modeal-insert-before-char ()
  (interactive)
  (insert-keymode))

(defun modeal-insert-after-char ()
  (interactive)
  (forward-char)
  (insert-keymode))

(defun modeal-insert-before-line ()
  (interactive)
  (beginning-of-line)
  (insert-keymode))

(defun modeal-insert-after-line ()
  (interactive)
  (end-of-line)
  (insert-keymode))

(defun modeal-open-above ()
  (interactive)
  (goto-char (1- (line-beginning-position)))
  (modeal-newline)
  (insert-keymode))

(defun modeal-open-below ()
  (interactive)
  (end-of-line)
  (if (ignore-errors (next-line) t)
      (goto-char (1- (line-beginning-position)))
    (end-of-buffer))
  (modeal-newline)
  (insert-keymode))

;;;; Jumping to characters
(defun modeal-forward-find-letter (char)
  (interactive "cForward find letter")
  (forward-char 1)
  (search-forward (string char) (line-end-position) t)
  (backward-char 1)
  (1+ (point)))

(defun modeal-forward-to-letter (char)
  (interactive "cForward to letter")
  (forward-char 2)
  (search-forward (string char) (line-end-position) t)
  (backward-char 2)
  (1+ (point)))

(defun modeal-backward-find-letter (char)
  (interactive "cBackward find letter:")
  (search-backward (string char) (line-beginning-position) t))

(defun modeal-backward-to-letter (char)
  (interactive "cBackward to letter: ")
  (search-backward (string char) (line-beginning-position) t)
  (forward-char 1))

;;;; Big Words
(defun modeal-forward-big-word ()
  (interactive)
  (search-forward-regexp "[ \t\n]")
  (search-forward-regexp "[^ \t\n]")
  (goto-char (1- (point))))

(defun modeal-end-big-word ()
  (interactive)
  (forward-char 1)
  (search-forward-regexp "[^ \t\n]")
  (search-forward-regexp "[ \t\n]")
  (goto-char (1- (point))))

(defun modeal-back-big-word ()
  (interactive)
  (search-backward-regexp "[^ \t\n]")
  (search-backward-regexp "[ \t\n]")
  (forward-char 1))

;;;; Pasting
(defun modeal-paste-after (&optional string)
  (interactive)
  (unless string (setq string (car kill-ring)))
  (if (string-match "\n" string)
      (progn (end-of-line)
             (->> string
                  (replace-regexp-in-string "\\`\n\\|\n\\'" "")
                  (format "\n%s")
                  (insert)))
    (forward-char 1) (insert string)))

(defun modeal-paste-before (&optional string)
  (interactive)
  (unless string (setq string (car kill-ring)))
  (if (string-match "\n" string)
      (progn (beginning-of-line)
             (->> string
                  (replace-regexp-in-string "\\`\n\\|\n\\'" "")
                  (format "%s\n")
                  (insert)))
    (insert string)))

;;;; Replacing
(defun modeal-replace-char (&optional count)
  (interactive
   (list (if (numberp current-prefix-arg)
             current-prefix-arg nil)))
  (let ((char (read-char "Replace: ")))
    (delete-char (or count 1))
    (insert (make-string (or count 1) char))))

(defun modeal-replace-mode ()
  (interactive)
  (let ((char (read-char "Replace: ")))
    (while (and (>= char 32) (<= char 126))
      (delete-char 1)
      (insert (string char))
      (setq char (read-char "Replace: ")))))

;;;; Track Column
(defvar-local just-tracked nil
  "Whether or not the last command involved tracking the cursor")
(defvar-local tracked-column 0
  "The column of the cursor to adjust")
(defvar-local tracked-point 0
  "The last recorded point position in a buffer")

(defun track-column-advice (&rest args)
  (setq just-tracked t)
  (let ((line-start (beginning-of-visual-line)))
    (while (and (< (car (window-text-pixel-size nil line-start (point)))
                   tracked-column)
                (not (eolp)))
      (forward-char 1))))

(defun track-column-postcmd ()
  (if just-tracked
      (setq just-tracked nil)
    (unless (eq (point) tracked-point)
      (setq tracked-column
            (car (window-text-pixel-size
                  nil (save-excursion (beginning-of-visual-line)) (point))))))
  (setq tracked-point (point)))

(add-hook 'post-command-hook 'track-column-postcmd)

(advice-add 'next-line :after 'track-column-advice)
(advice-add 'previous-line :after 'track-column-advice)

;;;; Linewise Visual Mode
(setq modeal-visual-line-mode t)

(defun modeal--visual-line-update ()
  (rectangle-mark-mode 0)
  (let ((point-after (> (point) (mark))))
    (remove-overlays (point-min) (point-max) 'modeal-visual-line t)
    (unless point-after (exchange-point-and-mark))
    (end-of-line)
    (exchange-point-and-mark)
    (beginning-of-line)
    (when point-after (exchange-point-and-mark))
    (let ((o (make-overlay (max (point) (mark)) (1+ (max (point) (mark))))))
      (overlay-put o 'face 'region)
      (overlay-put o 'modeal-visual-line t))))

(defun modeal--visual-line-disable ()
  (setq modeal-visual-line-mode nil)
  (remove-overlays (point-min) (point-max) 'modeal-visual-line t)
  (remove-hook 'post-command-hook 'modeal--visual-line-update t)
  (remove-hook 'deactivate-mark-hook 'modeal--visual-line-disable t))

(defun modeal-visual-line ()
  (interactive)
  (setq modeal-visual-line-mode t)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (add-hook 'post-command-hook 'modeal--visual-line-update nil t)
  (add-hook 'deactivate-mark-hook 'modeal--visual-line-disable nil t))

;;;; Adjusting Case
(defun modeal-toggle-case ()
  (interactive)
  (let ((case-fold-search nil))
    (if (string-match-p "[a-z]" (buffer-substring (point) (1+ (point))))
        (upcase-char 1) (downcase-region (point) (1+ (point))))
    (forward-char)))

(defoperator modeal-upcase
    (modeal-motion-map)
  (upcase-region (car motion) (cdr motion)))

(defoperator modeal-downcase 
    (modeal-motion-map)
  (downcase-region (car motion) (cdr motion)))

;;;; Stay on the Same Line
(defun modeal-stay-on-line-advice (func &rest args)
  "Advises around a function to make sure that it stays on the same line"
  (let ((pos (point))
        (line-beg (line-beginning-position))
        (line-end (line-end-position)))
    (apply func args)
    (goto-char (max line-beg (min line-end (point))))))

(advice-add 'forward-char :around 'modeal-stay-on-line-advice)
(advice-add 'backward-char :around 'modeal-stay-on-line-advice)
(advice-add 'forward-word :around 'modeal-stay-on-line-advice)
(advice-add 'backward-word :around 'modeal-stay-on-line-advice)
(advice-add 'forward-to-word :around 'modeal-stay-on-line-advice)
(advice-add 'backward-to-word :around 'modeal-stay-on-line-advice)

;;; Keymaps
;;;; Normal keymap
(modeal-keys modeal-normal-map
  :sparse t
  "h" backward-char
  "j" next-line
  "k" previous-line
  "l" forward-char
  "e" forward-word
  "w" forward-to-word
  "b" backward-word
  "W" (@ up-sexp
         (if (in-string-p)
             (and (search-backward-regexp "[^\\]\"" nil t)
                  (forward-char))
           (let ((pos (point)))
             (while (ignore-errors (or (backward-sexp) (not (bobp)))))
             (if (bobp) (goto-char pos) (search-backward "(" nil t)))))
  "E" forward-sexp
  "B" backward-sexp
  "0" beginning-of-line
  "^" beginning-of-line-text
  "$" end-of-line
  "i" modeal-insert-before-char
  "a" modeal-insert-after-char
  "I" modeal-insert-before-line
  "A" modeal-insert-after-line
  "o" modeal-open-below
  "O" modeal-open-above
  "r" modeal-replace-char
  "R" modeal-replace-mode
  "v" (@ modeal-visual (set-mark-command nil) (rectangle-mark-mode 0))
  "g v" (@ modeal-last-selection (activate-mark))
  "V" modeal-visual-line
  "C-v" rectangle-mark-mode
  "f" modeal-forward-find-letter
  "F" modeal-backward-find-letter
  "t" modeal-forward-to-letter
  "T" modeal-backward-to-letter
  "y" modeal-copy
  "s" modeal-kill
  "d" modeal-delete
  "c" modeal-change
  "Y" (@ modeal-copy-line (modeal-copy (modeal-around-line)))
  "S" (@ modeal-kill-line (modeal-kill (modeal-around-line)))
  "D" (@ modeal-delete-line (modeal-delete (modeal-around-line)))
  "C" (@ modeal-change-line (modeal-change (modeal-around-line)))
  "x" (@ modeal-delete-forward-char (modeal-delete (cons (point) (1+ (point)))))
  "z" (@ modeal-delete-backward-char (modeal-delete (cons (point) (1- (point)))))
  "X" (@ modeal-delete-forward-word (modeal-delete (cons (point) (progn (forward-word) (point)))))
  "Z" (@ modeal-delete-backward-word (modeal-delete (cons (point) (progn (backward-word) (point)))))
  "p" modeal-paste-after
  "P" modeal-paste-before
  "u" undo
  "/" nonincremental-search-forward
  "?" nonincremental-search-backward
  "g /" nonincremental-re-search-forward
  "g ?" nonincremental-re-search-backward
  "n" nonincremental-repeat-search-forward
  "N" nonincremental-repeat-search-backward
  "m" point-to-register
  "`" jump-to-register
  "~" modeal-toggle-case
  "g u" modeal-downcase
  "g U" modeal-upcase
  "g =" (@ modeal-indent-buffer (indent-region (point-min) (point-max)))
  "]" scroll-down
  "[" scroll-up
  "{" scroll-left
  "}" scroll-right
  ;; Personal Preferences
  "M-q" (call-interactively (key-binding (kbd "C-g")))
  "H" (@ qv/left4 (backward-char 4))
  "L" (@ qv/right4 (forward-char 4))
  "K" (@ qv/up4 (previous-line 4))
  "J" (@ qv/down4 (next-line 4))
  "g j" end-of-buffer
  "g k" beginning-of-buffer
  "g h" beginning-of-line
  "g l" move-end-of-line
  "g H" beginning-of-line
  "C-M-j" end-of-buffer
  "C-M-k" beginning-of-buffer
  "C-M-h" beginning-of-line
  "C-M-l" end-of-line
  "M" (@ modeal-merge-lines (next-line) (join-line)))

;;;; Insert keymap
(modeal-keys modeal-insert-map
  :full t
  :parent (modeal-add-keymap-prefix modeal-normal-map "M-")
  "M-q" (insert-keymode 0)
  "M-9" (@ insert-parens
           (insert "(\n)") (lisp-indent-line)
           (previous-line 1) (end-of-line))
  "M-[" (@ insert-brackets
           (insert "[\n]") (lisp-indent-line)
           (previous-line 1) (end-of-line))
  "M-{" (@ insert-braces
           (insert "{\n}") (lisp-indent-line)
           (previous-line 1) (end-of-line))
  "M-\"" (@ insert-single-quotes
           (insert "\"\"") (backward-char 1))
  "M-'" (@ insert-double-quotes
            (insert "\"\n\"") (lisp-indent-line)
            (previous-line 1) (end-of-line)))
(dolist (char (number-sequence 32 126))
  (define-key modeal-insert-map (vector char) 'self-insert-command))

;;;; Motion keymap
(modeal-keys modeal-motion-map
  :sparse t
  "a l" modeal-around-line
  "i l" modeal-inside-line
  "a w" modeal-around-word
  "i w" modeal-inside-word
  "x" end-of-line
  "z" beginning-of-line)

;;;; Visual keymap
(modeal-keys modeal-visual-map
  :sparse t
  :parent modeal-motion-map
  "M-q"  (deactivate-mark)
  "v"  (deactivate-mark)
  "o"  exchange-point-and-mark
  "=" indent-region)

;;;; Action keymap
(modeal-keys modeal-action-map
  :sparse t
  "x" execute-extended-command
  "e" eval-expression
  ";" eval-expression
  "b" switch-to-buffer
  "w" delete-window
  "W" kill-buffer
  "f" find-file)

;;;; Window keymap
(modeal-keys modeal-window-map
  :sparse t
  "h"   windmove-left
  "j"   windmove-down
  "k"   windmove-up
  "l"   windmove-right
  "C-h" windmove-swap-left
  "C-j" windmove-swap-down
  "C-k" windmove-swap-up
  "C-l" windmove-swap-right
  "H"   (select-window (split-window nil nil 'left))
  "J"   (select-window (split-window nil nil 'down))
  "K"   (select-window (split-window nil nil 'up))
  "L"   (select-window (split-window nil nil 'right))
  "M-h" (window-resize nil 4 t)
  "M-j" (window-resize nil 1 nil)
  "M-k" (window-resize nil 1 nil)
  "M-l" (window-resize nil 4 t)
  "M-S-h" (window-resize nil 16 t)
  "M-S-j" (window-resize nil 4 nil)
  "M-S-k" (window-resize nil 4 nil)
  "M-S-l" (window-resize nil 16 t))

;;; Keymodes
;;;; Global
(use-global-map
 (make-composed-keymap
  (list modeal-normal-map
        (modeal-add-keymap-prefix modeal-window-map "M-")
        (modeal-add-keymap-prefix modeal-action-map "M-"))
  global-map))

;;;; Insert
(define-minor-mode insert-keymode
  "Minor mode that enables the insert keymap"
  nil "<I>"
  (make-composed-keymap
   modeal-insert-map
   (modeal-add-keymap-prefix modeal-normal-map "M-"))
  (when insert-keymode
    (setq minor-mode-map-alist
          (cons (cons 'insert-keymode modeal-insert-map)
                (-remove (lambda (e) (eq (car e) 'insert-keymode))
                         minor-mode-map-alist)))))

(defun modeal-insert-setup ()
  "Run when entering and exiting insert mode"
  (setq cursor-type (if insert-keymode '(bar . 1) t)))

(add-hook 'insert-keymode-hook 'modeal-insert-setup)

;;;; Visual
(unless (assoc 'mark-active minor-mode-map-alist)
  (push '(mark-active) minor-mode-map-alist))

(setcdr (assoc 'mark-active minor-mode-map-alist)
        modeal-visual-map)

(defun modeal-mark-hook ()
  "Run when activating and deactivating the mark"
  (blink-cursor-mode (if mark-active 0 1))
  (unless (and (not mark-active) insert-keymode)
    (setq cursor-type (if mark-active '(bar . 2) t))))

(add-hook 'activate-mark-hook 'modeal-mark-hook)
(add-hook 'deactivate-mark-hook 'modeal-mark-hook)

;;; Extra Stuff
;;;; Special Mode Map
(modeal-keys special-mode-map
  "r" revert-buffer
  "g" nil
  "h" nil)

;;;; Minibuffer
(add-hook 'minibuffer-setup-hook 'insert-keymode)

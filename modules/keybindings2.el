(qv/package dash)
(qv/package windmove)


;;; Motions/Operators
;;;; Operators
(defmacro qvk-defop (name custom &rest cmds)
  "Define an operator that acts using variables BEG and END.
Custom is a plist of strings (keys) and functions (motions) to be
specific motion keys for this operator only."
  (declare (indent 2))
  `(defun ,name (motion)
     (interactive (list (unless mark-active (apply 'qvk-read-motion ',custom))))

     (save-excursion
       ;; Only read a motion if the mark isn't active
       (if mark-active (deactivate-mark)
         (push-mark)
         (call-interactively motion))

       ;; Execute operator specific commands
       ,@cmds)))

;;;; Define Operators

(qvk-defop qvk-delete ("d" qvk-inside-line)
  (delete-region (mark) (point)))

(setq qvk-highlight-yank-time 0.1)
(qvk-defop qvk-copy ("y" qvk-inside-line)
  (copy-region-as-kill (mark) (point))
  (when qvk-highlight-yank-time
    (let ((o (make-overlay (mark) (point))))
      (overlay-put o 'face 'region)
      (run-with-timer qvk-highlight-yank-time nil #'delete-overlay o))))

(qvk-defop qvk-change ("c" qvk-inside-line)
  (delete-region (mark) (point))
  (qvk-insert-keymode))

(defvar qvk-last-kill nil)
(qvk-defop qvk-kill ("s" qvk-inside-line)
  (kill-region (mark) (point))
  ;; Clean up the previous marker
  (unless (markerp qvk-last-kill) (setq qvk-last-kill (make-marker)))
  (set-marker qvk-last-kill (point)))

(defun qvk-swap-kills ()
  (interactive)
  (let ((last-kill (car kill-ring)))
    (kill-region (mark) (point))
    (insert last-kill)

    (with-current-buffer (marker-buffer qvk-last-kill)
      (save-excursion (goto-char qvk-last-kill)
                      (insert (car kill-ring))))))

;;;; Motions
(qv/keys qvk-motion-map
  :sparse t)

(defmacro qvk-defmotion (name beg &rest end)
  "A simplified way to define a two sided motion."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     ,beg
     (push-mark)
     ,@end))

(defun qvk-read-motion (&rest custom)
  (let ((cmap (make-sparse-keymap)))
    (while (cdr custom) (define-key cmap (kbd (pop custom)) (pop custom)))
    (setq minor-mode-overriding-map-alist
          `((t . ,cmap) (t . ,qvk-motion-map)
            . ,minor-mode-overriding-map-alist))
    (unwind-protect (key-binding (read-key-sequence ""))
      (setq minor-mode-overriding-map-alist (cddr minor-mode-overriding-map-alist)))))


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

(qvk-defmotion qvk-inside-big-word
  (progn (forward-char) (backward-sexp))
  (forward-sexp))

(qvk-defmotion qvk-around-big-word
  (progn (forward-char) (backward-sexp 2) (forward-sexp))
  (progn (forward-sexp 2) (backward-sexp)))

(qvk-defmotion qvk-around-sexp
  (if (in-string-p)
      (while (in-string-p) (search-backward-regexp "[\"'`]"))
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
  ;; (message "Switching to keymode %s" keymode)
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

(qv/hook minibuffer-setup-hook qvk-insert-keymode)

;; Don't you dare change this to activate-mark-hook stupid
(qv/hook post-command-hook qvk-update-keymode
  (when (not (eq (current-buffer) qvk-last-buffer))
    (qvk-keymode qvk-keymode)
    (setq qvk-last-buffer (current-buffer)))

  (qv/delay 0
    (when (and mark-active (not (eq qvk-keymode 'visual)))
      (qvk-keymode 'visual))
    (when (and (not mark-active) (eq qvk-keymode 'visual))
      (qvk-keymode 'normal))))


;;; Keymaps
;;;; Find map

;; Required to be defined for normal map
(qv/require avy)
(qv/keys qvk-find-map
  :sparse t
  "m" ((push-mark) (consult-mark))
  "M" pop-global-mark
  "p" consult-yank-from-kill-ring
  "g" consult-line
  "G" qv/grep
  "f" (@ qv/affe-find (let ((dir (or (qv/activity-get :path) default-directory)))
                        (find-file (f-join dir (affe-find dir)))))
  "F" (defun qv/affe-find-in-directory (dir)
        (interactive "DDirectory: ")
        (find-file (f-join dir (affe-find dir))))

  "d" qv/avy-dired-file
  "w" qv/avy-word-in-line
  "l" avy-goto-line
  "b" pick-window)

;;;; Normal map

(qv/keys esc-map "q" qvk-normal-keymode)
(qv/key * "<escape>" ,esc-map)

(qv/keys qvk-normal-map
  :full t
  :prefix ("M-" qvk-M-normal-map)
  "M-q" keyboard-quit
  "h" (@ qvk-left (qvk-stay-on-line (backward-char =arg=)))
  "l" (@ qvk-right (qvk-stay-on-line (forward-char =arg=)))
  "k" (@ qvk-up (line-move-visual (- (or =arg= 1))))
  "j" (@ qvk-down (line-move-visual (or =arg= 1)))
  "H" (@ qvk-left4 (qvk-stay-on-line (backward-char 4)))
  "L" (@ qvk-right4 (qvk-stay-on-line (forward-char 4)))
  "K" (@ qvk-up4 (line-move-visual -4))
  "J" (@ qvk-down4 (line-move-visual 4))
  "g m" (@ qvk-middle-of-line (beginning-of-line) (forward-char (/ (- (line-end-position) (point)) 2)))
  "g j" end-of-buffer
  "g k" beginning-of-buffer
  "g h" (@ qv/beginning-of-line (beginning-of-line))
           ;; (if (not (line-move-visual -1 t)) (beginning-of-buffer)
           ;;   (setq goal-column 1) (line-move-visual 1) (beginning-of-line)))
  "g l" (@ qv/end-of-line (end-of-line))
           ;; (if (not (line-move-visual 1 t)) (end-of-buffer)
           ;;   (goto-char (1- (line-beginning-position)))))
  "&" (@ qvk-run-last-command
         (setq this-command last-command)
         (call-interactively last-command))
  "e" (@ qvk-forward-word
         (dotimes (_ (or =arg= 1))
           (qvk-stay-on-line (if (looking-at "[ \t]\\{2,\\}")
                                 (goto-char (match-end 0)) (forward-word)))))
  "b" (@ qvk-backward-word
         (dotimes (_ (or =arg= 1))
           (qvk-stay-on-line (if (looking-back "[ \t]\\{2,\\}" (line-beginning-position) t)
                                 (goto-char (match-beginning 0)) (backward-word)))))
  "w" (@ qvk-forward-to-word (qvk-stay-on-line (forward-to-word (or =arg= 1))))
  "W" up-sexp
  "E" forward-sexp
  "B" backward-sexp
  "0" beginning-of-line
  "g i" beginning-of-line-text
  "i" (@ qvk-insert-before-char (qvk-insert-keymode))
  "a" (@ qvk-insert-after-char (unless (eolp) (forward-char)) (qvk-insert-keymode))
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
  "d" qvk-delete
  "y" qvk-copy
  "s" qvk-kill
  "c" qvk-change
  "Y" (@ qvk-copy-line (qvk-copy 'qvk-around-line))
  "S" (@ qvk-kill-line (qvk-kill 'qvk-around-line))
  "D" (@ qvk-delete-line (qvk-delete 'qvk-around-line))
  "C" (@ qvk-change-line (qvk-change 'qvk-inside-line))
  "x" (@ qvk-delete-forward-char (qvk-delete 'qvk-right))
  "z" (@ qvk-delete-backward-char (qvk-delete 'qvk-left))
  "X" (@ qvk-delete-forward-word (qvk-delete 'qvk-forward-word))
  "Z" (@ qvk-delete-backward-word (qvk-delete 'qvk-backward-word))
  "p" (@ qvk-paste-after
         (let ((start (point)))
           (qvk-open-below) (qvk-normal-keymode) (qvk-paste =arg=)
           (ignore-errors (delete-region (point) (search-backward-regexp "\n+\\=" start)))))
  "P" (@ qvk-paste
         (let ((idx (if (numberp =arg=) =arg= 0)))
           (setq this-command 'yank kill-ring-yank-pointer (nthcdr idx kill-ring))
           (yank)))

  "g p" yank-pop

  "u" undo
  "t" qvk-forward-to-letter
  "T" qvk-backward-to-letter

  "f" ,qvk-find-map
  "/" nonincremental-search-forward
  "?" nonincremental-search-backward
  "g /" nonincremental-re-search-forward
  "g ?" nonincremental-re-search-backward
  "n" nonincremental-repeat-search-forward
  "N" nonincremental-repeat-search-backward

  "G" (@ qvk-goto (if (numberp =arg=) (goto-line =arg=) (end-of-buffer)))
  "m m" (@ qv/set-mark (setq qv/mark (point-marker)))
  "m g" (@ qv/goto-mark (switch-to-buffer (marker-buffer qv/mark)) (goto-char (marker-position qv/mark)))
  "m p" pop-to-mark-command
  "~" (@ qvk-toggle-case (if (memq (aref (buffer-substring (point) (1+ (point))) 0) (number-sequence ?A ?Z))
                             (downcase-region (point) (1+ (point))) (upcase-region (point) (1+ (point))))
                         (forward-char))
  "g U" ,(qvk-defop qvk-upcase ("U" qvk-inside-line) (upcase-region (mark) (point)))
  "g u" ,(qvk-defop qvk-downcase ("u" qvk-inside-line) (downcase-region (mark) (point)))
  "g v" (activate-mark)
  "g a" (@ qvk-select-all (set-mark (point-min)) (end-of-buffer))
  "g t" transpose-words
  "g T" transpose-chars
  "]" scroll-up
  "[" scroll-down
  "}" scroll-left
  "{" scroll-right

  "g f" fill-paragraph
  "M" (@ qvk-merge-lines
         (if mark-active
             (replace-string "\n" "" nil (min (point) (mark)) (max (point) (mark)))
         (next-line) (join-line)))

  ";" (@ qvk-comment-line (save-excursion (comment-line 1)))
  ":" (@ qvk-comment-expr
         (or (save-excursion
               (when (looking-back "^[ \t]*")
                 (beginning-of-line-text))
               (forward-char (length comment-start))
               (when-let ((start (comment-beginning)))
                 (goto-char start)
                 (while (comment-forward 1))
                 (uncomment-region start (point))))
             (comment-region (point) (save-excursion (forward-sexp) (point)))))
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

  "$" (insert (format "$%s" (1- qv/result-number)))

  "g =" (@ qv/format-buffer
           (indent-region (point-min) (point-max))
           (whitespace-cleanup))

  "(" qvk-paren-replace
  ")" qvk-paren-delete

  "'" qvk-read-snippet

  "1" digit-argument
  "2" digit-argument
  "3" digit-argument
  "4" digit-argument
  "5" digit-argument
  "6" digit-argument
  "7" digit-argument
  "8" digit-argument
  "9" digit-argument
  "0" digit-argument)

;; All windmove keybindings get put into the windmove map
(when (boundp 'windmove-mode)
  (windmove-mode 0))

;;;; Insert map
(qv/keys qvk-insert-map
  :sparse t
  "M-q" (@ qv/insert-quit
           (if (not (minibufferp)) (qvk-normal-keymode)
             ;; In case you do it by accident
             (let ((content (minibuffer-contents)))
               (unless (string= content "") (push content minibuffer-history))
               (abort-minibuffers))))
  "M-Q" qvk-normal-keymode

  "C-M-l" end-of-line
  "C-M-h" beginning-of-line
  "C-M-j" end-of-buffer
  "C-M-k" beginning-of-buffer

  "M-(" ((insert "()") (backward-char))
  "M-9" ((insert "()") (backward-char))
  "M-{" ((insert "{}") (backward-char))
  "M-[" ((insert "[]") (backward-char))
  "M-<" ((insert "<>") (backward-char))
  "M-'" ((insert "''") (backward-char))
  "M-`" ((insert "``") (backward-char))
  "M-\"" ((insert "\"\"") (backward-char))
  "M-*" ((insert "**") (backward-char))
  "M-/" ((insert "//") (backward-char))

  "1" self-insert-command
  "2" self-insert-command
  "3" self-insert-command
  "4" self-insert-command
  "5" self-insert-command
  "6" self-insert-command
  "7" self-insert-command
  "8" self-insert-command
  "9" self-insert-command
  "0" self-insert-command)


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
  "a b" qvk-around-big-word
  "i b" qvk-inside-big-word
  "a l" qvk-around-line
  "i l" qvk-inside-line)

(dolist (char '("\"" "'" "`" "(" "[" "{" "<"))
  (eval `(qv/keys qvk-motion-map
           ,(concat "i " char) (qv/select-inside ,char)
           ,(concat "a " char) (qv/select-around ,char))))

;;;; Visual map
(qv/keys qvk-visual-map
  :sparse t
  :parent qvk-motion-map
  "q" (@ qvk-deactivate-mark (deactivate-mark))
  "M-q" qvk-deactivate-mark
  "v" qvk-deactivate-mark

  "o" exchange-point-and-mark
  "I" qvk-macro-on-region

  ":" comment-region
  "g :" uncomment-region

  "." (let (deactivate-mark) (qvk-run-on-lines qvk-shift-right (mark) (point)))
  "," (let (deactivate-mark) (qvk-run-on-lines qvk-shift-left (mark) (point)))


  "r" (let ((c (string (read-char "Surround with: ")))) (qv/surround c c))
  "(" (qv/surround "(" ")")
  ")" (qv/surround "(" ")")
  "[" (qv/surround "[" "]")
  "]" (qv/surround "[" "]")
  "{" (qv/surround "{" "}")
  "}" (qv/surround "{" "}")
  "<" (qv/surround "<" ">")
  ">" (qv/surround "<" ">")
  "'" (qv/surround "'" "'")
  "`" (qv/surround "`" "`")
  "\"" (qv/surround "\"" "\"")
  "*" (qv/surround "/*" "*/")
  "\\" (qv/surround "\\( " " \\)")

  "D" delete-rectangle
  "S" kill-rectangle
  "Y" copy-rectangle-as-kill

  "x" qvk-swap-kills)

;;;; Action map
(qv/keys qvk-action-map
  :sparse t
  :prefix ("M-" qvk-M-action-map)
  "f" find-file
  "e" qv/eval
  "s" save-buffer

  ;; "0" (switch-to-buffer (nth 10 qv/tab-line-tabs))
  ;; "9" (switch-to-buffer (nth 9  qv/tab-line-tabs))
  ;; "8" (switch-to-buffer (nth 8  qv/tab-line-tabs))
  ;; "7" (switch-to-buffer (nth 7  qv/tab-line-tabs))
  ;; "6" (switch-to-buffer (nth 6  qv/tab-line-tabs))
  ;; "5" (switch-to-buffer (nth 5  qv/tab-line-tabs))
  ;; "4" (switch-to-buffer (nth 4  qv/tab-line-tabs))
  ;; "3" (switch-to-buffer (nth 3  qv/tab-line-tabs))
  ;; "2" (switch-to-buffer (nth 2  qv/tab-line-tabs))
  ;; "1" (switch-to-buffer (nth 1  qv/tab-line-tabs))
  )

;;; Keymodes

(qvk-defkeymode normal
    (qvk-normal-map qvk-M-action-map)
  :cursor box)

(qv/keys qv/exwm-insert-override-map :sparse t)
(qvk-defkeymode insert
    (qvk-insert-map qvk-M-normal-map qv/exwm-insert-override-map)
  :cursor (bar . 1))

(qvk-defkeymode visual
    (qvk-visual-map qvk-normal-map qvk-M-action-map)
  :cursor (bar . 3))

(qvk-defkeymode none ())

(qvk-keymode qvk-keymode)

;;; Extra
;;;; Remove global keys
(qv/keys *
  "C-k" nil "C-j" nil
  "C-l" nil "C-w" nil)


;;;; Define mark
(defvar qv/mark (point-marker))

;;;; Delete delimiters
(defun qv/select-around (start)
  (while (not (or (bobp) (looking-at (regexp-quote start))))
    (call-interactively #'er/expand-region)))

(defun qv/select-inside (start)
  (while (not (or (bobp) (looking-at (regexp-quote start))))
    (call-interactively #'er/expand-region))
  (forward-char)
  (set-mark (1- (mark))))


;;;; JK/KJ Exits Insert
(setq qv/previous-command-event nil)
(qv/hook post-command-hook qv/detect-jk
  (when (eq qvk-keymode 'insert)
    (let ((events (list qv/previous-command-event last-command-event)))
      (when (and (eq ?j (car events)) (eq ?k (cadr events)))
        (delete-backward-char 2)
        (qvk-normal-keymode))))
  (setq qv/previous-command-event last-command-event))

;;;; Special Eval
(setq qv/result-number 1)
(defun qv/eval (expr)
  (interactive (list (read--expression "Eval: ")))
  ;; (let* ((time (benchmark-elapse (setq $0 (qv/math expr))))
  (let* ((time (benchmark-elapse (setq $0 (eval expr))))
         (var (format "$%s" qv/result-number))
         (p (prin1-to-string $0)))

    (set (intern var) $0)
    (put (intern var) 'variable-documentation expr)
    (setq qv/result-number (1+ qv/result-number))

    (message
     "%s %s %s%s %s%s"
     (propertize (format "(%ss)" time) 'face 'shadow)
     (propertize var 'face 'help-key-binding)
     (propertize "=" 'face 'bold)
     ;; (if (not (and (symbolp expr) (not (local-variable-p expr)))) ""
     ;;   (format " %s %s" (propertize "global" 'face 'help-key-binding)
     ;;           (propertize "=" 'face 'bold)))
     "" p
     (if (not (and (symbolp expr) (local-variable-p expr))) ""
       (format "   %s = %s" (propertize "global" 'face 'help-key-binding)
               (default-value expr))))))

(qv/advise :around helpful--variable-p qv/exclude-dollar-variables (func sym)
  (let ((name (symbol-name sym)))
    (unless (or (string= name "") (eq (aref name 0) ?$)) (funcall func sym))))


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
  (goto-char (1+ (save-excursion
                   (forward-char -1)
                   (search-backward (string c) (line-beginning-position))))))

;;;; Keep Column
(setq-default qvk-last-pos nil)
(qv/hook post-command-hook qvk-cursor-column
  (if (not truncate-lines) (setq goal-column nil)
    (when qvk-last-pos
      (when (or (not (eolp))
                (and (eq (car qvk-last-pos) (line-number-at-pos))
                     (not (eq (cdr qvk-last-pos) (current-column)))))
        (setq-local goal-column (current-column))))
    (when track-eol (end-of-line))

    (set-face-attribute 'cursor nil :background
                        (if track-eol (qv/color blue) "white"))
    ;; (message "Tracking: %s" goal-column)

    (setq-local qvk-last-pos (cons (line-number-at-pos) (current-column)))))

;;;; Stay on line
(defmacro qvk-stay-on-line (&rest exprs)
  "Advises around a function to make sure that it stays on the same line"
  `(let ((pos (point))
         (line-beg (line-beginning-position))
         (line-end (line-end-position)))
     (unwind-protect
         (progn . ,exprs)
       (goto-char (max line-beg (min line-end (point)))))))

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
(qv/advise :around newline qvk-newline-advice (newline &rest args)
  (when (and (not (bobp)) (not (eobp))
             (member (buffer-substring (1- (point)) (1+ (point)))
                     '("()" "[]" "{}" "''" "``" "\"\"" "<>" "><")))
    (save-excursion (apply newline args)))

  (apply newline args))


;;;; Parenthesis
;; If open is a space, just insert a space inside of the parens
(defun qvk-paren-replace (open)
  (interactive (list (read-key "Replace With: ")))
  (unless (looking-at-p "[])}>[({<\"'`]")
    (error "Not a bracketed expression"))

  (when (eq open 13) (setq open 10))
  (setq open (pcase open (?\) ?\() (?} ?{) (?\] ?\[) (?> ?<) (o o)))
  (let ((close (pcase open (?\( ?\)) (?{ ?}) (?\[ ?\]) (?< ?>) (o o)))
        (noreplace (or (eq open ?\s) (eq open 10))) ;; If newline or space, insert, don't replace
        (backspace (memq open '(127 33554464))) ;; Backspace or Shift+Space)
        start end)
    (save-excursion
      (when (or (looking-at-p "[])}>]")
                (and (looking-at-p "['\"`]") (in-string-p)))
        (forward-char) (backward-sexp))
      (save-excursion
        (setq start (point))
        (forward-sexp)
        (when backspace (backward-char))
        (if noreplace (backward-char) (delete-backward-char 1))
        (unless backspace (insert (string close)))
        (setq end (+ 2 (point))))
      (when backspace (forward-char))
      (if noreplace (forward-char) (delete-forward-char 1))
      (unless backspace (insert (string open)))
      (backward-char))

    (when (eq open 10) (indent-region start end))))

(defun qvk-paren-delete ()
  (interactive)
  (unless (looking-at-p "[])}>[({<\"']")
    (error "Not a bracketed expression"))
  (save-excursion
    (when (or (looking-at-p "[])}>]")
              (and (looking-at-p "['\"]") (in-string-p)))
      (forward-char) (backward-sexp))
    (save-excursion (forward-sexp) (delete-backward-char 1)
                    (beginning-of-line-text)
                    (when (eolp) (delete-region (line-beginning-position) (1+ (point)))))
    (delete-forward-char 1)))

;;;; Surround
(defun qv/surround (left right)
  (unless mark-active (error "Mark is not active."))

  (let ((p (point)) (m (mark)))
    (deactivate-mark)
    (goto-char (max p m)) (insert right)
    (goto-char (min p m)) (insert left)
    (backward-char)))

;;;; Open invisible overlays
(qv/keys *
  "C-x C-w" ((insert "\\<\\>") (backward-char 2)))


(setq qv/search-opened-overlays nil)
(setq qv/search-open-jumps
      '(
        qvk-search-forward
        qvk-re-search-forward
        qvk-search-backward
        qvk-re-search-backward
        qvk-repeat-search-forward
        qvk-repeat-search-backward
        qvmc-add-next-match
        qvmc-add-previous-match
        qvmc-add-next-skip
        qvmc-add-previous-skip
        flycheck-next-error
        flycheck-previous-error
        flyspell-goto-next-error
        undo
        jump-to-register))

(qv/hook post-command-hook qv/search-open-invisible
  :remove
  (if (and (symbolp this-command)
           (let ((s (symbol-name this-command)))
             (or (s-starts-with-p "nonincremental-" s)
                 (s-contains-p "-next" s))))
      (progn
        (dolist (a qv/search-opened-overlays)
          (when (overlay-buffer (car a))
            (overlay-put (car a) 'invisible (cdr a))))
        (setq qv/search-opened-overlays nil)
        (dolist (o (overlays-at (point)))
          (when-let ((inv (overlay-get o 'invisible)))
            (push (cons o inv) qv/search-opened-overlays)
            (overlay-put o 'invisible nil))))

    (while qv/search-opened-overlays
      (delete-overlay (car (pop qv/search-opened-overlays))))
    (dolist (o (overlays-at (point)))
      (when (overlay-get o 'invisible)
        (delete-overlay o)))))

(qv/hook post-command-hook qv/search-open-invisible
  (if (not (and (symbolp this-command)
                (memq this-command qv/search-open-jumps)))
      (while qv/search-opened-overlays
        (delete-overlay (car (pop qv/search-opened-overlays))))

    (dolist (a qv/search-opened-overlays)
      (when (overlay-buffer (car a))
        (overlay-put (car a) 'invisible (cdr a))))
    (setq qv/search-opened-overlays nil)
    (dolist (o (overlays-at (point)))
      (when-let ((inv (overlay-get o 'invisible)))
        (push (cons o inv) qv/search-opened-overlays)
        (overlay-put o 'invisible nil)))))

;;;; Snippets

(qv/keys qvk-read-snippet-map
  :sparse t
  "'" qvk-read-snippet)
(push (cons 'qvk-snippets qvk-read-snippet-map)
      minor-mode-map-alist)

;; An alist of keybinds to snippets
(defvar-local qvk-snippets nil)

(defun qvk-read-snippet (snippet &optional pos)
  (interactive
   (let ((map (make-sparse-keymap)) vector)
     (dolist (snip (cons '("'" "'") qvk-snippets))
       (define-key map (kbd (car snip)) (vector (cdr snip))))

     (push (cons t map) minor-mode-overriding-map-alist)
     (unwind-protect
         (setq vector (key-binding (read-key-sequence "Snippet: ")))
       (pop minor-mode-overriding-map-alist))
     (unless (vectorp vector) (error "Invalid snippet"))
     (aref vector 0)))

  (cond ((stringp snippet) (insert snippet))
        ((functionp snippet) (funcall snippet))
        (t (error "Snippet must be string or function")))

  (when (numberp pos)
    (when (>= pos 0) (backward-char (length snippet)))
    (forward-char pos)
    (qvk-insert-keymode)))

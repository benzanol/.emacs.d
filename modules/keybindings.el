;;; Foundation
;;;; Operators
(defmacro qvk-defop (name custom &rest cmds)
  "Define an operator that acts using variables BEG and END.
Custom is a plist of strings (keys) and functions (motions) to be
specific motion keys for this operator only."
  (declare (indent 2))
  `(defun ,name (&optional motion)
     (interactive)
     (save-excursion
       (let ((start (point)))
         ;; Only read a motion if the mark isn't active
         (unless mark-active
           (push-mark)
           (call-interactively
            (or motion (apply 'qvk-read-motion ',custom)))
           (unless mark-active (activate-mark)))

         ;; Execute operator specific commands
         ,@cmds))))

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
  (qvk-remember :motion
    (let ((cmap (make-sparse-keymap)))
      (while (cdr custom)
        (define-key cmap (kbd (pop custom)) (pop custom)))
      (setq minor-mode-map-alist
            `((t . ,cmap) (t . ,qvk-motion-map)
              . ,minor-mode-map-alist))
      (unwind-protect (key-binding (read-key-sequence ""))
        (setq minor-mode-map-alist (cddr minor-mode-map-alist))))))

;;;; Defining Keymodes
(defvar qvk-keymodes nil
  "Alist of the form (NAME SPEC).")

(defvar qvk-keymode nil
  "Symbol of current keymode.")

(defmacro qvk-defkeymode (name &rest maps)
  (declare (indent 1))
  `(progn
     (setq qvk-keymodes
           (cons (cons ',name ',maps)
                 (assq-delete-all ',name qvk-keymodes)))
     (defun ,name ()
       (interactive)
       (run-hook-with-args 'qvk-before-keymode-hook)
       (setq minor-mode-map-alist
             (cons (cons 'qvk-keymode
                         (let ((ms (alist-get ',name qvk-keymodes)))
                           (cons 'keymap (mapcar 'eval ms))))
                   (assq-delete-all 'qvk-keymode
                                    minor-mode-map-alist))
             qvk-keymode ',name)
       (run-hook-with-args 'qvk-after-keymode-hook))))

(defvar qvk-before-keymode-hook nil)
(defvar qvk-after-keymode-hook nil)

;;;; Keymap Prefix
(defun qvk-add-keymap-prefix (keymap prefix &optional recursive)
  "Return a new keymap containing each of the keys in KEYMAP
modified by PREFIX.

PREFIX should be a string to add before the string representation of
a keymap to modify it, for example \"M-\" would add the meta modifier
to a key sequence."

  (when (symbolp keymap) (setq keymap (symbol-value keymap)))
  (if (or (not (stringp prefix)) (eq prefix "")) keymap
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
           (if (and recursive (keymapp binding))
               (qvk-add-keymap-prefix binding prefix t)
             binding)))
       keymap)
      new-map)))

;;; Keymaps
;;;; Motion Map
(qv/keys qvk-motion-map
  :sparse t
  "x" end-of-line
  "z" beginning-of-line
  "a l" qvk-around-line
  "i l" qvk-inside-line)

;;;; Action Map
(qv/keys qvk-action-map
  :sparse t
  :prefix ("M-" qvk-M-action-map)
  "f" find-file
  "e" eval-expression)

;;;; Insert Map
(setq qvk-insert-map (make-keymap))
(dolist (char (number-sequence 32 126))
  (define-key qvk-insert-map (vector char) 'self-insert-command))

(qv/keys qvk-insert-map
  "M-q" qvk-normal-mode
  "RET" newline)

;;;; Visual Map
(qv/keys qvk-visual-map
  :sparse t
  "M-q" deactivate-mark
  "o" exchange-point-and-mark)

;;;; Normal Map
(qv/keys qvk-normal-map
  :full t
  :prefix ("M-" qvk-M-normal-map)
  "h" backward-char
  "j" (@ qvk-down (next-line 1))
  "k" (@ qvk-up (previous-line 1))
  "l" forward-char
  "e" forward-word
  "w" forward-to-word
  "b" backward-word
  "W" up-sexp
  "E" forward-sexp
  "B" backward-sexp
  "0" beginning-of-line
  "^" beginning-of-line-text
  "$" end-of-line
  "i" (@ qvk-insert-before-char (qvk-insert-mode))
  "a" (@ qvk-insert-after-char (forward-char) (qvk-insert-mode))
  "I" (@ qvk-insert-beginning-of-line
         (beginning-of-line-text) (qvk-insert-mode))
  "A" (@ qvk-insert-end (end-of-line) (qvk-insert-mode))
  "o" (@ qvk-open-below (end-of-visual-line) (newline) (qvk-insert-mode))
  "O" (@ qvk-open-above (beginning-of-line) (newline) (forward-line -1)
                        (qvk-insert-mode))
  "r" qvk-replace-char
  "R" qvk-replace-mode
  "v" set-mark-command
  "V" qvk-around-line
  "f" qvk-forward-find-letter
  "F" qvk-backward-find-letter
  "t" qvk-forward-to-letter
  "T" qvk-backward-to-letter
  "y" qvk-copy
  "s" qvk-kill
  "d" qvk-delete
  "c" qvk-change
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
           (qvk-open-below) (qvk-normal-mode)
           (yank)
           (delete-region (point) (search-backward-regexp
                                   "\n+\\=" start t))))
  "P" yank
  "u" undo
  "/" qvk-search-forward
  "?" qvk-search-backward
  "g /" qvk-search-forward-regexp
  "g ?" qvk-search-backward-regexp
  "n" nonincremental-repeat-search-forward
  "N" nonincremental-repeat-search-backward
  "m" point-to-register
  "`" jump-to-register
  "~" qvk-toggle-case
  "g u" qvk-downcase
  "g U" qvk-upcase
  "g =" (@ qvk-indent-buffer (indent-region (point-min) (point-max)))
  "]" scroll-up
  "[" scroll-down
  "{" scroll-left
  "}" scroll-right
  ;; Personal Preferences
  "M-q" (@ qv/keyboard-quit
           (if (active-minibuffer-window)
               (with-selected-window (active-minibuffer-window)
                 (keyboard-escape-quit))
             (eval (list (key-binding (kbd "C-g"))))))
  "H" (@ qvk-left4 (backward-char 4))
  "L" (@ qvk-right4 (forward-char 4))
  "K" (@ qvk-up4 (previous-line 4))
  "J" (@ qvk-down4 (next-line 4))
  "g j" end-of-buffer
  "g k" beginning-of-buffer
  "g h" beginning-of-line-text
  "g l" move-end-of-line
  "g H" beginning-of-line
  "C-M-j" end-of-buffer
  "C-M-k" beginning-of-buffer
  "C-M-h" beginning-of-line
  "C-M-l" end-of-line
  "M" (@ qvk-merge-lines (next-line) (join-line)))

;;;; Keymodes
(qvk-defkeymode qvk-insert-mode
  qvk-insert-map qvk-M-normal-map)

(qvk-defkeymode qvk-normal-mode
  qvk-M-action-map)

(qvk-defkeymode qvk-visual-mode
  qvk-visual-map qvk-motion-map)

(use-global-map (list 'keymap qvk-normal-map (current-global-map)))

;;; Functions
;;;; Remember
;; Remember certain values for the current command loop
(defvar qvk-memory nil
  "Alist of values being remembered this command loop.")

(defvar qvk-memory-command nil
  "sed last to set the memory.")

(defmacro qvk-remember (key &rest forms)
  (declare (indent 1))
  `(progn (setq qvk-memory-command last-command)
          (or (alist-get (or ,key t) qvk-memory)
              (cdr (car (push (cons (or ,key t) (progn . ,forms))
                              qvk-memory))))))

(defun qvk-forget ()
  (unless (eq qvk-memory-command last-command)
    (setq qvk-memory nil
          qvk-memory-command nil)))
(add-hook 'post-command-hook 'qvk-forget)

;;;; Operators
(qvk-defop qvk-delete ("d" qvk-inside-line)
  (delete-region (mark) (point)))

(qvk-defop qvk-copy ("y" qvk-inside-line)
  (copy-region-as-kill (mark) (point)))

(qvk-defop qvk-kill ("s" qvk-inside-line)
  (kill-region (mark) (point)))

(qvk-defop qvk-change ("c" qvk-inside-line)
  (delete-region (mark) (point))
  (qvk-insert-mode))

;;;; Motions
(qvk-defmotion qvk-around-word
  (backward-word) (forward-word))

(qvk-defmotion qvk-inside-line
  (beginning-of-line) (end-of-line))

(qvk-defmotion qvk-around-line
  (beginning-of-visual-line)
  (end-of-visual-line)
  (goto-char (min (point-max) (1+ (point)))))

(defun up-sexp ()
  (interactive)
  (if (in-string-p)
      (and (search-backward-regexp "[^\\]\"" nil t)
           (forward-char))
    (let ((pos (point)))
      (while (ignore-errors (or (backward-sexp) (not (bobp)))))
      (if (bobp) (goto-char pos) (search-backward "(" nil t)))))

(qvk-defmotion qvk-inside-sexp
  (forward-sexp)
  (backward-sexp)
  (exchange-point-and-mark))

(qvk-defmotion qvk-around-sexp
  (up-sexp)
  (forward-sexp))

;;;; Searching
(defun qvk-search (&optional back regexp)
  (let ((str (qvk-remember :search
               (read-string
                (format "%sSearch%s: "
                        (if regexp "Regexp " "")
                        (if back " Backward" "")))))
        (func (intern (format "nonincremental-%ssearch-%sward"
                              (if regexp "re-" "") (if back "back" "for"))))
        (p (point)))
    (unless (ignore-errors (funcall func str))
      (if back (point-max) (point-min))
      (if (ignore-errors (funcall func str))
          (message "Wrapped around")
        (goto-char p)
        (message "Search failed")))))

(defun qvk-search-forward () (interactive) (qvk-search nil nil))
(defun qvk-search-backward () (interactive) (qvk-search t nil))
(defun qvk-search-forward-regexp () (interactive) (qvk-search nil t))
(defun qvk-search-backward-regexp () (interactive) (qvk-search t t))

(defun qvk-find-letter (&optional back to)
  (funcall (if back 'search-backward 'search-forward)
           (qvk-remember :find (string (read-char "Letter: ")))
           (if back (line-beginning-position) (line-end-position)))
  (when to (if back (forward-char) (backward-char))))

(defun qvk-forward-find-letter () (interactive) (qvk-find-letter nil nil))
(defun qvk-backward-find-letter () (interactive) (qvk-find-letter t nil))
(defun qvk-forward-to-letter () (interactive) (qvk-find-letter nil t))
(defun qvk-backward-to-letter () (interactive) (qvk-find-letter t t))

;;;; Replacing
(defun qvk-replace-char (&optional count)
  (interactive
   (list (if (numberp current-prefix-arg)
             current-prefix-arg nil)))
  (let ((char (read-char "Replace: ")))
    (delete-char (or count 1))
    (insert (make-string (or count 1) char))))

(defun qvk-replace-mode ()
  (interactive)
  (let ((char (read-char "Replace: ")))
    (while (and (>= char 32) (<= char 126))
      (delete-char 1)
      (insert (string char))
      (setq char (read-char "Replace: ")))))

;;; Extra
;;;; Minibuffer
(setq qvk-last-buffer (current-buffer))
(qv/hook window-state-change-hook qvk-window-change
  (unless (eq qvk-last-buffer (current-buffer))
    (setq qvk-last-buffer (current-buffer))
    (if (and (eq (cadr (window-tree)) (selected-window))
             (not (eq (cadr (current-local-map)) y-or-n-p-map)))
        (qvk-insert-mode)
      (qvk-normal-mode))))

;;;; Different Cursors
(qv/hook qvk-after-keymode-hook qvk-set-cursor
  (setq cursor-type
        (if mark-active '(bar . 3)
          (pcase qvk-keymode
            ('qvk-insert-mode '(bar . 1))
            ('qvk-normal-mode 'box)))))

(add-hook 'activate-mark-hook 'qvk-set-cursor)
(add-hook 'deactivate-mark-hook 'qvk-set-cursor)

;;;; Stay on Line
(defun qvk-stay-on-line-advice (func &rest args)
  "Advises around a function to make sure that it stays on the same line"
  (let ((pos (point))
        (line-beg (line-beginning-position))
        (line-end (line-end-position)))
    (apply func args)
    (goto-char (max line-beg (min line-end (point))))))

(advice-add 'forward-char :around 'qvk-stay-on-line-advice)
(advice-add 'backward-char :around 'qvk-stay-on-line-advice)
(advice-add 'forward-word :around 'qvk-stay-on-line-advice)
(advice-add 'backward-word :around 'qvk-stay-on-line-advice)
(advice-add 'forward-to-word :around 'qvk-stay-on-line-advice)
(advice-add 'backward-to-word :around 'qvk-stay-on-line-advice)

(bz/require pick-window)

;;; Special Maps
;;;; Motions

(bz/keys bz/motion-map
  :full t
  "i l" (@ bz/motion-in-line (set-mark (point-at-bol)) (end-of-line))
  "a l" (@ bz/motion-around-line
           (beginning-of-visual-line) (set-mark (point))
           (end-of-visible-line)
           (goto-char (min (point-max) (1+ (point)))))

  "i w" (@ bz/inside-word (forward-char) (backward-word) (set-mark (point)) (forward-word))
  "a w" (@ bz/around-word (forward-char) (backward-to-word 1) (set-mark (point)) (forward-word))

  "i s" (@ bz/motion-in-sexp (bz/up-sexp) (set-mark (1+ (point))) (forward-sexp) (backward-char))
  "a s" (@ bz/motion-around-sexp (bz/up-sexp) (set-mark (point)) (forward-sexp))

  "x" end-of-line
  "z" beginning-of-line)


;;;; Mark active

(bz/keys bz/selection-map
  :sparse t
  :parent bz/motion-map

  "M-q" (@ bz/deactivate-mark (deactivate-mark))
  "q" bz/deactivate-mark

  "o" exchange-point-and-mark

  "C-," mc2-add-previous-match
  "C-." mc2-add-next-match
  "C-<" mc2-add-previous-word-match
  "C->" mc2-add-next-word-match
  "A" mc2-add-lines
  "I" mc2-add-all-matches
  "Z" mc2-add-search
  "X" mc2-add-regexp-search

  "y" bz/operator-copy

  ":" comment-region
  "g :" uncomment-region

  "," (@ bz/shift-region-left  (let (deactivate-mark) (bz/run-on-lines bz/shift-left  (mark) (point))))
  "." (@ bz/shift-region-right (let (deactivate-mark) (bz/run-on-lines bz/shift-right (mark) (point))))

  "R" bz/replace-region
  "r" bz/surround
  "(" (bz/surround "(" ")")
  ")" (bz/surround "(" ")")
  "[" (bz/surround "[" "]")
  "]" (bz/surround "[" "]")
  "{" (bz/surround "{" "}")
  "}" (bz/surround "{" "}")
  "<" (bz/surround "<" ">")
  ">" (bz/surround "<" ">")
  "*" (bz/surround "/*" "*/")
  "\\" (bz/surround "\\( " " \\)")

  "x" bz/swap-kills)


;;;; Action map

(bz/keys bz/action-map
  :full t
  :prefix ("M-" bz/mod-action-map)

  "q" (@ bz/keyboard-quit (call-interactively (key-binding (kbd "C-g"))))

  "h" windmove-left
  "l" windmove-right
  "k" windmove-up
  "j" windmove-down

  "H" (@ bz/split-window-left  (bz/split-window 'left))
  "L" (@ bz/split-window-right (bz/split-window 'right))
  "J" (@ bz/split-window-down  (bz/split-window 'down))
  "K" (@ bz/split-window-up    (bz/split-window 'up))

  "C-h" bz/window-move-left
  "C-l" bz/window-move-right
  "C-k" bz/window-move-up
  "C-j" bz/window-move-down

  "," (@ bz/shrink-window-horizontal (bz/window-resize -8 t))
  "." (@ bz/grow-window-horizontal   (bz/window-resize +8 t))
  "<" (@ bz/shrink-window-vertical   (bz/window-resize -2 nil))
  ">" (@ bz/grow-window-vertical     (bz/window-resize +2 nil))

  "f" find-file
  "F" bz/fuzzy-find-file

  "w" bz/close-window
  "W" bz/kill-current-buffer

  "m" bz/pick-window-pull-buffer
  "M" bz/pull-window

  "b" bz/buffer-history-back
  "B" bz/switch-to-buffer
  "Q" previous-buffer
  "S" (@ bz/switch-to-scratch (switch-to-buffer "*scratch*"))

  "x" execute-extended-command
  "e" bz/eval
  "E" repeat-complex-command
  "r" $
  "t" bz/vterm
  "T" (@ bz/vterm-next (bz/buffer-history-back
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) '(vterm-mode vterm-copy-mode)))))
  )

(defun bz/window-resize (delta horizontal)
  ;; In case in the minibuffer
  (let ((window-size-fixed nil))
    (window-resize (selected-window) delta horizontal)))

(defun bz/split-window (direction)
  (let ((window-size-fixed nil))
    (select-window (split-window nil nil direction))
    (when (memq major-mode '(exwm-mode term-mode vterm-mode))
      (switch-to-buffer "*scratch*"))))

(defun bz/close-window ()
  (interactive)
  (let ((remove (selected-window))
        (goto (or (window-next-sibling) (window-prev-sibling))))
    (while (and (windowp goto) (not (window-live-p goto)))
      (setq goto (window-child goto)))
    (select-window goto)
    (delete-window remove)))

(defun bz/kill-current-buffer ()
  (interactive)
  (let* ((buf (current-buffer))
         (process (get-buffer-process buf))
         (tabs (ignore-errors bz/tab-line-tabs))
         (idx (when tabs (seq-position tabs buf)))
         (new-tab (when idx (or (nth (1+ idx) tabs) (nth (1- idx) tabs)))))
    (if (buffer-live-p new-tab) (switch-to-buffer new-tab) (previous-buffer))
    (when process (kill-process process) (set-process-buffer process nil))
    (kill-buffer buf)))

(defun bz/pull-window ()
  (interactive)
  (switch-to-buffer (with-selected-window bz/last-window
                      (prog1 (current-buffer) (previous-buffer)))))


(defun bz/switch-to-buffer (&optional dir only-files)
  (interactive)

  (setq bz/buffer-history (-filter #'buffer-live-p bz/buffer-history))
  (when dir (setq dir (expand-file-name dir)))

  (let* ((vertico-sort-function nil)
         (bufs (delete-dups (append (cdr bz/buffer-history) (buffer-list))))
         (var (cond (only-files 'buffer-file-name) (dir 'default-directory)))
         (pred `(lambda (buf) (let ((value (buffer-local-value ',var (get-buffer buf))))
                                (and value (string-prefix-p ,dir (expand-file-name value))
                                     (not (string-prefix-p " " buf)))))))
    (switch-to-buffer (completing-read "Buffer: " (mapcar #'buffer-name bufs) (when var pred)))))


;;; Modal Maps
;;;; g Map

(bz/keys bz/g-map
  :full t
  "h" beginning-of-line
  "l" end-of-line
  "j" end-of-buffer
  "k" beginning-of-buffer

  "H" beginning-of-visual-line
  "L" end-of-visual-line
  "i" beginning-of-line-text

  "s" bz/spell-check
  "S" bz/spell-actions

  "m" (@ bz/set-mark (setq bz/mark (point-marker)))
  "M" (@ bz/goto-mark (switch-to-buffer (marker-buffer bz/mark))
                      (goto-char (marker-position bz/mark)))

  "n" (@ bz/selection-search-forward (deactivate-mark) (bz/search-forward (buffer-substring (mark) (point))))
  "N" (@ bz/selection-search-backward (deactivate-mark) (bz/search-backward (buffer-substring (mark) (point))))

  "u" bz/operator-downcase
  "U" bz/operator-upcase

  "t" transpose-words
  "T" transpose-chars

  "/" nonincremental-re-search-forward
  "?" nonincremental-re-search-backward

  "a" (@ bz/select-all (set-mark (point-min)) (end-of-buffer))
  "p" consult-yank-from-kill-ring
  "f" fill-paragraph
  "=" (@ bz/format-buffer
         (let ((buf-cmd (alist-get major-mode bz/indent-command-alist)))
           (if buf-cmd (funcall buf-cmd)
             (indent-region (point-min) (point-max))))
         (whitespace-cleanup)))

(defvar bz/indent-command-alist nil)


;;;; f Map

(bz/keys bz/f-map
  :full t

  "f" bz/fold-hide
  "F" bz/fold-show
  "l" bz/fold-level

  "g" consult-line
  "G" bz/grep
  "z" bz/fuzzy-find-file

  "m" (@ bz/read-mark (push-mark) (consult-mark))
  "p" consult-yank-from-kill-ring

  "w" bz/avy-word-in-line)


;;;; Navigation

(bz/keys bz/navigate-map
  :parent bz/mod-action-map
  :full t

  (?0 ?9) digit-argument

  "g" ,bz/g-map
  "f" ,bz/f-map

  ;; "k" (@ bz/up (previous-line =arg=))
  ;; "j" (@ bz/down (next-line =arg=))
  "k" (@ bz/up (line-move-visual (- (or =arg= 1))))
  "j" (@ bz/down (line-move-visual (or =arg= 1)))
  "K" (@ bz/up4 (bz/up 4))
  "J" (@ bz/down4 (bz/down 4))

  "h" (@ bz/left (bz/stay-on-line (backward-char =arg=)))
  "l" (@ bz/right (bz/stay-on-line (forward-char =arg=)))
  "H" (@ bz/left4 (bz/left 4))
  "L" (@ bz/right4 (bz/right 4))

  "E" forward-sexp
  "B" backward-sexp
  "W" (@ bz/up-sexp
         (if (in-string-p) (and (search-backward-regexp "[^\\][\"']" nil t) (forward-char))
           (let ((pos (point)))
             (while (ignore-errors (or (backward-sexp) (not (bobp)))))
             (if (bobp) (goto-char pos) (search-backward-regexp "[({[]" nil t)))))

  "v" set-mark-command
  "V" bz/visual-line
  "i" bz/insert

  ;; Get rid of debugger for searches
  "/" (defun bz/search-forward (str) (interactive "sSearch Forward: ") (ignore-errors (nonincremental-search-forward str)))
  "?" (defun bz/search-backward (str) (interactive "sSearch Backward: ") (ignore-errors (nonincremental-search-backward str)))
  "n" (@ bz/repeat-search-forward (ignore-errors (nonincremental-repeat-search-forward)))
  "N" (@ bz/repeat-search-backward (ignore-errors (nonincremental-repeat-search-backward))))



;;;; Normal

(bz/keys bz/normal-map
  :sparse t
  :parent bz/navigate-map
  :prefix ("M-" bz/mod-normal-map)

  "q" bz/q
  "RET" (@ bz/click bz/run-key-without-keymode)
  "SPC" (@ bz/fold-toggle bz/run-key-without-keymode)
  "S-SPC" (@ bz/fold-toggle-small bz/run-key-without-keymode)
  "C-SPC" (@ bz/fold-toggle-all bz/run-key-without-keymode)

  "[" (@ bz/up100 (previous-line 100))
  "]" (@ bz/down100 (next-line 100))

  "e" (@ bz/forward-word
         (bz/stay-on-line (if (looking-at "[ \t]\\{2,\\}")
                              (goto-char (match-end 0)) (forward-word))))
  "b" (@ bz/backward-word
         (bz/stay-on-line (if (looking-back "[ \t]\\{2,\\}" (point-at-bol) t)
                              (goto-char (match-beginning 0)) (backward-word))))
  "w" (@ bz/forward-to-word (bz/stay-on-line (forward-to-word 1)))

  "i" bz/insert
  "a" (@ bz/insert-after-char (unless (eolp) (forward-char)) (bz/insert))
  "I" (@ bz/insert-beginning-of-line
         (beginning-of-line-text) (bz/insert))
  "A" (@ bz/insert-end (end-of-line) (bz/insert))
  "o" (@ bz/open-below (end-of-visible-line) (newline) (bz/insert))
  "O" (@ bz/open-above (beginning-of-line) (newline) (forward-line -1) (bz/insert))

  "y" bz/operator-copy
  "d" bz/operator-delete
  "c" bz/operator-change
  "s" bz/operator-kill
  "Y" (@ bz/copy-line   (bz/operator-copy #'bz/motion-around-line))
  "D" (@ bz/delete-line (bz/operator-delete #'bz/motion-around-line))
  "C" (@ bz/change-line (bz/operator-change #'bz/motion-in-line))
  "S" (@ bz/kill-line   (bz/operator-kill   #'bz/motion-around-line))

  "x" (@ bz/delete-forward-char (bz/operator-delete #'bz/right))
  "z" (@ bz/delete-backward-char (bz/operator-delete #'bz/left))
  "X" (@ bz/delete-forward-word (bz/operator-delete #'bz/forward-word))
  "Z" (@ bz/delete-backward-word (bz/operator-delete #'bz/backward-word))

  "p" bz/paste-after
  "P" bz/paste

  "r" bz/replace-char
  "R" bz/replace-mode

  "G" (@ bz/goto-line (if (numberp =arg=) (goto-line =arg=) (end-of-buffer)))
  "m" pop-to-mark-command
  "M" (@ bz/merge-lines (next-line) (join-line))

  "t" bz/forward-to-letter
  "T" bz/backward-to-letter

  "u" undo
  "U" redo

  ";" (@ bz/comment-line (save-excursion (comment-line 1)))
  ":" bz/comment-expression

  "." bz/shift-right
  "," bz/shift-left

  "(" bz/paren-replace
  ")" bz/paren-delete

  "~" bz/toggle-case
  "$" (@ bz/insert-last-variable (insert (format "$%s" (1- bz/eval-variable-number))))
  "'" bz/insert-snippet
  "\"" (@ bz/insert-snippet-around (bz/paren-replace ?')))


;;;; Insert

(bz/keys bz/insert-map
  :sparse t
  :parent bz/mod-normal-map

  "M-q" (@ bz/insert-quit (if (minibuffer-window-active-p (selected-window))
                              (abort-minibuffers) (bz/normal)))
  "M-Q" bz/normal

  "M-C-l" end-of-line
  "M-C-h" beginning-of-line
  "M-C-j" end-of-buffer
  "M-C-k" beginning-of-buffer

  "M-9" ((insert "(") (save-excursion (forward-sexp) (insert ")")))
  "M-0" (save-excursion (forward-sexp) (insert ")"))

  "M-(" ((insert "()") (backward-char))
  "M-{" ((insert "{}") (backward-char))
  "M-[" ((insert "[]") (backward-char))
  "M-<" ((insert "<>") (backward-char))
  "M-'" ((insert "''") (backward-char))
  "M-`" ((insert "``") (backward-char))
  "M-\"" ((insert "\"\"") (backward-char))
  "M-*" ((insert "**") (backward-char))
  "M-/" ((insert "//") (backward-char))
  "M-|" ((insert "||") (backward-char))
  "M-=" ((insert "==") (backward-char))

  "ESC ESC" bz/normal
  "'" bz/insert-snippet)


;;;; Def Keymodes

(defvar bz/keymode nil)
(defmacro bz/defkeymode (name map &optional cursor)
  `(progn
     (defvar ,name nil)
     (setf (alist-get ',name minor-mode-map-alist) ,map)
     (put ',name 'bz/keymode-cursor ',cursor)
     (defun ,name ()
       (interactive)
       (when bz/keymode (set bz/keymode nil))

       (setq ,name t)
       (setq bz/keymode ',name)

       (deactivate-mark)
       (bz/update-cursor))))

(defun bz/update-cursor ()
  (setq cursor-type (or (get bz/keymode 'bz/keymode-cursor) 'box)))

(bz/defkeymode bz/navigate bz/navigate-map)
(bz/defkeymode bz/normal bz/normal-map)
(bz/defkeymode bz/insert bz/insert-map (bar . 1))
(bz/defkeymode bz/nokeys bz/mod-action-map)

;; Selections
(setf (alist-get 'mark-active minor-mode-map-alist) bz/selection-map)
(bz/hook activate-mark-hook bz/visual-mode-setup (setq cursor-type '(bar . 3)))
(bz/hook deactivate-mark-hook bz/update-cursor)


;;; Emacs Maps

(bz/keys special-mode-map
  [remap bz/replace-char] revert-buffer
  [remap bz/q] quit-window)

(bz/keys bz/profiler-map
  :sparse t
  "C-s" (profiler-start 'cpu)
  "C-q" profiler-stop
  "C-r" profiler-report)

(bz/keys ctl-x-map
  "C-p" ,bz/profiler-map
  "C-u" undo-tree-visualize
  "C-r" (@ bz/revert-buffer (revert-buffer t t t))
  "C-m" bz/move-buffer-file

  "c" bz/escape-char-at-point
  "d" toggle-debug-on-error
  "i" toggle-case-fold-search
  "h" bz/insert-color ; [h]ex
  "l" bz/toggle-color-mode
  "n" display-line-numbers-mode
  "s" tab-line-mode
  "t" toggle-truncate-lines
  "u" bz/insert-unicode-char
  )

(bz/keys *
  :full t
  :parent bz/normal-map

  (32 126) self-insert-command

  ;; Make the scroll wheel scroll through time instead of space
  [double-mouse-4] undo-tree-undo
  [double-mouse-5] undo-tree-redo
  [mouse-6] (bz/undo-tree-move-branch -1)
  [mouse-7] (bz/undo-tree-move-branch +1)

  "M-<tab>" other-frame

  "C-x" ,ctl-x-map
  "C-h" ,help-map
  "C-g" keyboard-quit
  "C-u" universal-argument

  "C-h f" helpful-callable
  "C-h v" helpful-variable

  "C-+" (bz/change-face-height 'default +16)
  "C-_" (bz/change-face-height 'default -16)

  "C-r" bz/eval-replace
  "C-e" bz/eval-last-sexp
  "C-d" eval-defun
  "C-s" tab-line-mode

  "RET" newline
  "DEL" delete-backward-char
  "TAB" indent-for-tab-command
  "ESC ESC" bz/normal
  "<insert>" quoted-insert)

(defun bz/change-face-height (face increment)
  (set-face-attribute face nil :height
                      (+ (face-attribute face :height) increment)))


;;; Buffer Keymodes

(defvar bz/last-keymode-window nil)
(defvar bz/last-keymode-buffer nil)
(bz/hook window-state-change-hook bz/update-keymode
  ;; This line makes it work for magit COMMIT_EDITMSGs
  (condition-case error
      (with-current-buffer (window-buffer (selected-window))

        ;; Only update when truly switching windows
        (if (and (eq (selected-window) bz/last-keymode-window)
                 (eq (current-buffer) bz/last-keymode-buffer))
            (bz/update-cursor)

          (when (window-live-p bz/last-keymode-window)
            (with-selected-window bz/last-keymode-window
              (setq-local cursor-type 'hollow)))

          (setq bz/last-keymode-window (selected-window)
                bz/last-keymode-buffer (current-buffer))

          (cond ((minibuffer-window-active-p (selected-window)) (bz/insert))
                ((or (derived-mode-p 'doc-view-mode 'undo-tree-visualizer-mode)
                     (and (derived-mode-p 'vterm-mode) (not vterm-copy-mode)))
                 (bz/nokeys))
                ((derived-mode-p 'magit-mode 'dired-mode 'profiler-report-mode 'Custom-mode 'debugger-mode)
                 (bz/navigate))
                (t (bz/normal)))))
    (error (message "Error in window state change hook: %s" (cadr error)))))


;;; Extra
;;;; Jk normal mode
(bz/hook post-self-insert-hook bz/jk-exit-insert
  (when (looking-back "jk" 2) (delete-backward-char 2) (bz/normal)))



;;;; Fuzzy find file

(defun bz/fuzzy-find-file ()
  (interactive)
  (let* ((default-directory (or (bz/activity-get :path) default-directory "~"))
         (files (->> (directory-files-recursively default-directory "^[^.]")
                     (-map #'abbreviate-file-name)
                     (-map #'file-relative-name)))
         (file (completing-read "Select file: " files)))
    (find-file file)))


;;;; Run key without keymode
(defun bz/run-key-without-keymode (key)
  "Run the command associated with key outside of the keymode."
  (interactive (list (this-command-keys)))

  (let (binding)
    (set bz/keymode nil)
    (setq binding (key-binding key))
    (set bz/keymode t)

    (if binding (call-interactively binding)
      (message "%s is undefined" (key-description key)))))


;;;; Stay on line

(defmacro bz/stay-on-line (&rest exprs)
  "Advises around a function to make sure that it stays on the same line"
  `(let ((pos (point))
         (line-beg (line-beginning-position))
         (line-end (line-end-position)))
     (unwind-protect
         (progn . ,exprs)
       (goto-char (max line-beg (min line-end (point)))))))


;;;; Run for each line
(defmacro bz/run-on-lines (func beg end)
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

;;;; Operators

(defun bz/read-motion ()
  (unless mark-active
    (let* ((same-key-map
            (let ((map (make-sparse-keymap)))
              (define-key map (this-command-keys) 'bz/motion-in-line)
              map))
           (minor-mode-map-alist `((t . ,same-key-map) (t . ,bz/motion-map) . ,minor-mode-map-alist))
           (key (read-key-sequence "" 'continue-echo)))
      (key-binding key))))

(defmacro bz/defoperator (name operator &rest after)
  (declare (indent 2))
  `(defun ,name (motion)
     (interactive (list (bz/read-motion)))
     (save-excursion
       (when motion
         (set-mark (point))
         (call-interactively motion))
       (unwind-protect (,operator (mark) (point)) (deactivate-mark))
       ,@after)))

(bz/defoperator bz/operator-delete   delete-region)
(bz/defoperator bz/operator-change   delete-region (bz/insert))
(bz/defoperator bz/operator-upcase   upcase-region)
(bz/defoperator bz/operator-downcase downcase-region)


;; Highlight copied areas
(setq bz/highlight-yank-time 0.1)
(bz/defoperator bz/operator-copy copy-region-as-kill
  (when bz/highlight-yank-time
    (let ((o (make-overlay (mark) (point))))
      (overlay-put o 'face 'region)
      (run-with-timer bz/highlight-yank-time nil #'delete-overlay o))))


;; Remember the location of the last kill
(defvar bz/last-kill (make-marker))
(bz/defoperator bz/operator-kill kill-region
  (set-marker bz/last-kill (point)))

(defun bz/swap-kills ()
  (interactive)
  (let ((last-kill (car kill-ring)))
    (kill-region (mark) (point))
    (insert last-kill)

    (with-current-buffer (marker-buffer bz/last-kill)
      (save-excursion (goto-char bz/last-kill)
                      (insert (car kill-ring))))))


;;;; Replacing

(defun bz/replace-char (char &optional count)
  (interactive
   (list (read-char "Replace: ")
         (if (numberp current-prefix-arg)
             current-prefix-arg nil)))

  (when (characterp char)
    (delete-char (or count 1))
    (insert (make-string (or count 1) char))))

(defun bz/replace-mode ()
  (interactive)
  (let ((cursor-type 'hbar)
        char)
    (while (characterp (setq char (read-char "Replace: ")))
      (delete-char 1)
      (if (eq char 13) (newline)
        (insert (string char))))))

(defun bz/replace-region (char &optional count)
  (interactive
   (list (read-char "Replace Region: ")
         (if (numberp current-prefix-arg)
             current-prefix-arg nil)))
  (when (< (mark) (point)) (exchange-point-and-mark))
  (bz/replace-char char (- (mark) (point))))


;;;; Pasting

(defun bz/paste-after (arg)
  (interactive "P")
  (if (and arg (not (numberp arg)))
      (bz/paste-data)

    (let ((start (point)))
      (bz/open-below) (bz/normal) (bz/paste arg)
      (ignore-errors (delete-region (point) (search-backward-regexp "\n+\\=" start))))))

(defun bz/paste (arg)
  (interactive "P")
  (if (and arg (not (numberp arg)))
      (bz/paste-data)

    (let ((idx (if (numberp arg) arg 0)))
      (setq this-command 'yank kill-ring-yank-pointer (nthcdr idx kill-ring))
      (yank))))

(defun bz/paste-data ()
  (let* ((case-fold-search nil)
         (targets (or (--filter (and (symbolp it) (string-match-p "[a-z]" (symbol-name it)))
                                (append (gui-get-selection 'CLIPBOARD 'TARGETS) nil))
                      (error "No targets found")))
         (mimetype (intern (completing-read "Type: " targets nil t)))
         ;; (data (gui-get-selection 'CLIPBOARD mimetype))
         (pwd (if buffer-file-name (concat (f-parent buffer-file-name) "/") (or default-directory "~")))
         (filename (read-file-name "Save to: " pwd))
         (file-abbrev (file-relative-name filename pwd)))
    (unless (memq (aref file-abbrev 0) '(?/ ?~ ?.))
      (setq file-abbrev (concat "./" file-abbrev)))

    (when (f-exists-p filename)
      (unless (y-or-n-p (format "File %s exists, overwrite?" file-abbrev))
        (error "File exists")))

    ($$ "xclip -selection clipboard -t %s -o > %s" mimetype (expand-file-name filename))

    (when (s-starts-with-p "image/" (symbol-name mimetype))
      (insert "#+attr_latex: :width 300px\n"))
    (insert (format "[[%s]]" file-abbrev))))


;;;; Linewise Visual Mode

(setq bz/visual-line-mode t)

(defun bz/visual-line-update ()
  (rectangle-mark-mode 0)
  (let ((point-after (> (point) (mark))))
    (remove-overlays (point-min) (point-max) 'bz/visual-line t)
    (unless point-after (exchange-point-and-mark))
    (end-of-visual-line)
    (exchange-point-and-mark)
    (beginning-of-visual-line)
    (when point-after (exchange-point-and-mark))
    (let ((o (make-overlay (max (point) (mark)) (1+ (max (point) (mark))))))
      (overlay-put o 'face 'region)
      (overlay-put o 'bz/visual-line t))))

(defun bz/visual-line-disable ()
  (setq bz/visual-line-mode nil)
  (remove-overlays (point-min) (point-max) 'bz/visual-line t)
  (remove-hook 'post-command-hook 'bz/visual-line-update t)
  (remove-hook 'deactivate-mark-hook 'bz/visual-line-disable t))

(defun bz/visual-line ()
  (interactive)
  (setq bz/visual-line-mode t)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (add-hook 'post-command-hook 'bz/visual-line-update nil t)
  (add-hook 'deactivate-mark-hook 'bz/visual-line-disable nil t))


;;;; Jump to letter

(defun bz/forward-to-letter (c)
  (interactive (list (read-char "Forward to letter: ")))
  (goto-char (1- (search-forward (string c) (line-end-position))))
  (isearch-update-ring (string c) nil))

(defun bz/backward-to-letter (c)
  (interactive (list (read-char "Backward to letter: ")))
  (isearch-update-ring (string c) nil)
  (goto-char (1+ (save-excursion
                   (forward-char -1)
                   (search-backward (string c) (line-beginning-position))))))


;;;; Paren replace

;; If open is a space, just insert a space inside of the parens
(defun bz/paren-replace (open)
  (interactive (list (read-key "Replace With: ")))
  (unless (looking-at-p "[])}>[({<\"'`]")
    (error "Not a bracketed expression"))

  (if (or (eq open ?\\) (eq open ?\)))
      (progn
        (push-mark)
        (forward-sexp)
        (delete-region (mark) (1+ (mark)))
        (delete-region (point) (1- (point)))
        (run-with-timer 0 nil #'activate-mark)
        (when (eq open ?\\) (call-interactively #'bz/insert-snippet)))

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

      (when (eq open 10) (indent-region start end)))))

(defun bz/paren-delete ()
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


;;;; Paren surround

(defun bz/surround (left &optional right)
  (interactive (list (string (read-char "Surround with: "))))
  (unless right (setq right left))

  (unless mark-active (error "Mark is not active."))

  (let ((p (point)) (m (mark)))
    (deactivate-mark)
    (goto-char (max p m)) (insert right)
    (goto-char (min p m)) (insert left)
    (backward-char)))


;;;; Customize newline
(bz/advise :around newline bz/newline-advice (newline &rest args)
  (when (and (not (bobp)) (not (eobp))
             (member (buffer-substring (1- (point)) (1+ (point)))
                     '("()" "[]" "{}" "''" "``" "\"\"" "<>" "><")))
    (save-excursion (apply newline args)))

  (apply newline args))

;;;; Comment next expression

(defun bz/comment-expression ()
  (interactive)
  (or (save-excursion
        (when (looking-back "^[ \t]*")
          (beginning-of-line-text))
        (forward-char (length comment-start))
        (when-let ((start (comment-beginning)))
          (goto-char start)
          (while (comment-forward 1))
          (uncomment-region start (point))))
      (comment-region (point) (save-excursion (forward-sexp) (point)))))


;;;; Shift indent

(defun bz/shift-right ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert (make-string tab-width ?\s)))
  (when (bolp) (forward-char tab-width)))

(defun bz/shift-left ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at (make-string tab-width ?\s))
      (delete-forward-char tab-width))))


;;;; Toggle case

(defun bz/toggle-case ()
  (interactive)
  (if (memq (aref (buffer-substring (point) (1+ (point))) 0) (number-sequence ?A ?Z))
      (downcase-region (point) (1+ (point))) (upcase-region (point) (1+ (point))))
  (forward-char))


;;;; Snippets

;; Each value is an alist of keybinds to snippets
(defvar bz/snippet-mode-alist nil)

(setq bz/snippet-indicator-regexp "<<\\([^<>\n]*?\\)>>")

(defun bz/read-snippet (snippets)
  (let ((map (make-sparse-keymap)) vector)
    (dolist (snip (cons '("'" "'") snippets))
      (define-key map (kbd (car snip)) (vector (cdr snip))))

    (let ((minor-mode-map-alist (cons (cons t map) minor-mode-map-alist)))
      (setq vector (key-binding (read-key-sequence "Snippet: "))))

    (unless (vectorp vector) (error "Invalid snippet"))
    (aref vector 0)))

(defun bz/insert-snippet (snippet)
  (interactive
   (let ((mode-snips (alist-get major-mode bz/snippet-mode-alist)))
     (if mode-snips (bz/read-snippet mode-snips) (list nil))))

  (let ((body (when mark-active
                (prog1 (buffer-substring (point) (mark))
                  (delete-region (point) (mark))
                  (deactivate-mark))))
        match end replacement vars cursor end-marker)

    (when (vectorp snippet) (setq snippet (s-join "\n" (append snippet nil))))

    (cond
     ((stringp snippet)
      (save-excursion (insert snippet) (setq end-marker (point-marker)))
      (while (search-forward-regexp bz/snippet-indicator-regexp (max (point) end-marker) 'noerror)
        (goto-char (match-beginning 0))
        (setq match (match-string 1) end (match-end 0)
              replacement
              (pcase (ignore-errors (read match))
                ((guard (string= match "")) (setq cursor (point-marker)) "")
                ('nil (match-string 0))
                ((and (pred keywordp) var)
                 (or (alist-get var vars)
                     (setf (alist-get var vars) (read-string (format "%s: " var)))))
                (expr (format "%s" (eval expr)))))
        (delete-region (point) end)
        (insert replacement))

      (when cursor
        (goto-char cursor)
        (if (null body) (run-with-timer 0.01 nil #'bz/insert)
          (insert body)
          (bz/normal))))

     ((null snippet) (call-interactively #'self-insert-command))
     ((functionp snippet) (funcall snippet))
     ((listp snippet) (insert (eval snippet)))
     (t (error "Invalid snippet")))))

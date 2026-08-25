;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-buffer-history)
(require 'bz-functions)
(require 'bz-multicursors2)
(require 'bz-search)
(require 'bz-tabline)
(require 'bz-wingroup)

(require 'dash)
(require 'debug)
(require 'f)
(require 's)


;;; Helpers
;;;; Stay on line

(defmacro bz/stay-on-line (&rest exprs)
  "Advises around a function to make sure that it stays on the same line"
  `(let ((line-beg (pos-bol))
         (line-end (pos-eol)))
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
         (funcall #',func)
         (forward-line 1)))))


;;;; Replace regexp

(defun bz/replace-regexp (regexp repl &optional subexp pred)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (when (or (null pred) (save-match-data (funcall pred)))
        (replace-match repl nil nil nil subexp)))))


;;; Special Maps
;;;; Motions

(bz/keys bz/motion-map
  :doc "Vim-like motions keymap."
  :full t
  "i l" (@ bz/motion-in-line (set-mark (pos-bol)) (end-of-line))
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

(defmacro bz/with-lines-selected (&rest body)
  `(progn
     (when (> (point) (mark)) (exchange-point-and-mark))
     (goto-char (pos-bol))
     (exchange-point-and-mark)
     (goto-char (pos-eol))
     (forward-char 1)
     ,@body))

(bz/keys bz/selection-map
  :doc "Keymap enabled when selecting text."
  :sparse t
  :parent bz/motion-map

  "M-q" (@ bz/deactivate-mark (deactivate-mark))
  "q" bz/deactivate-mark

  "o" exchange-point-and-mark

  "D" (@ bz/delete-selected-lines (bz/with-lines-selected (bz/operator-delete nil)))
  "C" (@ bz/change-selected-lines (bz/with-lines-selected (bz/operator-change nil)))
  "S" (@ bz/kill-selected-lines (bz/with-lines-selected (bz/operator-kill nil)))
  "Y" (@ bz/copy-selected-lines (bz/with-lines-selected (bz/operator-copy nil)))

  "C-," mc2-add-previous-match
  "C-." mc2-add-next-match
  "C-<" mc2-add-previous-word-match
  "C->" mc2-add-next-word-match
  "A" mc2-add-lines
  "I" mc2-add-all-matches
  "O" mc2-add-all-word-matches
  "Z" mc2-add-search
  "X" mc2-add-regexp-search

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
  "*" (if (derived-mode-p 'markdown-mode)
          (bz/surround "**" "**")
        (bz/surround "/*" "*/"))
  ":" (bz/surround "*" ":*")
  "\\" (bz/surround "\\( " " \\)")
  "$" (bz/surround "${" "}")
  "`" (cond ((derived-mode-p 'emacs-lisp-mode) (bz/surround "`" "'"))
            ((derived-mode-p 'markdown-mode) (bz/surround "`"))
            (t (bz/surround "`${" "}`")))

  "x" bz/swap-kills)


;;;; Action map

(bz/keys bz/action-map
  :doc "Keymap for global actions."
  :full t
  :prefix ("s-" bz/mod-action-map)

  "h" (@ bz/window-left (bz/windmove 'left))
  "l" (@ bz/window-right (bz/windmove 'right))
  "k" (@ bz/window-up (bz/windmove 'up))
  "j" (@ bz/window-down (bz/windmove 'down))

  "H" (@ bz/split-window-left  (bz/split-window 'left))
  "L" (@ bz/split-window-right (bz/split-window 'right))
  "J" (@ bz/split-window-down  (bz/split-window 'down))
  "K" (@ bz/split-window-up    (bz/split-window 'up))

  "C-h" bz/window-move-left
  "C-l" bz/window-move-right
  "C-k" bz/window-move-up
  "C-j" bz/window-move-down

  "," (@ bz/shrink-window-horizontal (bz/window-resize -8 t))
  "." (@ bz/grow-window-horizontal (bz/window-resize +8 t))
  "<" (@ bz/shrink-window-vertical (bz/window-resize -2 nil))
  ">" (@ bz/grow-window-vertical (bz/window-resize +2 nil))
  "C-," (@ bz/shrink-window-horizontal-big (bz/window-resize -32 t))
  "C-." (@ bz/grow-window-horizontal-big (bz/window-resize +32 t))
  "C-<" (@ bz/shrink-window-vertical-big (bz/window-resize -12 nil))
  "C->" (@ bz/grow-window-vertical-big (bz/window-resize +12 nil))

  "f" find-file
  "F" bz/fuzzy-find-file

  "w" bz/close-window
  "W" bz/kill-current-buffer

  "m" bz/pick-window-pull-buffer
  "M" bz/pull-window

  "b" (@ bz/normal-buffer-back
         (bz/buffer-history-back
          (lambda (buf)
            (not (memq (buffer-local-value 'major-mode buf)
                       '(vterm-mode vterm-copy-mode exwm-mode))))))
  "B" wosp-switch-to-buffer
  "C-B" bz/switch-to-buffer
  "Q" previous-buffer
  "S" wosp-scratch

  "x" execute-extended-command
  "e" bz/eval
  "E" repeat-complex-command
  "r" (defun bz/sync-shell-command (arg cmd)
        (interactive "P\nsSync Command: ")
        (let* ((shell (format "zsh -ic %s" (shell-quote-argument cmd)))
               (out (string-trim (shell-command-to-string shell))))
          (if arg (insert out)
            (kill-new out) (message out))))
  "R" $
  "t" wosp-terminal-open
  "<return>" wosp-terminal-run-action
  "T" (@ bz/term-default (switch-to-buffer (wosp-get-buffer (list 'terminal (plist-get (wosp-get "default") :id) "default"))))
  ;; "T"
  ;; (@ bz/vterm-next (bz/buffer-history-back
  ;;                       (lambda (buf)
  ;;                         (memq (buffer-local-value 'major-mode buf) '(vterm-mode vterm-copy-mode)))))
  )

(defvar bz/resize-window-atom-root t
  "Whether resizing resizes the window atom root or the current window.")

(defun bz/windmove (direction)
  (if bz/resize-window-atom-root
      (wingroup-window-move direction)
    (when-let* ((win (window-in-direction direction nil nil 1)))
      (select-window win))))

(defun bz/window-resize (delta horizontal)
  ;; In case in the minibuffer
  (let ((window-size-fixed nil))
    (window-resize (or (when bz/resize-window-atom-root (window-atom-root))
                       (selected-window))
                   delta horizontal)))

(defun bz/split-window (direction)
  (let ((window-size-fixed nil))
    (select-window (split-window nil nil direction))
    (when (or (derived-mode-p '(exwm-mode term-mode vterm-mode eat-mode))
              (not (bufferp (wingroup-of (current-buffer)))))
      (switch-to-buffer "*scratch*"))))

(defun bz/close-window ()
  (interactive)
  (let ((window-size-fixed nil))
    (let ((remove (selected-window))
          (goto (or (window-next-sibling) (window-prev-sibling))))
      (while (and (windowp goto) (not (window-live-p goto)))
        (setq goto (window-child goto)))
      (select-window goto)
      (delete-window remove))))

(defun bz/kill-current-buffer ()
  (interactive)

  ;; Never prompt the user if modified; just auto-save
  (when (buffer-modified-p)
    (do-auto-save t)
    (set-buffer-modified-p nil))

  ;; Switch to the correct tab
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

  (let* ((bufs (delete-dups (append (cdr bz/buffer-history) (buffer-list))))
         (var (cond (only-files 'buffer-file-name) (dir 'default-directory)))
         (pred `(lambda (buf) (let ((value (buffer-local-value ',var (get-buffer buf))))
                                (and value (string-prefix-p ,dir (expand-file-name value))
                                     (not (string-prefix-p " " buf)))))))
    (switch-to-buffer (completing-read "Buffer: " (mapcar #'buffer-name bufs) (when var pred)))))


;;; Modal Maps
;;;; g Map

(defvar bz/indent-command-alist nil)

(bz/keys bz/g-map
  :doc "Keymap bound to the `g' key."
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

  ;; "m" (@ bz/set-mark (setq bz/mark (point-marker)))
  ;; "M" (@ bz/goto-mark (switch-to-buffer (marker-buffer bz/mark))
  ;;                     (goto-char (marker-position bz/mark)))
  "m" point-to-register
  "M" (@ bz/read-mark (push-mark) (consult-mark))
  "b" bookmark-jump
  "B" bookmark-set

  "n" (@ bz/selection-search-forward (deactivate-mark) (bz/search-forward (buffer-substring (mark) (point))))
  "N" (@ bz/selection-search-backward (deactivate-mark) (bz/search-backward (buffer-substring (mark) (point))))

  "u" bz/operator-downcase
  "U" bz/operator-upcase

  "t" transpose-words
  "T" transpose-sexps

  "/" bz/search-forward-regexp
  "?" bz/search-backward-regexp

  ;; "r" bz/avy-word-in-line
  ;; "r" ()
  "w" (@ bz/elisp-replace-parent
         (let ((child-start (point))
               (child-end (progn (forward-sexp) (point)))
               (parent-start (progn (bz/up-sexp) (point)))
               (parent-end (progn (forward-sexp) (point))))
           (goto-char child-start)
           (delete-region child-end parent-end)
           (delete-region parent-start child-start)))

  "a" (@ bz/select-all (set-mark (point-min)) (goto-char (point-max)))
  "y" (@ bz/copy-buffer (kill-new (buffer-substring-no-properties (point-min) (point-max))))
  "p" consult-yank-from-kill-ring

  "f" fill-paragraph
  ;; "f" bz/fold-hide
  "g" bz/fold-show
  "F" bz/fold-level

  ;; "g" consult-line
  "G" (@ bz/grep (consult-grep (read-directory-name  "Search Location: ")))
  "z" bz/fuzzy-find-file

  "=" (@ bz/format-buffer
         (let ((buf-cmd (alist-get major-mode bz/indent-command-alist)))
           (if buf-cmd (funcall buf-cmd)
             (indent-region (point-min) (point-max))))
         (whitespace-cleanup))
  "+" (@ bz/clean-newlines
         (save-excursion
           (goto-char (point-min))
           (replace-regexp "\n\n\n+" "\n\n"))))


;;;; G Map

(defun bz/goto-col (pct)
  (let ((beg (save-excursion (beginning-of-visual-line) (point)))
        (end (save-excursion (end-of-visual-line) (point))))
    (goto-char (+ beg (round (* (- end beg) pct))))))

(bz/keys bz/G-map
  :doc "Keymap bound to the `G' key."
  :sparse t
  "q" (@ bz/goto-col-q (bz/goto-col 0.090))
  "w" (@ bz/goto-col-w (bz/goto-col 0.181))
  "e" (@ bz/goto-col-e (bz/goto-col 0.272))
  "r" (@ bz/goto-col-r (bz/goto-col 0.363))
  "t" (@ bz/goto-col-t (bz/goto-col 0.454))
  "y" (@ bz/goto-col-y (bz/goto-col 0.545))
  "u" (@ bz/goto-col-u (bz/goto-col 0.636))
  "i" (@ bz/goto-col-i (bz/goto-col 0.727))
  "o" (@ bz/goto-col-o (bz/goto-col 0.818))
  "p" (@ bz/goto-col-p (bz/goto-col 0.909)))


;;;; Navigation

(defvar bz/vertical-motion-info nil
  "Is a list (IS-UP POINT)")

(bz/keys bz/navigate-map
  :doc "Keymap containing navigation commands."
  :parent bz/mod-action-map
  :full t

  (?0 ?9) digit-argument

  "g" ,bz/g-map

  ;; "k" (@ bz/up n (previous-line n))
  ;; "j" (@ bz/down n (next-line n))
  "k" (@ bz/up n (line-move-visual (- (or n 1)))
         (setq bz/vertical-motion-info (list t (point))))
  "j" (@ bz/down n (line-move-visual (or n 1))
         (setq bz/vertical-motion-info (list nil (point))))
  "K" (@ bz/up4 (bz/up 4))
  "J" (@ bz/down4 (bz/down 4))

  "h" (@ bz/left n (bz/stay-on-line (backward-char n)))
  "l" (@ bz/right n (bz/stay-on-line (forward-char n)))
  "H" (@ bz/left4 (bz/left 4))
  "L" (@ bz/right4 (bz/right 4))

  "y" bz/operator-copy

  "e" (@ bz/forward-word
         (bz/stay-on-line
          (if (looking-at "[ \t]\\{2,\\}") (goto-char (match-end 0))
            (forward-word))))
  "b" (@ bz/backward-word
         (let* ((syntax (syntax-ppss))
                (str-start (when (nth 3 syntax) (nth 8 syntax))))
           (bz/stay-on-line
            (if (looking-back "[ \t]\\{2,\\}" (pos-bol) t) (goto-char (match-beginning 0))
              (backward-word))
            ;; Use the start of a string as a minimum distance to travel
            (and str-start (< (point) str-start) (goto-char str-start)))))
  "w" (@ bz/forward-to-word (bz/stay-on-line (forward-to-word 1)))

  "E" forward-sexp
  "B" backward-sexp
  "W" (@ bz/up-sexp
         (if (eq (face-at-point) 'font-lock-string-face)
             (and (search-backward-regexp "[^\\][\"']" nil t) (forward-char))
           (let ((pos (point)))
             (while (ignore-errors (or (backward-sexp) (not (bobp)))))
             (if (bobp) (goto-char pos) (search-backward-regexp "[({[]" nil t)))))

  "v" set-mark-command
  "V" bz/visual-line
  "i" bz/insert

  ;; Get rid of debugger for searches
  "/" bz/search-forward
  "?" bz/search-backward
  "n" bz/search-repeat-forward
  "N" bz/search-repeat-backward)


;;;; Normal

(bz/keys bz/normal-map
  :doc "Default keymap for normal mode."
  :sparse t
  :parent bz/navigate-map
  :prefix ("M-" bz/mod-normal-map)

  "q" (@ bz/q bz/run-key-without-keymod)
  "Q" (@ bz/Q bz/run-key-without-keymod)
  "RET" (@ bz/click bz/run-key-without-keymode)
  "SPC" (@ bz/fold-toggle bz/run-key-without-keymode)
  "F" (@ bz/fold-toggle bz/run-key-without-keymode)
  "S-SPC" (@ bz/fold-toggle-small bz/run-key-without-keymode)
  "C-SPC" (@ bz/fold-toggle-all bz/run-key-without-keymode)
  "=" (@ bz/comment-header
         (goto-char (pos-bol))
         (when (looking-at (concat (regexp-quote comment-start) "\s*=*\s*")) (delete-region (point) (match-end 0)))
         (insert comment-start "===== ")
         (goto-char (pos-eol))
         (when (looking-back "[^\s=]\s*=+" (pos-bol)) (delete-region (1+ (match-beginning 0)) (point)))
         (save-excursion (insert " ") (insert (make-string (max 0 (- 60 (current-column))) ?=)))
         (bz/insert))

  "f" (@ bz/wosp-dashboard-open-or-unbind
         (call-interactively (if current-prefix-arg #'wosp-dashboard-unbind #'wosp-dashboard-open)))
  "F" wosp-dashboard-assign
  ;; "G" ,bz/G-map
  "G" goto-line

  "{" (@ bz/up-page (forward-line (- (window-height))))
  "}" (@ bz/down-page (forward-line (window-height)))
  "[" bz/surround-with-parens
  "]" bz/insert-close-paren-after

  "i" bz/insert
  "a" (@ bz/insert-after-char (unless (eolp) (forward-char)) (bz/insert))
  "I" (@ bz/insert-beginning-of-line
         (beginning-of-line-text) (bz/insert))
  "A" (@ bz/insert-end (end-of-line) (bz/insert))
  "o" (@ bz/open-below (end-of-visible-line) (insert "\n") (bz/insert))
  "O" (@ bz/open-above (beginning-of-line) (insert "\n") (forward-line -1) (bz/insert))

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

  "m" point-to-register
  "`" jump-to-register
  "M" (@ bz/merge-lines (forward-line) (join-line))
  "<" pop-to-mark-command

  "t" bz/forward-to-letter
  "T" bz/backward-to-letter

  "u" undo
  "U" undo-redo

  ";" (@ bz/comment-line (save-excursion (comment-line 1)))
  ":" bz/comment-expression

  "." bz/shift-right
  "," bz/shift-left

  "(" bz/paren-replace
  ")" bz/paren-delete

  "~" bz/toggle-case
  "$" (@ bz/insert-last-variable (insert (format "$%s" (1- bz/eval-variable-number))))
  "'" bz/insert-snippet
  "\"" (@ bz/insert-snippet-around
          (push-mark)
          (forward-sexp)
          (run-with-timer 0 nil #'activate-mark)
          (call-interactively #'bz/insert-snippet)))


;;;; Insert

(bz/keys bz/insert-map
  :doc "Keymap enabled in insert mode."
  :sparse t
  :parent bz/mod-normal-map

  "S-SPC" (insert " ")
  ;; "M-<tab>" company-complete

  "M-q" bz/normal
  "ESC ESC" bz/normal

  "M-C-l" end-of-line
  "M-C-h" beginning-of-line
  "M-C-j" end-of-buffer
  "M-C-k" beginning-of-buffer

  "M-9" (@ bz/surround-with-parens (insert "(") (save-excursion (forward-sexp) (insert ")")))
  "M-0" (@ bz/insert-close-paren-after (save-excursion (forward-sexp) (insert ")")))

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
  "M-=" (@ bz/alt-=
           (if (derived-mode-p 'org-mode)
               (progn (insert "==") (backward-char))
             (bz/comment-header)))

  "'" bz/insert-snippet)


;;;; Def Keymodes

(defvar bz/keymode nil)
(defvar bz/keymode-change-hook nil)

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

       (with-demoted-errors "Error in keymode hook: %s"
         (run-hook-with-args 'bz/keymode-change-hook))

       (deactivate-mark)
       (bz/update-cursor))))

(defun bz/update-cursor ()
  (setq cursor-type (or (get bz/keymode 'bz/keymode-cursor) 'box)))

(bz/defkeymode bz/navigate bz/navigate-map)
(bz/defkeymode bz/normal bz/normal-map)
(bz/defkeymode bz/insert bz/insert-map (bar . 1))
(bz/defkeymode bz/nokeys bz/mod-action-map)


;; Selections
(setf (alist-get 'mark-active minor-mode-map-alist nil t) bz/selection-map)
(bz/hook activate-mark-hook bz/visual-mode-setup (setq cursor-type '(bar . 3)))
(bz/hook deactivate-mark-hook bz/update-cursor)


;;; Global Maps
;;;; Miscellaneous

(bz/keys special-mode-map
  [remap bz/replace-char] revert-buffer
  [remap bz/q] quit-window)

(bz/keys debugger-mode-map
  [remap bz/q] debugger-quit
  "C-e" debugger-eval-expression)

(bz/keys bz/profiler-map
  :doc "Keymap for profiler commands."
  :sparse t
  "C-c" (profiler-start 'cpu)
  "C-m" (profiler-start 'mem)
  "C-q" profiler-stop
  "C-r" profiler-report

  ;; Etrace (flamegraph profiling)
  "C-s" etrace-clear ; start
  "C-f" etrace-write ; finish
  "C-i" elp-instrument-package ; instrument
  "C-u" elp-restore-all ; uninstrument

  ;; Tracing (show call stack)
  "C-d" (@ bz/trace-defun trace-function (call-interactively 'eval-defun))
  "C-t" (@ bz/trace-package
           (let* ((prefix (read-string "Prefix (blank to remove all): "))
                  (count 0))
             (mapatoms (lambda (sym)
                         (if (string-empty-p prefix) (untrace-function sym)
                           (and (fboundp sym) (string-prefix-p prefix (symbol-name sym))
                                (cl-callf 1+ count) (trace-function-background sym))))))))


;;;; Multi cursors

(bz/keys mc2-minor-mode-map
  :doc "Multicursors map."
  :sparse t
  "C-f" mc2-all ; F for Forall? idk man
  "C-q" mc2-disable
  "C-o" mc2-one
  "C-s" mc2-none

  "C-d" mc2-delete-cursor
  "C-a" mc2-add-cursor
  "C-w" mc2-add-next-word

  "C-," mc2-add-previous-match
  "C-." mc2-add-next-match
  "C-<" mc2-add-previous-word-match
  "C->" mc2-add-next-word-match
  "C-M-," mc2-add-previous-skip
  "C-M-." mc2-add-next-skip
  "C-M-<" mc2-add-previous-word-skip
  "C-M->" mc2-add-next-word-skip

  "C-j" mc2-forward-cursor
  "C-k" mc2-backward-cursor
  [remap flymake-goto-next-error] mc2-forward-cursor
  [remap flymake-goto-prev-error] mc2-backward-cursor
  [remap flycheck-next-error] mc2-forward-cursor
  [remap flycheck-previous-error] mc2-backward-cursor)

(bz/keys mc2-prefix-map
  :doc "Multicursors prefix map."
  :sparse t
  :parent mc2-minor-mode-map
  "C-v" mc2-all
  "C-x" mc2-add-all-matches
  "C-c" mc2-condensed-mode
  "C-r" mc2-insert-range
  "C-l" mc2-remember-case
  "C-S-l" mc2-forget-case
  "C-SPC" mc2-align-cursors
  "<C-return>" mc2-add-next-line)

(bz/key * "C-v" ,mc2-prefix-map)

(push (cons 'mc2-mode mc2-minor-mode-map)
      minor-mode-map-alist)


;;;; Cyborg

(bz/keys bz/cyborg-map
  :doc "Keymap for communicating with an llm"
  :sparse t
  "RET" gptel-send
  "C-c" cyborg-correct-typos
  "C-r" cyborg-replace
  "C-e" cyborg-fix-flycheck-error
  "y" cyborg-copy-file-with-filename)


;;;; Control-X

(bz/keys ctl-x-map
  "C-p" ,bz/profiler-map
  "C-u" undo-tree-visualize
  "C-r" (@ bz/revert-buffer
           (unless buffer-file-name (error "Not a file"))
           (let ((inhibit-read-only t))
             (bz/save-position
              (clear-visited-file-modtime)
              (erase-buffer)
              (insert-file-contents (buffer-file-name))
              (set-buffer-modified-p nil))))
  "C-m" bz/move-buffer-file
  "C-x" execute-extended-command
  "C-e" (@ bz/debug-eval-buffer (let ((debug-on-error t)) (eval-buffer)))

  "C-l" bz/load
  "C-;" (@ bz/load-all
           (bz/load 'javascript)
           (bz/load 'nixos)
           (bz/load 'undotree)
           (bz/load 'eat)
           (bz/load 'outline)
           (bz/load 'hideshow)
           (bz/load 'flycheck)
           (bz/load 'eglot)
           (bz/load 'company)
           (bz/load 'org)
           (bz/load 'modeline)
           (bz/load 'markdown))

  "C-," (@ bz/toggle-resize-mode
           (cl-callf not bz/resize-window-atom-root)
           (if bz/resize-window-atom-root
               (message "Resizing from root")
             (message "Resizing individual windows")))

  "g" gemini-open-session
  "C-g" gemini-start-session

  "TAB" minuet-show-suggestion

  "c" bz/escape-char-at-point
  "d" toggle-debug-on-error
  "D" toggle-debug-on-quit
  "i" toggle-case-fold-search
  "h" bz/insert-color ; [h]ex
  "l" bz/switch-theme
  "o" bz/toggle-transparent
  "n" display-line-numbers-mode
  "s" tab-line-mode
  "t" toggle-truncate-lines
  "u" bz/insert-unicode-char
  "a" copilot-mode
  )


;;;; Global

(bz/keys *
  :full t
  :parent bz/normal-map

  (32 126) self-insert-command

  ;; Make the scroll wheel scroll through time instead of space
  [double-mouse-4] undo-tree-undo
  [double-mouse-5] undo-tree-redo
  [mouse-6] (bz/undo-tree-move-branch -1)
  [mouse-7] (bz/undo-tree-move-branch +1)

  "s-q" keyboard-quit

  "C-v" ,mc2-prefix-map
  "C-x" ,ctl-x-map
  "C-t" ,bz/profiler-map ; trace
  "C-h" ,help-map
  "C-z" ,bz/cyborg-map

  "C-f" forward-char
  "C-b" backward-char
  "C-n" next-line
  "C-p" previous-line
  "C-g" keyboard-quit
  "C-u" universal-argument

  "M-|" shell-command-on-region

  "C-h f" helpful-callable
  "C-h v" helpful-variable
  "C-h i" (@ bz/info-page
             (let* ((fn #'(lambda (file)
                            (--map (format "(%s) %s" file it)
                                   (-uniq (-filter #'stringp (flatten-list (Info-toc-nodes file)))))))
                    (nodes (apply #'nconc (mapcar fn '("emacs" "elisp")))))
               (Info-goto-node (completing-read "Info: " nodes))))
  "C-h =" describe-char

  "C-+" (bz/change-face-height 'default +16)
  "C-_" (bz/change-face-height 'default -16)

  "C-r" bz/eval-replace
  "C-e" bz/eval-last-sexp
  "C-d" eval-defun
  "C-s" tab-line-mode

  "RET" (@ bz/newline
           (when (ignore-errors
                   (save-excursion
                     (backward-char)
                     (looking-at-p "()\\|\\[]\\|{}\\|''\\|``\\|\"\"\\|<>\\|><")))
             (save-excursion (insert "\n") (indent-for-tab-command)))
           (insert "\n")
           (unless (derived-mode-p 'comint-mode)
             (indent-for-tab-command)))
  "DEL" delete-backward-char
  "TAB" indent-for-tab-command
  "ESC ESC" bz/normal
  "<insert>" quoted-insert
  "<up>" bz/up
  "<down>" bz/down
  "<left>" bz/left
  "<right>" bz/right
  )

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
                     (and (derived-mode-p 'vterm-mode) (not (bound-and-true-p vterm-copy-mode))))
                 (bz/nokeys))
                ((derived-mode-p 'magit-mode 'profiler-report-mode 'Custom-mode 'debugger-mode)
                 (bz/navigate))
                ((derived-mode-p 'dired-mode)
                 (if (bound-and-true-p dired-hide-details-mode)
                     (bz/nokeys) (bz/normal)))
                (t (bz/normal)))))
    (error (message "Error in window state change hook: %s" (cadr error)))))


;;; Extra
;;;; Override mode

(define-minor-mode bz/key-override-mode
  "Override all buffer local keys with normal mode keys."
  :global nil
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (if bz/key-override-mode
        (buffer-swap-properties 'keymap 'bz/saved-keymap)
      (buffer-swap-properties 'bz/saved-keymap 'keymap))))

(defun buffer-swap-properties (from to)
  ;; Handle overlay properties
  (dolist (ov (overlays-in (point-min) (point-max)))
    (let ((val (overlay-get ov from)))
      (when val
        (when to (overlay-put ov to val))
        (overlay-put ov from nil))))
  ;; Handle text properties
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let* ((next (next-single-property-change pos from nil (point-max)))
             (val (get-text-property pos from)))
        (when val
          (when to (put-text-property pos next to val))
          (remove-text-properties pos next (list from nil)))
        (setq pos next)))))


;;;; Jk normal mode

(bz/hook post-self-insert-hook bz/jk-exit-insert
  (when (looking-back "jk" 2) (delete-char -2) (bz/normal)))


;;;; Elisp format

;; Add an elisp mode indent command
(setf (alist-get 'emacs-lisp-mode bz/indent-command-alist) #'bz/elisp-format-buffer)
(defun bz/elisp-format-buffer ()
  (save-excursion
    ;; Add provide+lexical binding for modules
    (when (and buffer-file-name (file-in-directory-p buffer-file-name "~/.emacs.d/modules"))
      (let ((provide-str (format "(provide 'bz-%s)" (file-name-base buffer-file-name))))
        (elisp-enable-lexical-binding)
        (goto-char (point-max))
        (unless (search-backward provide-str nil t)
          (insert ";;; Provide\n\n" provide-str))))

    ;; Organize imports
    (let (start str bz-requires requires all-str)
      (goto-char (point-min))
      (re-search-forward "\\=\\(\n\\|;;.*\n\\)*;;.*\n" nil t)
      (setq start (point))
      (while (re-search-forward "\\=\\(?:(require '\\([a-zA-Z0-9-_]+\\))\\)?\n" nil t)
        (when (setq str (match-string 1))
          (if (s-starts-with-p "bz-" str) (push str bz-requires) (push str requires))))
      (when (or requires bz-requires)
        (setq requires (sort requires) bz-requires (sort bz-requires))
        (setq all-str (concat (if (eq start (point-min)) "" "\n")
                              (s-join "\n" (--map (when it (format "(require '%s)" it))
                                                  (append bz-requires (when (and requires bz-requires) '(nil)) requires)))
                              "\n\n\n"))
        (unless (equal (buffer-substring-no-properties start (point)) all-str)
          (delete-region start (point))
          (insert all-str))))

    ;; Adjust spacing
    (goto-char (point-min))
    ;; Too much space before heading
    (bz/replace-regexp "\n\n\n\n+;;;" "\n\n\n;;;")
    ;; Not enough space before heading
    (bz/replace-regexp ".\\(\n\n?\\);;;" "\n\n\n" 1
                       (lambda () (not (or (looking-back ";;;.*\n\n?;;;" (pos-bol 0))
                                           (looking-back "(provide '.*)\n;;;" (pos-bol 0))))))
    ;; Too much space after heading
    (bz/replace-regexp "^\\(;;;.*\\)\n\n\n+" "\\1\n\n")
    ;; Not enough space after heading
    (bz/replace-regexp "^\\(;;;.*\\)\n\\([^\n;]\\)" "\\1\n\n\\2")
    ;; Too much space between headings
    (bz/replace-regexp "^\\(;;;.*\\)\n\n+;;;" "\\1\n;;;"))
  (indent-region (point-min) (point-max)))


;;;; Fuzzy find file

(defun bz/fuzzy-find-file ()
  (interactive)
  (let* ((default-directory
          (or (dired-get-filename nil 'no-error-if-not-filep)
              dired-directory
              (when (fboundp 'wosp-get) (plist-get (wosp-get nil t) :root))
              default-directory "~"))
         (files (->> (directory-files-recursively default-directory "^[^.]" nil
                                                  (lambda (name)
                                                    (not (or (s-ends-with-p "/node_modules" name)
                                                             (s-ends-with-p "/.git" name)))))
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

    (if (and binding (not (eq binding #'self-insert-command)))
        (call-interactively binding)
      (message "%s is undefined" (key-description key)))))


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
(bz/defoperator bz/operator-change   (lambda (m p) (delete-region m p) (unless (eq m p) (bz/insert))))
(bz/defoperator bz/operator-upcase   upcase-region)
(bz/defoperator bz/operator-downcase downcase-region)


;; Highlight copied areas
(defvar bz/highlight-yank-time 0.1)
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

(defun bz/replace-region (char)
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
         (pwd (if buffer-file-name (concat (f-parent buffer-file-name) "/")
                (or default-directory "~")))
         (read-root (if (ignore-errors (file-in-directory-p buffer-file-name wosp-scratch-directory))
                        (file-name-as-directory (plist-get (wosp-get (cadr (wosp-get-descriptor))) :root))
                      pwd))
         (filename (read-file-name "Save to: " read-root))
         (file-abbrev (file-relative-name filename pwd)))
    (unless (memq (aref file-abbrev 0) '(?/ ?~ ?.))
      (setq file-abbrev (concat "./" file-abbrev)))

    (let ((dir (f-parent filename))) (mkdir dir t))
    (when (f-exists-p filename)
      (unless (y-or-n-p (format "File %s exists, overwrite?" file-abbrev))
        (error "File exists")))

    ($$ "xclip -selection clipboard -t %s -o > %s" mimetype (expand-file-name filename))

    (when (s-starts-with-p "image/" (symbol-name mimetype))
      (insert "#+attr_latex: :width 300px\n"))
    (insert (format "[[%s]]" file-abbrev))))


;;;; Linewise Visual Mode

(defvar bz/visual-line-mode t)

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
  "Forward to the next non-composed letter."
  (interactive (list (read-char "Forward to letter: ")))
  (while
      (progn (goto-char (1- (search-forward-regexp (concat "." (regexp-quote (string c))) (pos-eol))))
             (get-text-property (point) 'composition)))
  (isearch-update-ring (string c) nil))

(defun bz/backward-to-letter (c)
  (interactive (list (read-char "Backward to letter: ")))
  (isearch-update-ring (string c) nil)
  (while
      (progn (goto-char (1+ (save-excursion
                              (forward-char -1)
                              (search-backward (string c) (pos-bol)))))
             (get-text-property (point) 'composition))))


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
                  ;; Check if in string
                  (and (looking-at-p "['\"`]") (nth 3 (syntax-ppss))))
          (forward-char) (backward-sexp))
        (save-excursion
          (setq start (point))
          (forward-sexp)
          (when backspace (backward-char))
          (if noreplace (backward-char) (delete-char -1))
          (unless backspace (insert (string close)))
          (setq end (+ 2 (point))))
        (when backspace (forward-char))
        (if noreplace (forward-char) (delete-char 1))
        (unless backspace (insert (string open)))
        (backward-char))

      (when (eq open 10) (indent-region start end)))))

(defun bz/paren-delete ()
  (interactive)
  (unless (looking-at-p "[])}>[({<\"']")
    (error "Not a bracketed expression"))
  (save-excursion
    (when (or (looking-at-p "[])}>]")
              (and (looking-at-p "['\"]") (nth 3 (syntax-ppss))))
      (forward-char) (backward-sexp))
    (save-excursion (forward-sexp) (delete-char -1)
                    (beginning-of-line-text)
                    (when (eolp) (delete-region (line-beginning-position) (1+ (point)))))
    (delete-char 1)))


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


;;;; Comment next expression

(defun bz/comment-expression ()
  (interactive)
  (or (save-excursion
        (when (looking-back "^[ \t]*" (pos-bol))
          (beginning-of-line-text))
        (forward-char (length comment-start))
        (when-let ((start (comment-beginning)))
          (goto-char start)
          (while (comment-forward 1))
          (uncomment-region start (point))))
      (comment-region (point) (save-excursion (forward-sexp) (point)))))


;;;; Shift indent

(defun bz/shift-tab-width ()
  (if (derived-mode-p 'org-mode 'markdown-mode) 2
    tab-width))

(defun bz/shift-right ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert (make-string (bz/shift-tab-width) ?\s)))
  (when (bolp) (forward-char (bz/shift-tab-width))))

(defun bz/shift-left ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at (make-string (bz/shift-tab-width) ?\s))
      (delete-char (bz/shift-tab-width)))))


;;;; Toggle case

(defun bz/toggle-case ()
  (interactive)
  (if (memq (aref (buffer-substring (point) (1+ (point))) 0) (number-sequence ?A ?Z))
      (downcase-region (point) (1+ (point))) (upcase-region (point) (1+ (point))))
  (forward-char))


;;;; Snippets

;; (MODE | MODE[] . [KEY SNIP][])[]
(defvar bz/snippet-mode-alist nil)
(defvar bz/snippet-indicator-regexp "<<\\([^<>\n]*?\\)>>")

(defun bz/read-snippet (snippets)
  (if (null snippets) (list nil)
    (let ((map (make-sparse-keymap)) vector binding)
      (dolist (snip (cons '("'" "'") snippets))
        (define-key map (kbd (car snip)) (vector (cdr snip))))

      (let ((minor-mode-map-alist (cons (cons t map) minor-mode-map-alist)))
        (setq vector (key-binding (read-key-sequence "Snippet: "))))

      (unless (vectorp vector) (error "Invalid snippet"))
      (setq binding (aref vector 0))
      (if (listp binding) binding (list binding)))))

(defun bz/insert-snippet (snippet)
  (interactive
   (bz/read-snippet
    (-flatten-n
     1
     (-map #'cdr (--filter (if (listp (car it)) (memq major-mode (car it)) (eq major-mode (car it)))
                           bz/snippet-mode-alist)))))

  (let ((body (when mark-active
                (prog1 (buffer-substring (point) (mark))
                  (delete-region (point) (mark))
                  (deactivate-mark))))
        match end replacement vars cursor end-marker)

    (when (vectorp snippet) (setq snippet (string-join (append snippet nil) "\n")))

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


;;; Provide

(provide 'bz-keys)

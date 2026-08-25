;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'benchmark)
(require 'dash)
(require 'helpful)
(require 'nadvice)


;;; Buffer string no properties

(defun bz/buffer-string (&optional buf)
  (if (null buf) (buffer-substring-no-properties (point-min) (point-max))
    (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))))


;;; Shorthand lambda syntax

(defmacro @ (&rest exprs)
  (list 'lambda '(&rest @*)
        (cons 'let (cons '((@1 (car @*)) (@2 (cadr @*))) exprs))))

(defmacro @0 (&rest exprs) (cons 'lambda (cons () exprs)))
(defmacro @1 (&rest exprs) (cons 'lambda (cons '(@1) exprs)))
(defmacro @2 (&rest exprs) (cons 'lambda (cons '(@1 @2) exprs)))


;;; Modify in place

(defmacro => (var func &rest args)
  `(setf ,var (,func ,var . ,args)))


;;; Special Eval

(defvar $0 nil "The result of the most recent evaluation")

(defvar bz/eval-variable-number 1)
(defun bz/eval (string &optional lexical)
  (interactive (list (read-string "Eval: ") t))

  (setq string (string-trim string))

  (let* ((print-length nil) (print-level nil)
         (is-math (string-match-p "\\`[0-9{]" string))
         (expr (unless is-math (read string)))
         (time (benchmark-elapse
                 (if is-math (setq $0 (bz/math string))
                   (setq $0 (eval expr lexical)))))
         (var-str (format "$%s" bz/eval-variable-number)))

    (set (intern var-str) $0)
    (put (intern var-str) 'variable-documentation
         (if is-math string (prin1-to-string expr)))
    (setq bz/eval-variable-number (1+ bz/eval-variable-number))

    (message
     "%s %s %s %s%s"
     (propertize (format "(%ss)" time) 'face 'shadow)
     (propertize var-str 'face 'help-key-binding)
     (propertize "=" 'face 'bold)
     (cl-prin1-to-string $0)
     ;; If the expression was a local variable, display the global value next to it
     (if (not (and expr (symbolp expr) (local-variable-p expr))) ""
       (format "   %s = %s" (propertize "global" 'face 'help-key-binding)
               (default-value expr))))))



;; (bz/advise :around helpful--variable-p bz/exclude-dollar-variables (func sym)
;;   (let ((name (symbol-name sym)))
;;     (unless (or (string= name "") (eq (aref name 0) ?$)) (funcall func sym))))


;;; Amazing Loop

(defmacro for (vars iter &rest exprs)
  "A special macro for looping.

vars ::= var | (var1 var2 ...)
var ::= element | element:index
iter ::= end | (start end) | (start end inc) | list
start, end, inc ::= integer

exprs can start with a plist, containing the following properties:
:beg - Skip the first N elements of the sequence.
:end - Only go up to
"

  (let ((var-ct (if (listp vars) (length vars) 1))
        (count 0) params var-split var-order var-exprs)

    ;; Parse the beginning of `exprs` as a plist into params
    (while (and (keywordp (car exprs)) (>= (length exprs) 2))
      (setq params (cons (pop exprs) (cons (pop exprs) params))))

    ;; If iter is a list of numbers, make it a number sequence
    (when (and (listp iter) (numberp (car iter)))
      (push 'number-sequence iter))

    ;; Parse the `vars` input into the fancy `var-order` list
    ;; `var-order` is a list of elements of the form (INCREMENT ELEMENT-VAR . INDEX-VAR)
    (unless (listp vars) (setq vars (list vars)))
    (while vars
      (when (numberp (car vars)) (setq count (pop vars)))
      (setq var-split (mapcar 'intern (split-string (format "%s" (pop vars)) ":" t)))
      (when (> (length var-split) 0) (push (list (nth 0 var-split) count nil) var-order))
      (when (> (length var-split) 1) (push (list (nth 1 var-split) count t  ) var-order))
      (setq count (1+ count)))

    ;; Create the literal let forms
    (setq var-exprs
          (--map (if (caddr it) ; If it is an index variable
                     `(,(car it) (+ =index= ,(cadr it)))
                   (if (>= (cadr it) 0) ; If it is being incremented
                       `(,(car it) (ignore-errors (elt =iter= (+ =index= ,(cadr it)))))
                     `(,(car it) (ignore-errors (elt (cons nil =iter=) (+ 1 =index= ,(cadr it)))))))
                 var-order))

    `(let* ((=params= ',params)
            (=iter= (--> ,iter (if (numberp it) (number-sequence 0 (1- it)) it)))
            (=index= (or ,(plist-get params :beg) 0))
            (=inc= (or ,(plist-get params :inc) ,var-ct))
            (=end= (or (--> ,(plist-get params :end)
                            (and it (+ it (if (>= it 0) 0 (length =iter=)))))
                       (--> ,(plist-get params :reps)
                            (and it (+ =index= (* it =inc=))))
                       (length =iter=)))
            (=continue= t)
            (=out= nil))

       (while (< =index= =end=)
         (let ,(reverse var-exprs)
           (push (progn ,@exprs) =out=)
           (setq =index= (+ =index= =inc=))))

       ;; Convert iter to a number sequence if necessary

       (reverse =out=))))


;;; Indexing

(defmacro indejas (expr)
  (if-let* ((str (and (symbolp expr) (symbol-name expr)))
            (start (and (string= "}" (substring str -1)) (string-match "{" str)))
            (var (or (eq start 0) (intern (substring str 0 start))))
            (index (substring str (1+ start) -1)))
      (if (eq start 0)
          `(math-eval ,index)
        `(nth (math-eval ,index) ,var))
    (if (not (listp expr)) expr
      (--map (macroexpand-1 `(indejas ,it)) expr))))


;;; Wait for Input

(defmacro bz/wait (&rest vars)
  `(progn
     (redraw-display)
     (read-char (mapconcat
                 (lambda (var) (format "%s: %s" var (eval var)))
                 ',vars "\n"))))


;;; Avg

(defun avg (&rest list) (/ (apply #'+ list) (float (length list))))


;;; Read Unicode Chars

(defvar bz/unicode-chars nil)

(defun bz/insert-unicode-char ()
  (interactive)
  ;; Initialize the list of unicode chars if it isn't already
  (unless bz/unicode-chars
    (maphash (lambda (name num)
               (push (format "%s\t %s" (string num) name)
                     bz/unicode-chars))
             (ucs-names))
    (setq bz/unicode-chars (reverse bz/unicode-chars)))

  (let ((result (completing-read "Unicode Char: " bz/unicode-chars)))
    (insert (if (eq (aref result 0) ?\t) "\t"
              (car (split-string result "\t"))))))


;;; Read Fonts

(defun bz/read-font ()
  (interactive)
  (let* ((ex (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     "abcdefghijklmnopqrstuvwxyz"
                     "0123456789"
                     ",.;:?!@#$%^&*~_-=+()[]{}<>\"'`/|\\"))
         (space (propertize "\t" 'display '(space :align-to 50)))
         (font (car (split-string
                     (completing-read
                      "Font Family: "
                      (--map (format "%s%s%s" it space (propertize ex 'face (list :family it)))
                             (font-family-list)))
                     "\t"))))
    (when (called-interactively-p t) (insert font))))


(require 'face-remap)

(defun bz/read-font-for-face (face)
  "Like `bz/read-font' but also live-previews and permanently sets FACE's :family.

This must be run in the buffer you want to preview the face for!"
  (interactive (list (read-face-name "Face: ")))
  (let* ((target-buffer (current-buffer))
         (remap-cookie nil)
         (last-font nil)
         (preview-hook
          (lambda ()
            (when (and (minibufferp)
                       (boundp 'vertico--index)
                       (boundp 'vertico--candidates)
                       (>= vertico--index 0))
              (when-let* ((candidate (nth vertico--index vertico--candidates))
                          (font (car (split-string candidate "\t"))))
                (unless (equal font last-font)
                  (setq last-font font)
                  (with-current-buffer target-buffer
                    (when remap-cookie
                      (face-remap-remove-relative remap-cookie))
                    (setq remap-cookie
                          (face-remap-add-relative face :family font)))))))))
    (unwind-protect
        (progn
          (add-hook 'post-command-hook preview-hook)
          (bz/read-font))
      (remove-hook 'post-command-hook preview-hook)
      (with-current-buffer target-buffer
        (when remap-cookie
          (face-remap-remove-relative remap-cookie)))
      ;; commit: last-font is whatever was highlighted when RET was pressed
      (when last-font (insert last-font)))))


;;; Base converter

(defun bz/decimal-to (base num)
  (apply '+ (mapcar (lambda (p) (* (expt 10 p) (% (/ num (expt base p)) base)))
                    (number-sequence 0 (if (eq num 0) 0 (floor (log num base)))))))

(defvar bz/base-chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defun bz/decimal-to-str (base num)
  (mapconcat (lambda (p) (string (aref bz/base-chars (% (/ num (expt base p)) base))))
             (reverse (number-sequence 0 (if (eq num 0) 0 (floor (log num base))))) ""))


;;; Prime factors

(defun bz/superscript-number (num)
  (apply 'string
         (--map (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" (- it ?0))
                (string-to-list (number-to-string num)))))

(defun bz/primes (num &optional fmt-base fmt-expt)
  (if (eq num 0) "0"
    (mapconcat
     (lambda (a) (format "%s%s"
                         (funcall (or fmt-base #'identity) (car a))
                         (if (= (cdr a) 1 ) ""
                           (propertize
                            (funcall (or fmt-expt #'bz/superscript-number) (cdr a))
                            'display (when fmt-expt '((raise 0.25) (height 0.75)))))))
     (bz/prime-factors-1 num 2) "×")))

(defun bz/prime-factors-1 (num factor)
  (unless (= num 1)
    (let (answer)
      (while (null answer)
        (if (not (= 0 (% num factor)))
            (setq factor (1+ factor))

          (setq answer (bz/prime-factors-1 (/ num factor) factor))
          (if (eq (caar answer) factor)
              (setcdr (car answer) (1+ (cdar answer)))
            (push (cons factor 1) answer))))

      answer)))


;;; Linux Executable

(defvar bz/executables nil)
(defvar bz/apps)

(defun bz/get-executables ()
  (or bz/executables
      (setq bz/executables
            (->> (split-string (getenv "PATH") ":")
                 (seq-filter #'file-directory-p)
                 (mapcar #'directory-files)
                 (flatten-list)
                 (remove "." )
                 (remove "..")))))

(defun bz/app (prefix)
  (interactive "P")
  (let ((cmd (completing-read "Run: " (bz/get-executables))))
    ($ (if prefix (concat "sudo " cmd) cmd))))

;; From counsel-linux-app: look through .desktop files
;; (defun bz/app (&optional arg)
;;   (interactive "P")
;;   ;; Stolen from counsel
;;   (when (or (null bz/apps) arg)
;;     (setq bz/apps nil)
;;     ;; Loop through desktop files
;;     (dolist (dir bz/app-directories)
;;       (dolist (f (split-string
;;                   ($$ "find '%s' 2> /dev/null | %s" dir "grep '\\.desktop$'")))
;;         ;; Parse the name of each program and add it to the list
;;         (push (cons (concat
;;                      (or (ignore-errors
;;                            (substring ($$ "grep '^Name\\(\\[en_GB\\]\\)\\?=' '%s' | head -n 1" f) 5 -1))
;;                          "")
;;                      (or (ignore-errors
;;                            (concat " - " (substring ($$ "grep '^Comment=' '%s' | head -n 1" f) 8 -1)))
;;                          ""))
;;                     (ignore-errors
;;                       (substring
;;                        ($$ "grep '^Exec=' '%s' | head -n 1" f)
;;                        5 -1)))
;;               bz/apps))))

;;   (let ((app (assoc (completing-read "App: " (mapcar 'car bz/apps)) bz/apps)))
;;     (start-process-shell-command
;;      (car app) (format " <<%s>>" (car app))
;;      (cdr app))))


;;; Move buffer file

(defun bz/move-buffer-file (new-location)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FMove file: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (file-exists-p new-location)
          (message "File '%s' already exists!" new-location)
        (rename-file filename new-location 1)
        (rename-buffer (file-name-nondirectory new-location))
        (set-visited-file-name new-location)
        (set-buffer-modified-p nil)))))


;;; Escape Unicode Chars

(defun bz/escape-chars (str)
  (let ((count 0) (i 0))
    (while (< i (length str))
      (when (> (aref str i) 255)
        (setq str (concat (substring str 0 i)
                          (format "\\u{%X}" (aref str i))
                          (substring str (1+ i)))
              count (1+ count)))
      (setq i (1+ i)))
    (message "Escaped %s chars" count)
    str))

(defun bz/escape-buffer-chars ()
  (interactive)
  (let ((escaped (bz/escape-chars (buffer-string)))
        (line (line-number-at-pos)))
    (delete-region (point-min) (point-max))
    (insert escaped)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun bz/escape-char-at-point ()
  (interactive)
  (let ((escaped (format "\\u%X" (char-after))))
    (delete-char 1)
    (insert escaped)))


;;; Remove all advice

(defun bz/unadvise (func)
  (interactive
   (list (helpful--read-symbol
          "Function: "
          (helpful--callable-at-point)
          (lambda (fn) (advice--p (advice--symbol-function fn))))))

  (advice-mapc (lambda (advice _) (advice-remove func advice)) func))


;;; Recompile modules

(defun bz/compile-modules ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/modules/" 0))


;;; Type out a buffer char by char

;; Inspired by primegean editor tier list
(defun bz/type-out-buffer ()
  (interactive)
  (let ((content (buffer-string)))
    (font-lock-mode 0)

    (delete-region (point-min) (point-max))
    (bz/insert-rest-of-buffer (current-buffer) content 0)))

(defun bz/insert-rest-of-buffer (buffer text index)
  (when (>= (length text) (1+ index))
    (with-current-buffer buffer
      (setq-local cursor-type nil)
      (goto-char (point-max))
      (insert (substring text index (1+ index)))
      (run-with-timer 0.05 nil 'bz/insert-rest-of-buffer buffer text (1+ index)))))


;;; Markdown to org

(defun bz/md-to-org ()
  "Open a temporary buffer containing the current markdown file
converted to org mode for emacs-friendly viewing."
  (interactive)
  (let ((buf (format "*%s->org*" (buffer-name))))
    (shell-command-on-region
     (point-min) (point-max) "pandoc -f markdown -t org" buf)

    (switch-to-buffer buf)

    (org-mode)))


;;; Add face text property

(defun bz/add-face (text &rest faces)
  (dolist (face faces)
    (add-face-text-property 0 (length text) face t text))
  text)


;;; Insert without overlays

(defun bz/insert-without-overlays (text)
  (let ((start (point)))
    (insert text)
    (dolist (o (overlays-at start))
      (if (eq (overlay-start o) start) (move-overlay o (point) (overlay-end o))
        (if (eq (overlay-end o) (point)) (move-overlay o (overlay-start o) start)
          (error "What the fook?"))))))


;;; Alphabet

(defvar bz/alphabet "abcdefghijklmnopqrstuvwxyz")
(defun bz/alphabet (&optional n)
  (interactive)
  (cond ((called-interactively-p 'interactive) (insert bz/alphabet))
        (n (intern (substring bz/alphabet n (1+ n))))
        ((intern bz/alphabet))))


;;; Rgn

(defun rgn ()
  (interactive)
  (if mark-active (buffer-substring-no-properties (point) (mark))
    (buffer-substring-no-properties (point) (1+ (point)))))


;;; Math eval

(defun bz/math (str)
  (let* ((seg "") (dep 0) (count 1) (quoted 0)
         (repl (replace-regexp-in-string "^\\([a-zA-Z/-]+\\)=\\(.+\\)" "(setq \\1 \\2)" str))
         ;; (str (replace-regexp-in-string "\\^" "**" str))
         ;; (str (replace-regexp-in-string "\\$[0-9]+" "(symbol-value '\\&)" str))
         el segs char update calc-str calc-args dollar)
    ;; Generate `segs`, an alternating list of calc segments and elisp segments
    ;; to be concatenated together. The first segment is always a math segment
    (if (or (not (string= str repl)) (and (> (length str) 0) (eq (aref str 0) ?\")))
        (eval (read (or repl str)))
      (dotimes (i (length str))
        (setq char (aref str i) update nil)
        (when (and dollar (not (and (>= char ?0) (<= char ?9)))) (setq dollar nil))
        (unless (eq quoted 0) (setq quoted (1- quoted)))
        (cond (dollar)
              ((and (not el) (eq char ?$)) (setq dollar t el t update t))
              ((eq char ?\()
               (if (and el (= dep 0) (= quoted 0))
                   (progn (push seg segs) (setq seg " " dep 1 update t el t))
                 (setq dep (1+ dep) update (not el) el t)))
              ((eq char ?\)) (setq dep (1- dep)))
              ((and (or (not el) (> quoted 0)) (memq char '(?' ?`)))
               (setq quoted 2 update (not el) el t))
              ((and (not el) (or (and (>= char ?A) (<= char ?Z))
                                 (and (>= char ?a) (<= char ?z))))
               (setq el t update t))
              ((and el (= dep 0) (not (or (and (>= char ?A) (<= char ?Z))
                                          (and (>= char ?a) (<= char ?z))
                                          (memq char '(?- ?/)))))
               (setq el nil update t)))
        ;; When not in an emacs lisp expression, replace { and } with ( and )
        (when (and (not el) (eq char ?{)) (setq char 40))
        (when (and (not el) (eq char ?})) (setq char 41))
        (when update (push seg segs) (setq seg ""))
        (if (and (not el) (eq char ?^))
            (setq seg (concat seg "**"))
          (setq seg (concat seg (string char)))))
      (push seg segs) (setq el t)
      ;; Go through segs, and interperet odd numbered segments as math
      ;; expressions, and pass even numbered segments as elisp arguments
      (if (and (eq (length segs) 2) (string= (cadr segs) ""))
          (eval (read (car segs)))
        (dolist (s (reverse segs))
          (setq el (not el))
          (if (not el) (setq calc-str (concat calc-str s))
            (setq calc-str (concat calc-str (make-string count ?$)) count (1+ count))
            (push (read s) calc-args)))
        (string-to-number (apply #'calc-eval calc-str nil (mapcar 'eval (reverse calc-args))))))))


;;; Copy a self-referencing tree

(defun bz/copy-looped-tree (tree &optional origs copies)
  (if (not (listp tree)) tree

    (let ((idx (seq-position origs tree #'eq))
          copy)
      (if idx (nth idx copies)

        (setq copy (mapcar #'ignore tree))
        (setq origs (nconc origs (list tree)))
        (setq copies (nconc copies (list copy)))

        (dotimes (i (length tree))
          (setf (nth i copy) (bz/copy-looped-tree (nth i tree) origs copies)))

        copy))))


;;; Continued fractions

(defun bz/continued-fraction (non rep &optional depth)
  (unless depth (setq depth 0))
  (cond ((and (null non) (null rep)) 1)
        ((= depth 20) 1)
        (non (+ (car non) (if (and (null (cdr non)) (null rep)) 0
                            (/ 1.0 (bz/continued-fraction (cdr non) rep depth)))))
        (t (+ (nth (mod depth (length rep)) rep) (/ 1.0 (bz/continued-fraction nil rep (1+ depth)))))))


;;; Cycles

(defmacro bz/compose-cycles (&rest cycles)
  `(apply 'bz/compose-cycles-func ',cycles))

(defun bz/compose-cycles-func (&rest cycles)
  (let* ((max (apply #'max (flatten-list cycles)))
         (nums (number-sequence 1 max))
         (fn (lambda (acc cycle)
               (let ((idx (-elem-index acc cycle)))
                 (if idx (nth (mod (1+ idx) (length cycle)) cycle) acc))))
         cs c next)
    (while nums
      (setq c nil next (car nums))
      (while (not (eq next (car c)))
        (setq c (nconc c (list next)))
        (setq nums (delq next nums))
        (setq next (-reduce-from fn next (reverse cycles))))
      (setq cs (nconc cs (list c))))
    cs))


;;; Save position

(defmacro bz/save-position (&rest body)
  `(let ((line (line-number-at-pos))
         (column (current-column)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char (min column (- (pos-eol) (point))))))


;;; Bytes to string

(defun bz/bytes-to-string (bytes-str)
  (let ((byte-strs (split-string bytes-str " ")))
    (read (format "\"%s\"" (string-join (--map (format "\\u00%s" it) byte-strs))))))


;;; Read color

(defun bz/insert-color (color)
  (interactive (list (read-color "Insert Color: " t)))
  (when (stringp color)
    (insert (concat "#" (substring color 1 3) (substring color 5 7) (substring color 9 11)))))


;;; Add 1 to certain letters (topology textbook)

(defun add-1-to-letters (string)
  (mapconcat
   (lambda (letter)
     (string
      (if (and (>= letter (1+ ?j)) (<= letter (1+ ?z)))
          (1- letter) letter)))
   (append string nil)
   ""))


;;; Byte compile package

(defun bz/byte-compile-package-functions (package-name)
  "Byte compile all functions belonging to PACKAGE-NAME."
  (interactive "sPackage name: ")
  (let ((package-regexp (format "\\`%s\\(-\\|\\'\\)" (regexp-quote package-name))))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-match-p package-regexp (symbol-name sym))
                  (not (byte-code-function-p (symbol-function sym)))
                  (not (macrop sym)))
         (message "Byte compiling `%s'" sym)
         (byte-compile sym))))
    (message "Byte compilation finished")))


;;; DBG macro

(defmacro dbg (&rest forms)
  (let ((format-str
         (cl-loop for form in forms
                  for sep = "" then ", "
                  concat (format "%s%s=%%s" sep
                                 (if (and (consp form) (cdr form))
                                     (format "(%s...)" (car form))
                                   form)))))
    `(message ,format-str ,@forms)))


;;; Window bottom right poshandler

(defun posframe-poshandler-bz/window-bottom-right-corner (info)
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (+ window-left window-width
             (- 0 12 posframe-width))
          (+ window-top window-height
             (- 0 12 mode-line-height posframe-height)))))


;;; Purge a library prefix

(defun bz/purge-prefix (prefix)
  (interactive "sPrefix: ")
  (let* ((regexp (format "^%s\\>" (regexp-quote prefix)))
         (pred (lambda (sym) (string-match-p regexp (format "%s" sym)))))
    (mapatoms
     (lambda (sym)
       (when (funcall pred sym)
         (fmakunbound sym)
         (makunbound sym))

       ;; Clear symbol properties
       (cl-loop for (prop _) on (symbol-plist sym) by #'cddr
                when (funcall pred prop)
                do (put sym prop nil))))

    (cl-callf2 cl-remove-if pred features)))


;;; Provide

(provide 'bz-functions)

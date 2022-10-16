(qv/package dash)

;;; Wait
(defmacro async (&rest args)
  (let ((time 0))
    (when (numberp (car args)) (setq time (car args) args (cdr args)))
    `(run-with-timer ,time nil (lambda () . ,args))))

;;; Modify in place
(defmacro : (var func &rest args)
  `(setf ,var (,func ,var . ,args)))

;;; Not Equal
(defun != (arg1 &rest args)
  (not (apply 'eq arg1 args)))

;;; Vector to list
(defun vector->list (vector)
  (let (list)
    (dotimes (i (length vector))
      (push (aref vector i) list))
    (reverse list)))

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

;;; Math
(defmacro $ (&rest args)
  (let ((seg "") (dep 0) (count 1)
        (str (if (and (eq 1 (length args)) (stringp (car args)))
                 (car args) (substring (format "%s" args) 1 -1)))
        el segs char update calc-str calc-args)
    ;; Generate `segs`, an alternating list of calc segments and elisp segments
    ;; to be concatenated together. The first segment is always a math segment
    (dotimes (i (length str))
      (setq char (aref str i) update nil before nil)
      (cond ((eq char 40) (setq dep (1+ dep) update (not el) el t))
            ((eq char 41) (setq dep (1- dep)))
            ((and (not el) (>= char ?A) (<= char ?z)) (setq el t update t))
            ((and el (eq dep 0) (or (< char ?A) (> char ?z))) (setq el nil update t)))
      ;; When not in an emacs lisp expression, replace { and } with ( and )
      (when (and (not el) (eq char ?{)) (setq char 40))
      (when (and (not el) (eq char ?})) (setq char 41))
      (when update (push seg segs) (setq seg ""))
      (setq seg (concat seg (string char))))
    (push seg segs) (setq el t)
    ;; Go through segs, and interperet odd numbered segments as math
    ;; expressions, and pass even numbered segments as elisp arguments
    (dolist (s (reverse segs))
      (setq el (not el))
      (if (not el) (setq calc-str (concat calc-str s))
        (setq calc-str (concat calc-str (make-string count ?$)) count (1+ count))
        (push (read s) calc-args)))
    `(string-to-number (calc-eval ,calc-str nil . ,(reverse calc-args)))))

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

;;; Grabbing Text
(defun qv/text (&optional dist1 dist2)
  "Get some text near the point."
  (buffer-substring-no-properties
   (+ (point) (or dist2 0)) (+ (point) (or dist1 1))))

;;; Lines
(defun qv/line (&optional num distp)
  (save-excursion
    (cond (distp (forward-line num))
          (num (goto-line num)))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun qv/linenum (&optional num distp)
  (string-to-number (qv/line num distp)))

(defun qv/set-line (contents &optional num distp)
  (save-excursion
    (cond (distp (forward-line num))
          (num (goto-line num)))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert (format "%s" contents))))

;;; Save to Clipboard
(defun qv/copy-last-echo ()
  (interactive)
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (1- (point-max)))
      (kill-new (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))))

(qv/keys * "C-x C-y" qv/copy-last-echo)

(defun qv/copy (expr)
  (kill-new (format "%s" expr)))

;;; Global to String
(defun str (expr)
  (format "%s" expr))

;;; Wait for Input
(defmacro qv/wait (&rest vars)
  `(progn
     (redraw-display)
     (read-char (mapconcat
                 (lambda (var) (format "%s: %s" var (eval var)))
                 ',vars "\n"))))

;;; New buffer
(defun qv/new-buffer (name)
  (interactive "sBuffer Name: ")
  (switch-to-buffer (generate-new-buffer name)))
;;; Read Unicode Chars
(setq qv/unicode-chars nil)

(defun qv/read-unicode-char ()
  (interactive)
  ;; Initialize the list of unicode chars if it isn't already
  (unless qv/unicode-chars
    (maphash (lambda (name num)
               (push (format "%s\t %s" (string num) name)
                     qv/unicode-chars))
             (ucs-names))
    (setq qv/unicode-chars (reverse qv/unicode-chars)))

  (let ((result (completing-read "Unicode Char: " qv/unicode-chars)))
    (insert (if (eq (aref result 0) ?\t) "\t"
              (car (split-string result "\t"))))))



;;; Read Fonts
(when (ignore-errors ivy-format-functions-alist)
  (push '(qv/read-font . ivy-format-function-default) ivy-format-functions-alist))
(defun qv/read-font ()
  (interactive)
  (let* ((ex (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     "abcdefghijklmnopqrstuvwxyz"
                     "0123456789"
                     ",.;:?!@#$%^&*~_-=+()[]{}<>\"'`/|\\"))
         (space (propertize "\t" 'display '(space :align-to 50))))
    (insert
     (car (split-string
           (completing-read
            "Font Family:"
            (--map (format "%s%s%s" it space (propertize ex 'face (list :family it)))
                   (font-family-list)))
           "\t")))))
;;; Base converter
(defun decimal-to (num base)
  (apply '+ (mapcar (lambda (p) (* (expt 10 p) (% (/ num (expt base p)) base)))
                    (number-sequence 0 (if (eq num 0) 0 (floor (log num base)))))))
;;; Linux App
(setq qv/apps nil)
(setq qv/app-directories '("/run/current-system/sw/share/applications/"))
(defun qv/app (&optional arg)
  (interactive "P")
  ;; Stolen from counsel
  (when (or (null qv/apps) arg)
    (setq qv/apps nil)
    ;; Loop through desktop files
    (dolist (dir qv/app-directories)
      (dolist (f (split-string
                  (shell-command-to-string
                   (format "find '%s' 2> /dev/null | %s"
                           dir "grep '\\.desktop$'"))))
        ;; Parse the name of each program and add it to the list
        (push (cons (concat
                     (or (ignore-errors
                           (substring
                            (shell-command-to-string
                             (format "grep '^Name\\(\\[en_GB\\]\\)\\?=' '%s' | head -n 1" f))
                            5 -1))
                         "")
                     (or (ignore-errors
                           (concat
                            " - "
                            (substring
                             (shell-command-to-string
                              (format "grep '^Comment=' '%s' | head -n 1" f))
                             8 -1)))
                         ""))
                    (ignore-errors
                      (substring
                       (shell-command-to-string
                        (format "grep '^Exec=' '%s' | head -n 1" f))
                       5 -1)))
              qv/apps))))

  (let ((app (assoc (completing-read "App: " (mapcar 'car qv/apps)) qv/apps)))
    (start-process-shell-command
     (car app) (format " <<%s>>" (car app))
     (cdr app))))
;;; Copy to clipboard
(defun qv/copy (text)
  (with-temp-buffer
    (insert (format "%s" text))
    (kill-region (point-min) (point-max))))
;;; Grep Directory
;;; Search in Directory
(defun qv/grep (file)
  (interactive "GSearch location: ")

  (let ((qv/vertico-dont-format-candidates t))

    (if (file-directory-p file)
        (consult-grep file)
      (find-file file)
      (consult-line))))
;;; Move buffer file
(defun qv/move-buffer-file (new-location)
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

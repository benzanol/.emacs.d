(require 'dash)

(defvar pyelisp-buffer-name "*pyelisp*")

(defvar pyelisp-script "~/.emacs.d/my-packages/python-elisp/process.py")

(defvar pyelisp-process nil)

(defvar pyelisp-prefix "~~"
  "String to indicate the start of an elisp expression in python code.")

(defvar pyelisp-output-prefix "--eval--"
  "Indicates emacs lisp code to be evaluated.")

(defun pyelisp-start ()
  "Start a python process and return it."
  (interactive)

  (let* ((buf pyelisp-buffer-name)
         (existing (get-buffer-process buf)))

    ;; Kill the existing process if there is one
    (when (process-live-p existing)
      (kill-process existing))

    ;; Create the new process
    (setq pyelisp-process
          (make-process
           :name "pyelisp"
           :buffer buf
           :filter 'pyelisp-filter-output
           :command '("python3")))

    ;; If you try to run python3 directly with the full script path,
    ;; it won't be able to find it, so it has to be sent directly
    (with-temp-buffer
      (insert-file pyelisp-script)
      (process-send-region pyelisp-process (point-min) (point-max)))))

(defun pyelisp-format-code (code)
  "Format mixed python/elisp code CODE to be valid python code."
  (let ((inhibit-message t))

    (with-temp-buffer
      (insert code)
      
      ;; Replace emacs lisp segments with valid python
      (beginning-of-buffer)
      (let ((regexp (format "\\(?:\\`\\|[ \n(]?\\)\\(%s\\)[^ \n]" pyelisp-prefix)))
        (while (search-forward-regexp regexp nil t)
          ;; Delete the prefix
          (goto-char (match-beginning 1))
          (delete-region (match-beginning 1) (match-end 1))

          (cond
           ;; If it is a direct function call
           ((looking-at-p "[^][\s\t\n(){}\"']+(")
            (insert "elisp_call('")
            ;; insert('hi') => elisp_call('insert', 'hi'
            (search-forward "(")
            (delete-backward-char 1)
            (insert "', "))

           ;; If it is setting a variable
           ((and (= (point) (save-excursion (beginning-of-line-text) (point)))
                 (looking-at-p "[^][\s\t\n(){}\"']+ += +[^\s\t\n]"))
            (insert "elisp_set('")
            (search-forward-regexp " += +")
            (delete-region (match-beginning 0) (match-end 0))
            (insert "', ")
            (end-of-line) (insert ")"))

           ;; Otherwise, treat it as a raw elisp expression
           (t
            ;; Add the function call
            (insert "elisp_eval(f'")

            ;; Add the closing quote and paren
            (forward-sexp)
            (insert "')")))))

      ;; Combine the code into a single line, replacing newlines with '\n'
      ;; and literal backslashes with '\b'
      (beginning-of-buffer) (replace-string "\\" "\\b")
      (beginning-of-buffer) (replace-string "\n" "\\n")

      ;;(message "%s" (current-buffer)) (recursive-edit)

      (buffer-string))))

(defun pyelisp-send (code)
  "Send python code to the pyelisp process."

  (process-send-string pyelisp-process (pyelisp-format-code code))
  (process-send-string pyelisp-process "\n"))

(defun pyelisp-python-representation (obj)
  "Convert an elisp object OBJ to its python string representation."

  (cond ((eq obj t) "True")
        ((eq obj nil) "[]") ;; The empty list also evaluates to false in python
        ((listp obj)
         (--> (mapcar 'pyelisp-python-representation obj)
              (string-join it ", ")
              (format "[%s]" it)))
        ((vectorp obj) (pyelisp-python-representation (append obj nil)))
        ((symbolp obj) (format "ElispSymbol('%s')" obj))
        ((bufferp obj) (format "ElispBuffer('%s')" (buffer-name obj)))
        (t (prin1-to-string obj))))

(defun pyelisp-filter-output (pname output)
  (setq output (s-trim output))

  (dolist (line (split-string output "\n"))
    (cond
     ((--any? (string-match-p it line)
              '("Python [0-9]\\.[0-9]\\.[0-9] .*"
                "\\[GCC [0-9]+\\.[0-9]+\\.[0-9]+\\].*"
                "Type \"help\", \"copyright\", \"credits\" or \"license\".*"
                "^>>> [.>\s\t]+")))
     ((s-starts-with-p pyelisp-output-prefix line)

      (condition-case nil

          (let* ((in (substring line (length pyelisp-output-prefix)))
                 (out (pyelisp-python-representation (eval (read in)))))
            ;;(message "COMMS: %s -> %s" in out)
            (process-send-string pyelisp-process (concat out "\n")))

        ;; If there was an error evaluating the elisp, make sure the
        ;; python process recieves some value so it isn't interrupted
        (error (message "Python: Error calling elisp: `%s`"
                        (substring line (length pyelisp-output-prefix)))
               (process-send-string pyelisp-process "False\n"))))

     (t (message "Python: %s" line)))))

(defun pyelisp-send-region (beg end)
  (interactive
   (if mark-active (list (mark) (point))
     (list (line-beginning-position) (line-end-position))))

  (pyelisp-send (buffer-substring-no-properties beg end)))

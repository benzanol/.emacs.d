;; Hash table of {sid -> prop alist} as well as {tag -> sid list}
(defvar lens-index)

(setq lens-state-directory "~/Test/states/")


(defun lens--state-path (sid)
  (file-name-concat lens-state-directory (format "%s.md" sid)))

(defun lens-index ()
  "Returns a hashtable from sids (numbers) to tag lists (symbols) and tags to sid lists"
  (let* ((default-directory lens-state-directory)
         (cmd "for file in ./*.md; do\n echo '>>>>>'$file\n tail -n +2 $file | sed /---/q | head -n +2\n done")
         (file-outputs (split-string (shell-command-to-string cmd) ">>>>>" 'nonulls))
         (ht (make-hash-table))
         sid shell-file props-str props)

    (dolist (file file-outputs)
      (let* ((lines (split-string file "\n" 'nonulls))
             (sid (string-to-number (file-name-base (car lines))))
             (props (--map (let ((i (s-index-of ": " it)))
                             (cons (intern (substring it 0 i))
                                   (substring it (+ i 2))))
                           (cdr lines))))

        (puthash sid props ht)
        (dolist (tag (cons (concat "type:" (or (alist-get 'type props) "nil"))
                           (split-string (alist-get 'tags props) " ")))
          (puthash (intern tag) (cons sid (gethash (intern tag) ht)) ht))))

    ht))

(setq lens-index (lens-index))

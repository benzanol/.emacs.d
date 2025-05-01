
(defvar lens-id-index nil
  "A hash table mapping ids (symbols) to file paths.")

(defvar lens-directories nil
  "A list of directories possibly containing lens notes.")

(defun lens-setup-index ()
  "Update the lens-id-index by searching directories for ids"

  (setq lens-id-index (make-hash-table))

  ;; Print alternating lines of the file name and the first line
  (let* ((default-directory "~")
         (case-fold-search t) ; Ignore case when splitting
         (body-cmd "echo -n \"$f // \"; head -n 1 \"$f\"; echo")
         (grep-cmd "grep -i '^\\([^/]\\|.[^/]\\)\\+ // #+id: '")
         ;; For loop is roughly 3x faster than find -exec
         (cmd (format "(for f in $(find %%s -type f); do\n%s\n done) | %s" body-cmd grep-cmd)))

    (dolist (dir lens-directories)
      ;; (message (format-time-string "%H:%M:%S:%3N"))
      (let* ((dir-cmd (format cmd (shell-quote-argument (expand-file-name dir))))
             (output (shell-command-to-string dir-cmd)))
        ;; (message (format-time-string "%H:%M:%S:%3N"))
        (dolist (line (split-string output "\n" 'nonulls))
          (let ((split (s-split-up-to " // #\\+id: " line 1)))
            (puthash (intern (cadr split)) (car split) lens-id-index)))))))


(org-link-set-parameters "lens" :follow #'lens--follow-link)
(defun lens--follow-link (path _)
  "Open a lens type org link.
PATH is the id of the target note as a string."
  (find-file (gethash (intern path) lens-id-index)))

(defun lens-id-file (id)
  (unless lens-id-index (lens-setup-index))
  (gethash id lens-id-index))


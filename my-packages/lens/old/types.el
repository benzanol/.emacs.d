;; Alist of form (TYPENAME PLIST...)
;; Plist properties are all the values specified in the plan
(defvar lens-type-alist nil)

(defvar lens-project-root nil)


(defun lens--generate-type-plist (struct)
  "Parse a type file structure to a type spec."
  (let (plist hooks)

    (let ((val (alist-get 'inherit (car struct))))
      (when val (setf (plist-get plist :inherit)
                      (mapcar #'intern (split-string val " ")))))

    (dolist (hook-prop '("Create" "Open" "Save" "Delete"))
      (setq hooks nil)

      ;; Loop through the elements of the section
      (dolist (element (car (alist-get hook-prop (cdr struct) nil nil #'string=)))
        ;; If the element is a block, make it a hook
        (pcase element
          (`(block "emacs-lisp" ,code . ,_)
           (push (eval (read (format "(progn %s)" code))) hooks))))

      ;; Now reverse it to put it in order
      (when hooks
        (setf (plist-get plist (intern (format ":%s" (downcase hook-prop))))
              (reverse hooks))))

    plist))

(defun lens-setup-project-root (dir)
  (setq lens-project-root (expand-file-name dir))

  (setq lens-type-alist
        (--map (cons (intern (file-name-base it))
                     (lens--generate-type-plist (lens-parse-file it)))
               ;; Org files in the type directory
               (--filter (s-ends-with-p ".org" it) (f-files (concat dir "/types")))))

  lens-project-root)


(defun lens-run-type-hooks (type hook)
  (let* ((type-spec (alist-get type lens-type-alist))
         (fs (apply #'append
                    (plist-get type-spec hook)
                    (--map (lens--get-type-hooks it hook)
                           (plist-get type-spec :inherit)))))

    (dolist (func fs)
      (condition-case err
          (funcall func)
        (error (message "Error running %s: hook: %s" hook err))))))

(defun lens--get-type (&optional buf-or-file)
  "If buf-or-file is a string, get the type as it exists on disk.
Otherwise, get the value from a buffer if possible."

  (let* ((struct (if (stringp buf-or-file) (lens-parse-file buf-or-file)
                   (with-current-buffer (or buf-or-file (current-buffer)) (lens-parse))))
         (type-string (alist-get 'type (car struct))))
    (when (stringp type-string) (intern type-string))))

(defun lens--type-file (type)
  (format "%s/types/%s.org" lens-project-root type))

(defun lens-state-p ()
  (and (s-starts-with-p (expand-file-name lens-project-root)
                        (expand-file-name buffer-file-name))
       (lens--get-mode-spec buffer-file-name)))

(defun lens-file-link (file)
  (list 'link (concat "file:" (f-relative file lens-project-root))))


(defun lens-get-type-instances (type)
  (->> (car (lens-get-section (lens--type-file type) "Instances"))
       ((lambda (n) (message "--%s" n) n))
       (--map (pcase it (`(link ,target . ,_)
                         (when (string-match "\\`file:\\(.*\\)\\'" target)
                           (match-string 1 target)))))
       (-filter #'identity)))



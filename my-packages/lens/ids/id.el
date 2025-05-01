;;; Parse org files

(defun lens--parse-org ()
  (save-excursion
    (let (props)
      (beginning-of-buffer)

      ;; Read the props
      (while (looking-at "#\\+\\([a-zA-Z0-9]+\\): \\(.*\\)$")
        (push (cons (intern (match-string 1))
                    (org-no-properties (match-string 2)))
              props)
        (forward-line))

      ;; Parse from the end of the props as a section of level 0
      (cons props (lens--parse-org-section 0)))))

(defun lens--parse-org-section (level)
  ;; Skip blank lines
  (while (and (eolp) (not (eobp))) (forward-line))

  (let ((case-fold-search t)
        heading rev-lines rev-headings)
    ;; Loop until at the end of the buffer OR there is a heading greater than LEVEL
    (while (progn (setq heading (when (looking-at "\\*+ ") (1- (length (match-string 0)))))
                  (and (not (eobp)) (or (null heading) (> heading level))))
      (cond
       ;; TODO: Parse nested lists as a structure

       ;; If looking at a link (with description)
       ((and (null heading) (looking-at "\\[\\[\\([^][\n]+\\)\\]\\[\\([^][\n]+\\)\\]\\]"))
        (push (list 'link (org-no-properties (match-string 1)) (org-no-properties (match-string 2)))
              rev-lines)
        (forward-line))

       ;; If looking at a link (without description)
       ((and (null heading) (looking-at "\\[\\[\\([^][\n]+\\)\\]\\]"))
        (push (list 'link (org-no-properties (match-string 1))) rev-lines)
        (forward-line))

       ;; If looking at a code block
       ((and (null heading) (looking-at "#\\+begin_src \\([^ \n]+\\)\\(?: \\(.*\\)\\)?"))
        (let ((lang (match-string 1)) (props (match-string 2)) (eol (point-at-eol)))
          (search-forward-regexp "^#\\+end_src$")
          (push `(block
                  ,(org-no-properties lang)
                  ,(buffer-substring-no-properties (1+ eol) (1- (point-at-bol)))
                  ;; Only include a fourth list element if props is non-nil
                  ,@(when props (list (org-no-properties props))))
                rev-lines)
          (forward-line 1)))

       ;; Add to the content lines
       ((null heading)
        (push (buffer-substring-no-properties (point) (point-at-eol)) rev-lines)
        (forward-line))

       ((> heading (1+ level)) (error "Skipped heading level"))
       ;; Heading = 1+level, so parse the nested heading
       (t (push (cons (buffer-substring-no-properties (+ (point) heading 1) (point-at-eol))
                      (progn (forward-line) (lens--parse-org-section (1+ level))))
                rev-headings))))

    ;; Trim and reverse the rev-lines
    (while (equal (car rev-lines) "") (pop rev-lines))
    (setq rev-lines (reverse rev-lines))
    (while (equal (car rev-lines) "") (pop rev-lines))

    (cons rev-lines (reverse rev-headings))))


(defun lens--generate-org (struct)
  (let ((body (string-trim (lens--generate-org-section (cdr struct) 1) "\n" "\n")))
    ;; If there are no properties, just have the body
    (if (null (car struct)) (concat body "\n")

      ;; If there are properties, put newlines between them and the body
      (concat (if (null (car struct)) ""
                (->> (car struct)
                     (--map (format "#+%s: %s" (car it) (cdr it)))
                     ;; Sort the id property first
                     (seq-sort-by (lambda (p) (if (s-starts-with-p "#+id:" p) "" p)) #'string<)
                     (s-join "\n")))
              "\n\n" body "\n"))))

(defun lens--generate-org-section (section level)
  (let* ((lines (--map (pcase it
                         (`(link ,target ) (format "[[%s]]" target))
                         (`(link ,target ,desc) (format "[[%s][%s]]" target desc))
                         ((or `(block ,lang ,code) `(block ,lang ,code ,props))
                          (format "#+begin_src %s%s\n%s\n#+end_src"
                                  lang (if props (concat " " props) "") code))
                         (str str))
                       (car section)))
         (content (s-join "\n" lines)))

    (setq secs (--map (format "%s %s\n%s" (make-string level ?*) (car it)
                              (lens--generate-org-section (cdr it) (1+ level)))
                      (cdr section)))

    (cond ((not (string= content "")) (apply #'concat content "\n\n" secs))
          (secs (apply #'concat secs))
          (t "\n"))))

;;; User-facing Parse functions

(defun lens-parse-file (file)
  "Fetch the struct of a file as it is saved on disk."
  (with-temp-buffer
    (insert-file file)
    (lens--parse-org)))

(defun lens-parse (&optional file)
  "Fetch the most up to date struct (from an unsaved buffer if necessary.)"
  (let ((buf (if file (get-file-buffer file) (current-buffer))))
    (if (null buf) (lens-parse-file file)
      (with-current-buffer buf (lens--parse-org)))))


(defun lens-operate (file func)
  "Modify the buffer/file and then save it.
If FILE is nil, use the current buffer, and don't save.
FUNC takes a struct as its input and mutates it."

  (let* ((line (line-number-at-pos)) (col (current-column))
         (operation
          (lambda ()
            (let ((struct (lens--parse-org)))
              ;; Mutate the struct
              (funcall func struct)

              (delete-region (point-min) (point-max))
              (insert (lens--generate-org struct))))))

    (cond ((null file) (funcall operation))
          ((get-file-buffer file)
           (with-current-buffer (get-file-buffer file)
             (funcall operation)))
          ((unwind-protect
               (progn (find-file file)
                      (funcall operation))
             (kill-buffer))))

    (beginning-of-buffer)
    (forward-line (1- line))
    (forward-char col)))



;;; Query commands: util functions that use the above helper functions

;;;; Functions for modifying properties

(defun lens-query-get (file prop)
  (alist-get prop (car (lens-parse file))))

(defun lens-query-set (file prop string)
  (lens-operate
   file
   (lambda (struct)
     (setf (alist-get prop (car struct)) string))))

(defun lens-query-modify (file prop func)
  (lens-operate
   file
   (lambda (struct)
     (let* ((old (alist-get prop (car struct)))
            (new (funcall func old)))
       (setf (alist-get prop (car struct)) new)))))


;;;; Helper functions for modiying content

(defun lens--get-or-create-subsection (section headings)
  "Helper function for finding/creating a nested heading within an existing section."
  (if (null headings) section

    (let ((next-sec (alist-get (car headings) (cdr section) nil nil #'string=)))
      (unless next-sec
        (setq next-sec (list nil))
        (nconc section (list (cons (car headings) next-sec))))

      (lens--get-or-create-subsection next-sec (cdr headings)))))

(defun lens--get-subsection (section headings)
  "Helper function for finding/creating a nested heading within an existing section."
  (if (null headings) section
    (let ((next-sec (alist-get (car headings) (cdr section) nil nil #'string=)))
      (when next-sec (lens--get-subsection next-sec (cdr headings))))))


;;;; Functions for modiying content

(defun lens-query-insert (file elem pos &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings))
             (new-lines (-insert-at (or pos (length old-lines)) elem (car section))))
        (setcar section new-lines)))))

(defun lens-query-add (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (unless (member elem (car section))
          (setcar section (append (car section) (list elem))))))))

(defun lens-query-remove (file elem &rest headings)
  (lens-operate
   file
   `(lambda (struct)
      (let* ((section (lens--get-or-create-subsection (cdr struct) headings)))
        (setcar section (--filter (not (equal it elem)) (car section)))))))

(defun lens-query-section (file &rest headings)
  (lens--get-subsection (cdr (lens-parse file)) headings))



;;; Set up ids

(defun lens-generate-id-index (directories)
  "Update the lens-id-index by searching directories for ids."

  ;; Print alternating lines of the file name and the first line
  (let* ((default-directory "~")
         (id-index (make-hash-table))
         (case-fold-search t) ; Ignore case when splitting
         (body-cmd "echo -n \"$f // \"; head -n 1 \"$f\"; echo")
         (grep-cmd "grep -i '^\\([^/]\\|.[^/]\\)\\+ // #+id: '")
         (find-cmd "find %s -type f -name '*.org'")
         ;; (find-cmd "find %s -type f -name '*.org' -exec grep -Iq . {} \\; -print") ; Only text files
         ;; For loop is roughly 3x faster than find -exec
         (cmd (format "(for f in $(%s); do\n%s\n done) | %s" find-cmd body-cmd grep-cmd)))

    (dolist (dir directories)
      ;; (message (format-time-string "%H:%M:%S:%3N"))
      (let* ((dir-cmd (format cmd (shell-quote-argument (expand-file-name dir))))
             (output (shell-command-to-string dir-cmd)))
        ;; (message (format-time-string "%H:%M:%S:%3N"))
        (dolist (line (split-string output "\n" 'nonulls))
          (let ((split (s-split-up-to " // #\\+id: " line 1)))
            (puthash (intern (cadr split)) (car split) id-index)))))

    id-index))


(defvar lens-directories nil
  "A list of directories possibly containing lens notes.")

(defvar lens-id-index nil
  "A hash table mapping ids (symbols) to file paths.")


(org-link-set-parameters "lens" :follow #'lens--follow-id-link)
(defun lens--follow-id-link (path _)
  "Open a lens type org link.
PATH is the id of the target note as a string."
  (find-file (gethash (intern path) lens-id-index)))

(defun lens-id-file (id)
  (unless lens-id-index (lens-generate-id-index))
  (gethash id lens-id-index))



;;; Set up types

(defun lens-generate-type-alist (dir)
  (let (type-alist)
    (dolist (type-file (--filter (s-ends-with-p ".org" it) (f-files dir)))
      (let ((struct (lens-parse-file type-file)) plist hooks)

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
               (push (eval (read (format "(lambda () %s)" code))) hooks))))

          ;; Now reverse it to put it in order
          (when hooks
            (setf (plist-get plist (intern (format ":%s" (downcase hook-prop))))
                  (reverse hooks))))

        (setf (alist-get (intern (file-name-base type-file)) type-alist) plist)))

    type-alist))


(defvar lens-type-directory nil)

(defvar lens-type-alist nil
  "Alist of form (TYPENAME . PLIST)")


;;; Initialize
(defun lens-initialize (dir-or-dirs type-dir)
  (if (listp dir-or-dirs)
      (setq lens-directories dir-or-dirs
            lens-type-directory (expand-file-name type-dir))

    (setq lens-directories (list dir-or-dirs)
          lens-type-directory (let ((default-directory dir-or-dirs))
                                (expand-file-name type-dir))))

  (setq lens-id-index (lens-generate-id-index lens-directories)
        lens-type-alist (lens-generate-type-alist lens-type-directory))
  (message "Initialized!"))


;;; Project util functions

(defun lens-in-directory-p ()
  (--any (s-starts-with-p (expand-file-name it) buffer-file-name) lens-directories))

(defun lens-has-id-p ()
  (let ((case-fold-search t))
    (save-excursion (beginning-of-buffer) (looking-at-p "#\\+id: "))))

(defun lens-node-p ()
  "Return if the current buffer is a valid node"
  (and (derived-mode-p 'org-mode)
       (lens-in-directory-p)
       (lens-has-id-p)))


(defun lens-get-type (&optional file-or-node)
  (let* ((struct (cond ((consp file-or-node) file-or-node)
                       ((stringp file-or-node) (lens-parse-file file-or-node))
                       ((null file-or-node) (lens-parse))
                       ((error "Invalid file or node: `%s`" file-or-node))))
         (type-string (alist-get 'type (car struct))))
    (when (stringp type-string) (intern type-string))))

(defun lens-type-file (type)
  (format "%s/%s.org" lens-type-directory type))

(defun lens-generate-id ()
  (format "%s-%06d"
          (format-time-string "%Y-%m-%d")
          (mod (random) 1000000)))

(defun lens-create-link (id)
  "Return a lens link specification to the given id."
  (list 'link (format "lens:%s" id)))

(defun lens-run-type-hook (type hook-name)
  "Determine the hook functions associated with the given type, and run them."

  (let* ((all-types (list type))
         (queue (list type)))

    ;; Do a breadth first search of the type heirarchy
    (while queue
      (let* ((spec (alist-get (pop queue) lens-type-alist))
             (new-inherits (--filter (not (memq it all-types)) (plist-get spec :inherit))))
        (nconc all-types new-inherits)
        (setq queue (nconc queue new-inherits))))
    ;; Add nil, the all-encompassing type
    (unless (memq 'all all-types) (nconc all-types (list 'all)))

    (dolist (type all-types)
      (dolist (func (plist-get (alist-get type lens-type-alist) hook-name))
        (condition-case err
            (funcall func)
          (error (message "Error running %s: hook: %s" hook-name err)))))))


;;; Hooks

(defvar-local lens-node nil "The node structure as it exists on disk.")
(defvar-local lens-id nil "The node id.")
(defvar-local lens-type nil "The node type.")

(add-hook 'find-file-hook #'lens--on-open)
(defun lens--on-open ()
  ;; If it is missing everything but an id, ask to add an id
  (and (derived-mode-p 'org-mode)
       (lens-in-directory-p)
       (not (lens-has-id-p))
       (y-or-n-p "This file does not have an id, add one?")
       (progn (lens-add-id) (save-buffer)))

  (when (lens-node-p)
    (setq lens-node (lens-parse)
          lens-id (intern (alist-get 'id (car lens-node)))
          lens-type (lens-get-type lens-node))

    (lens-run-type-hook lens-type :open)))


(add-hook 'before-save-hook #'lens-node--format-org)
(defun lens-node--format-org ()
  (when lens-node

    (if (lens-node-p) (lens-operate nil #'identity)

      (when (y-or-n-p "This was a node but now its not. Add the id back?")
        (lens-query-set nil 'id lens-id)))))


;; (add-hook 'after-save-hook #'lens-node--after-save)
(defun lens-node--after-save ()
  (when lens-node
    (unless (lens-node-p)
      (lens-run-type-hook lens-type :delete)
      (error "This was a node but now its not"))

    ;; Update the contents as stored on disk
    (let* ((lens-old-node lens-node)
           (lens-old-type lens-type)
           (lens-new-node (lens-parse))
           (lens-new-type (lens-get-type lens-new-node)))

      (if (eq lens-old-type lens-new-type)
          (setq lens-node lens-new-node)

        ;; If there is a new type
        (lens-run-type-hook lens-old-type :delete)
        (setq lens-node lens-new-node lens-type lens-new-type)
        (lens-run-type-hook lens-new-type :create))

      (lens-run-type-hook lens-type :save))))


;;; Commands

(defun lens-delete-node ()
  (interactive)
  (unless (lens-node-p) (error "Not a lens node"))
  (lens-run-type-hook lens-type :delete)
  (delete-file buffer-file-name))

(defun lens-add-id (&optional filename)
  "Add an id to the current buffer."
  (interactive)

  (when (lens-query-get (or filename buffer-file-name) 'id)
    (error "Buffer already has an id"))

  (lens-query-set filename 'id (lens-generate-id)))


(defun lens-create-node (path)
  (interactive "fPath: ")
  (unless (s-ends-with-p ".org" path)
    (setq path (concat path ".org")))

  (when (f-exists-p path) (error "File %s already exists" path))

  (with-temp-buffer (lens-add-id) (write-file path))
  (find-file path))

;;; More functions
(defun lens-get-type-instances (type)
  "Return a list of file paths which are instances of TYPE."
  (->> (car (lens-get-section (lens-type-file type) "Instances"))
       ((lambda (n) (message "--%s" n) n))
       (--map (pcase it (`(link ,target . ,_)
                         (when (string-match "\\`lens:\\(.*\\)\\'" target)
                           (match-string 1 target)))))
       (-filter #'identity)))



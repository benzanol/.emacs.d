(setq st-type-alist nil)

(setq st-dir "~/State")


(defun st-save-state (full)
  (let* ((json (json-encode full))
         (id (plist-get full :id))
         (file (file-name-concat st-dir (format "%s.json" id))))

    ;; Insert the updated time
    (plist-put full :updated (format-time-string "%F %T" nil t))

    (write-region json nil file)))

(defun st-load-state (id)
  (if (not (numberp id)) id
    (let* ((file (file-name-concat st-dir (format "%s.json" id)))
           (json-object-type 'plist)
           (json-array-type 'list))
      (if (file-exists-p file)
          (json-read-file file)
        (error "No state file found: %s" file)))))

(defun st-new-state (type state)
  (let* ((time (format-time-string "%F %T" nil t))
         (new (list :type type
                    :state state
                    :id (abs (random))
                    :created time
                    :updated time)))
    (st-save-state new)
    new))

(defmacro st-modify-state (id &rest body)
  `(let* ((=props= (st-load-state ,id))
          (=state= (plist-get =props= :state)))
     ,@body
     (plist-put =props= :state =state=)
     (st-save-state =props=)))


(defmacro st-deftype (name &rest body)
  "Defines a type called NAME which runs BODY on updates.

BODY should return a Dom object. Within BODY, the variable
=state= is bound to the current state value. The value of this
should not be modified by the update function. The variable
=update= is bound to info about why the update is being called."
  (declare (indent 1))

  (let (key val obj plist)
    (while body
      (unless (keywordp (car body)) (error "Expected keyword, found %s" key))
      (setq key (pop body)
            val (pop body)
            obj (cond ((memq key '(:dom :hooks)) `(lambda (=state=) ,val))
                      ((eq key :create) (lambda () ,val))
                      (t val))
            plist (cons key (cons obj plist))))

    `(setf (alist-get ',name st-type-alist) (list . ,plist))))

(defun st-dom (id)
  (let* ((full (st-load-state id))
         (state (plist-get full :state))
         (type (intern (plist-get full :type)))
         (spec (or (alist-get type st-type-alist) (error "Type %s not found in types alist" type)))
         (dom-fn (or (plist-get spec :dom) (error "Type %s has no dom function" spec)))
         (dom (funcall dom-fn state)))

    ;; Add the state id to the beginning of the dom plist
    `(:id ,id . ,dom)))


(defun st--insert-dom (dom &optional id parent pos)
  (setq id (or (plist-get dom :id) id))
  (let ((ref (list :id id :dom dom :overlays nil :children nil :parent parent :pos pos))
        (start (point))
        (on-edit (plist-get dom :edit))
        o)

    ;; Add the ref to the list of children of the ref's parent
    (when parent (plist-put parent :children (cons ref (plist-get parent :children))))

    ;; Add the ref to the list of refs only if it is a top level dom
    (when (plist-get dom :id)
      (puthash id (cons ref (gethash id st-refs)) st-refs))

    (pcase (plist-get dom :type)
      ('const (insert (plist-get dom :content)))
      ('text (insert (plist-get dom :content)))
      ('div (let ((children (plist-get dom :children)))
              (dotimes (i (length children))
                (insert "\n") (backward-char)
                (st--insert-dom (nth i children) id ref i)
                (forward-char))))
      (_ (error "Invalid dom: %s" dom)))

    ;; If the overlay can be edited, it should expand
    (setq o (make-overlay start (point) nil (not on-edit) on-edit))
    (overlay-put o 'ref ref)

    (when on-edit
      (overlay-put o 'on-edit on-edit)
      (overlay-put o 'face '(:slant italic))
      (overlay-put o 'modification-hooks '(st--modification-hook))
      (overlay-put o 'insert-in-front-hooks '(st--modification-hook))
      (overlay-put o 'insert-behind-hooks '(st--modification-hook)))

    ;; Add the overlay to the ref list of overlays
    (plist-put ref :overlays (cons o (plist-get ref :overlays)))

    ;; Return the ref
    ref))

(defun st--insert (id)
  (setq-local st-id id)

  (let ((dom (st-dom id))
        (inhibit-modification-hooks t)
        (inhibit-read-only t)
        (line (line-number-at-pos))
        (col (current-column)))

    ;; Remove the previous contents of the buffer
    (delete-region (point-min) (point-max))
    (remove-overlays)

    (setq-local st-ref (st--insert-dom dom))

    (goto-char (point-min))
    (forward-line line)
    (forward-char col)))

(defun st--replace (id ref)
  (let* ((inhibit-modification-hooks t)
         (inhibit-read-only t)
         (o (car (plist-get ref :overlays)))
         (end (overlay-end o))
         (children (list ref))
         (parent (plist-get ref :parent))
         new-ref)

    (with-current-buffer (overlay-buffer o)
      (save-excursion
        (goto-char (overlay-start o))

        ;; Delete all the overlays and children's overlays
        (while children
          (mapc #'delete-overlay (plist-get (car children) :overlays))
          (nconc children (plist-get (car children) :children))
          (setq children (cdr children)))

        (delete-region (point) end)
        (setq new-ref (st--insert-dom (st-dom id)))
        (plist-put parent :children
                   (--map (if (eq it ref) new-ref it)
                          (plist-get parent :children)))))))

(defun st--modification-hook (o after &rest args)
  (-when-let*
      ((is-after? after)
       (on-edit (overlay-get o 'on-edit))
       ((&plist :id id :overlays os) (overlay-get o 'ref))
       (str (mapconcat (lambda (o) (buffer-substring-no-properties
                                    (overlay-start o) (overlay-end o)))
                       (reverse os) "\n"))

       (full (st-load-state id))
       (state (plist-get full :state))
       (root-ref (st--root-ref (overlay-get o 'ref))))

    (setq state (funcall on-edit state str))
    (plist-put full :state state)
    (st-save-state full)

    (st--clear-dead-refs)
    (dolist (old-ref (gethash id st-refs))
      (unless (eq old-ref root-ref)
        (st--replace id old-ref)))))

(defun st--root-ref (ref)
  (let* ((id (plist-get ref :id))
         (parent (plist-get ref :parent))
         (parent-id (plist-get parent :id)))
    (if (and parent (eq id parent-id))
        (st--root-ref parent)
      ref)))

;;; Refs
(setq st-refs (make-hash-table))

(defun st--clear-dead-refs ()
  "Garbage collect all refs which do not have an active first overlay."
  (dolist (k (hash-table-keys st-refs))
    (puthash k (--filter (let ((overlays (plist-get it :overlays)))
                           (and overlays (overlay-buffer (car overlays))))
                         (gethash k st-refs))
             st-refs)))


(defmacro st:div (&rest children)
  `(list :type 'div :children (list . ,children)))

(defmacro st:label (text &rest face)
  (let ((c (pcase (length face)
             (0 text)
             (1 `(propertize ,text 'face ',(car face)))
             (_ `(propertize ,text 'face ',face)))))
    `(list :type 'const :content ,c)))

(defmacro st:text (location)
  `(list :type 'text
         :content ,location
         :edit (lambda (=state= =text=)
                 (setf ,location =text=)
                 =state=)))

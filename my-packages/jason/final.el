(defun st--format (id-or-dom &optional prev-ids parent-ref parent-pos)
  "Returns a new Format for state object ID-OR-DOM.
If ID-OR-DOM is a number, it is used as an id in the st-state hashmap.
If it is a Dom, the id will be inherited from the parent ref (it mustn't be nil)
This is because not every dom is associated with an id.

PARENT-REF is the reference to the parent format, or nil."

  (let* ((id (if (numberp id-or-dom) id-or-dom (plist-get parent-ref :id)))
         (dom (if (numberp id-or-dom) (st--generate-dom id-or-dom) id-or-dom))
         (new-ref (list :id id :dom dom :overlays nil :parent parent-ref :parent-pos parent-pos))
         (all-ids (cons id prev-ids)))

    ;; If the id was nil, throw an error
    (unless id (error "No parent id with dom '%s' and parent '%s'" id-or-dom parent-ref))

    ;; If there is an infinite loop, break it
    (if (and (numberp id-or-dom) (memq id-or-dom prev-ids))
        (list (format "Infinite Loop: %s" id-or-dom))

      ;; Add the ref to the list of refs
      (puthash id (cons new-ref (gethash id st-refs)) st-refs)

      (pcase (plist-get dom :type)
        ('const (--map (list new-ref it) (split-string (plist-get dom :content) "\n")))

        ('text (--map (list new-ref (propertize it 'face '(:box t)))
                      (split-string (plist-get dom :content) "\n")))

        ('div (let* ((children (plist-get dom :children))
                     (ls (--map (st--format (nth it children) all-ids new-ref it)
                                (number-sequence 0 (1- (length children)))))
                     (all-lines (-flatten-n 1 ls))
                     (prefix (alist-get 'div-prefix all-vars))
                     (prefix-list (when prefix (list prefix))))

                (--map `(,new-ref ,@prefix-list ,it) all-lines)))))))


;; Insert a format
(defun st-insert (id)
  (interactive (list st-root))

  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)

        (line (line-number-at-pos))
        (col (current-column))
        prop o change-func modify-func)

    ;; Remove the previous contents of the buffer
    (delete-region (point-min) (point-max))
    (remove-overlays)

    ;; Insert the new dom
    (dolist (line-format (st--format id))
      (insert "\n")
      (goto-char (1- (point)))
      (st--insert-format line-format)
      (goto-char (1+ (point))))

    (put-text-property (point-min) (point-max) 'read-only t)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'st-on-edit)
        (put-text-property (overlay-start o) (overlay-end o) 'read-only nil)))


    ;; Go back to the original position
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))

(defun st--insert-format (format)
  (let ((start (point))
        (ref (car format))
        o)

    (dolist (elem (cdr format))
      (if (stringp elem)
          (insert (propertize elem 'st ref))
        (st--insert-format elem)))

    (setq o (make-overlay start (point) nil nil 'rear-advance))
    (overlay-put o 'st ref)
    ;; Add the overlay to the ref
    (plist-put ref :overlays (cons o (plist-get ref :overlays)))

    ;; Possibly add modification hooks
    (when-let ((dom (plist-get ref :dom))
               (edit (plist-get dom :on-edit)))

      (overlay-put o 'st-on-edit edit)

      (overlay-put o 'modification-hooks '(st--modification-hook))
      (overlay-put o 'insert-in-front-hooks '(st--modification-hook))
      (overlay-put o 'insert-behind-hooks '(st--modification-hook)))))



;;; Interning a state tree into a hash table

;; ID (number) => STATE
(setq st-state (make-hash-table))

;; ID (number) => REFS
(setq st-refs (make-hash-table))

;; Id of the root state
(setq st-root nil)

(defun st--intern (object)
  "Intern an object into st-state, with st-root as the root."
  (setq st-state (make-hash-table))
  (setq st-refs (make-hash-table))
  (setq st-root (st--intern-state object st-state)))

(defun st--intern-state (state ht)
  "Intern STATE, returning the interned version."
  (if (not (and (listp state) (null (cdr (last state))))) state
    (setq state (--map (st--intern-state it ht) state))

    (if (not (eq (car state) :type)) state

      (let ((id (abs (random))))
        (puthash id state ht)
        id))))

(defun st--clear-dead-refs ()
  "Garbage collect all refs which do not have an active first overlay."
  (interactive)
  (dolist (k (hash-table-keys st-refs))
    (puthash k (--filter (let ((overlays (plist-get it :overlays)))
                           (and overlays (overlay-buffer (car overlays))))
                         (gethash k st-refs))
             st-refs)))

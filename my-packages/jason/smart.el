;; A State is a unit of state which can be converted into a dom


;; A Dom is a unit of display. Its children, instead of being other
;; Doms, are actually States.

;; Dom ::=
;; (:type text :content STRING :styles? [STYLE] :on-edit? [HOOK])
;; (:type const :content STRING :styles? [STYLE] :on-click? [HOOK])
;; (:type div :children [STATE-ID])
;; (:type table :cells [[STATE-ID]] :rows [ROW] :columns [COLUMN])



;;; Alternative buffer

(setq st--command-advised nil)

(defun st--update-text-box (string ref)
  (st--update-property (plist-get ref :id) :content string))



;;; Formatting

;; Returns a list of Formats
;; Format ::= string | (Ref Formats...)
(defun st--format (id-or-dom &optional prev-ids vars parent-ref parent-pos)
  "Returns a new Format for state object ID-OR-DOM.
If ID-OR-DOM is a number, it is used as an id in the st-state hashmap.
If it is a Dom, the id will be inherited from the parent ref (it mustn't be nil)
VARS is an alist of variables to inherit from.
PARENT-REF is the reference to the parent format, or nil."

  (let* ((id (if (numberp id-or-dom) id-or-dom (plist-get parent-ref :id)))
         (dom (if (numberp id-or-dom) (st--generate-dom id-or-dom) id-or-dom))
         (new-ref (list :id id :dom dom :overlays nil :parent parent-ref :parent-pos parent-pos))
         (all-vars (append (plist-get dom :vars) vars))
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
                     (ls (--map (st--format (nth it children) all-ids all-vars new-ref it)
                                (number-sequence 0 (1- (length children)))))
                     (all-lines (-flatten-n 1 ls))
                     (prefix (alist-get 'div-prefix all-vars))
                     (prefix-list (when prefix (list prefix))))

                (--map `(,new-ref ,@prefix-list ,it) all-lines)))))))

(defun st--generate-dom (id)
  (let ((state (gethash id st-state)))
    (pcase (plist-get state :type)

      ('rows (let* ((divide `(:type const :content ,(plist-get state :divider)))
                    (divide-div `(:type div :children (,divide)))
                    (cs (plist-get state :children)))
               (list :type 'div
                     :vars `((div-prefix . ,(plist-get state :prefix)))
                     :children (cons divide (-flatten-n 1 (--map (list it divide-div) cs))))))

      ('text (list :type 'text :on-edit 'st--update-text-box
                   :content (plist-get state :content)
                   :styles (plist-get state :styles))))))


;;; Inserting

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

(defun st--modification-hook (o after &rest args)
  (-when-let* ((is-after? after)
               (on-edit (overlay-get o 'st-on-edit))
               (ref (overlay-get o 'st))
               (os (plist-get ref :overlays))
               (str (mapconcat (lambda (o) (buffer-substring-no-properties
                                            (overlay-start o) (overlay-end o)))
                               (reverse os) "\n")))

    (funcall on-edit str ref)

    (st-insert st-root)))


;;; Updating state
(defun st--update-state (id func)
  (let* ((current (gethash id st-state))
         (new (funcall func current)))
    (puthash id new st-state)

    ;; Update all references......
    ))

(defun st--update-property (id prop val)
  (let* ((func `(lambda (state) (plist-put state ',prop ',val))))
    (st--update-state id func)))


;;; Highlight current node
(setq st-highlighted-ref nil)

(qv/face st-highlight region :extend t)

(defun st-highlight-current ()
  (let ((current-ref (get-text-property (point) 'st)))
    (unless (eq current-ref st-highlighted-ref)

      ;; Un-highlight the previous ref
      (dolist (o (plist-get st-highlighted-ref :overlays))
        (overlay-put o 'face nil))

      ;; Highlight the current ref
      (dolist (o (plist-get current-ref :overlays))
        (overlay-put o 'face 'st-highlight))

      (setq st-highlighted-ref current-ref))))

(define-minor-mode st-highlight-mode
  "Highlight the node at the current cursor position."
  :global nil
  :init-value nil
  (if st-highlight-mode
      (add-hook 'post-command-hook 'st-highlight-current nil 'local)
    (remove-hook 'post-command-hook 'st-highlight-current 'local)
    (dolist (o (plist-get st-highlighted-ref :overlays))
      (overlay-put o 'face nil))))


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

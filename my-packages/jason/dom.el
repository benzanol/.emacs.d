;; (:type table :rows ROWS :columns COLS :cells CELLS ... ...)


;; All DOMs can have an additional :vars ALIST property
;; DOM ::=
;; (:type text :string STRING :styles STYLES)
;; (:type div :children CHILDREN :indent? STRING :wrap? STRING)
;; (:type table :cells CELLS :rows ROWS :columns COLUMNS) 

;; ID(NUM) -> (:object DOM :refs REFS)
;; Ref -> (:id ID :parent PARENT/BUFFER :overlays (OVERLAYS...))
;; ID references the object to which the ref belongs
;; PARENT is either another REF, or the buffer its in if it is the root node.
(setq st-objects (make-hash-table))

(setq st-pixel nil)

(defun st-string-width (str)
  (cond ((not pixel) (string-width str))
        ((boundp 'string-width) (string-width str))
        (t (require 'shr) (shr-string-pixel-width str))))

;; Remove all references in which the first overlay is deleted from `st-objects`
(defun st-clear-old-refs ()
  ;; Function to check if a reference is still alive in a buffer
  (let ((live-func (lambda (r) (overlay-live-p (car (plist-get r :overlays))))))
    ;; Remove all live references from every value in the hashmap
    (dolist (v (hash-table-values st-objects))
      (plist-put v :refs (seq-filter live-func (plist-get v :refs))))))



;; Insert a particular object
(defun st-insert (id &optional parent vars)
  (let* ((entry (gethash id st-objects))
         (obj (plist-get entry :object))
         (refs (plist-get entry :refs))
         (start (point))
         ol overlays new-ref)

    ;; Call the function that does the inserting
    (funcall (st-insert-function obj) obj vars)

    ;; Insert the overlays
    (goto-char start)
    (while (not (eobp))
      (setq ol (make-overlay (point) (point-at-eol)))
      (overlay-put ol 'st id)
      (push ol overlays))

    ;; Add a ref to the object
    (setq new-ref (list :id id
                        :parent (or parent (current-buffer))
                        :overlays overlays))
    (plist-put entry :refs (cons new-ref refs))))

;; Return the name of the function used to insert a particular object
(defun st-insert-function (obj)
  (intern (format "st-insert:%s" (plist-get :type obj))))


(defun st-insert:text (obj vars)
  (insert (propertize (plist-get :text obj)
                      'face (list :box t)))

(defun st-insert:div (obj vars)
  (insert #("Div:" 0 4 (face bold)))
  (newline)
  (let ((start (point))
        (indent (or (plist-get obj :indent) ""))
        (wrap (or (plist-get obj :wrap) ""))
        at-end)
    ;; Insert the children
    (dolist (child-id (plist-get :children obj))
      (st-insert child-id vars)
      (newline))
    ;; Delete the last newline
    (delete-backward-char 1)

    ;; Add indent/wrap
    (goto-char start)
    (insert indent)
    (while (eq (forward-line 1) 0) (insert wrap)))))

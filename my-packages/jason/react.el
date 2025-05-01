;; package --- Structural Editing in Emacs -*- lexical-binding: t; -*-

(require 'dash)
(require 'text-property-search)


;; Dom ::=
;; (:type text :content STRING :styles? [STYLE] :on-edit? [HOOK])
;; (:type const :content STRING :styles? [STYLE] :on-click? [HOOK])
;; (:type div :children [DOM])
;; (:type table :cells [[DOM]] :rows [ROW] :columns [COLUMN])

;; Ref ::= (:id STATE-ID :overlays [OVERLAY])

;; State ::= (:dom DOM :refs [REF])



;;; Formatting


(defun st-insert (id)
  (let ((line (line-number-at-pos))
        (col (current-column))
        (inhibit-modification-hooks t)
        (inhibit-read-only t)

        bol prop o func modify-func intervals)

    ;; Remove the previous contents of the buffer
    (delete-region (point-min) (point-max))
    (remove-overlays)

    ;; Insert the new dom
    (dolist (line (st--format id))
      ;; Add overlays where there are id text properties
      (st--insert-format line)
      (insert "\n"))

    ;; Go back to the original position
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))


;; FORMAT ::= STRING | (REF FORMATS...)
;; => [FORMAT]
(defun st--format (id &optional vars parent)
  (let* ((state (gethash id st-state))
         (dom (plist-get state :object))
         (new-ref (list :id id :overlays nil :parent parent))
         (all-vars (append (plist-get dom :vars) vars)))

    (pcase (plist-get dom :type)
      ('const (--map (list new-ref it)
                     (split-string (plist-get dom :content) "\n")))

      ('text (--map (list new-ref it)
                    (split-string
                     (propertize (plist-get dom :content)
                                 'face (list :box t)
                                 ;; Reference the plist itself stored in dom
                                 'modify `(lambda (s) (plist-put ',dom :content s)))
                     "\n")))

      ('div (let* ((lines (-flatten-n 1 (--map (st--format it all-vars new-ref)
                                               (plist-get dom :children))))
                   (prefix (alist-get 'div-prefix all-vars))
                   (prefix-list (when prefix (list prefix))))

              (--map `(,new-ref ,@prefix-list ,it) lines))))))

(defun st--insert-format (format)
  (if (stringp format) (insert format)
    (let ((start (point))
          (ref (car format))
          o)
      (mapc #'st--insert-format (cdr format))
      (setq o (make-overlay start (point)))
      (overlay-put o 'st ref)
      ;; Add the overlay to the ref
      (plist-put ref :overlays (cons o (plist-get ref :overlays))))))



;;; Interning

;; ID (number) => STATE
(setq st-state (make-hash-table))

;; Id of the root state
(setq-local st-root nil)

(defun st-intern (object)
  (setq st-state (make-hash-table))
  (setq-default st-root (st--intern-object object st-state)))

(defun st--intern-object (object ht)
  (if (not (and (listp object) (null (cdr (last object))))) object
    (setq object (--map (st--intern-object it ht) object))

    (if (not (eq (car object) :type)) object

      (let ((id (abs (random)))
            (state (list :object object :refs nil)))
        (puthash id state ht)
        id))))



(defmacro st-deftype (name &rest props)
  (declare (indent 1))
  `(setf (alist-get ',name st-type-alist) ',props))
(defun st-read-param (param)
  (cond ((plist-get (cdr param) :default))
        (t (read-string (capitalize (substring (format "%s: " (car param)) 1))))))
(defun st-create-state (type &rest plist)
  (interactive
   (let ((type (intern (completing-read "Type: " (mapcar #'car st-type-alist))))
         (type-spec (alist-get name st-type-alist))
         (params (->> (plist-get spec :params)
                      (--map (list (car it) (st-read-param it)))
                      (-flatten-n 1))))
     (cons type plist)))

  (let ((s (list :type type
                 :content plist
                 :id (abs (random))
                 :created (format-time-string "%F %T" nil t))))
    (st-save-state s)
    s))


;; ;; Add modification hooks using overlays
;; (goto-char (point-min))
;; (while (setq prop (text-property-search-forward 'modify))
;;   (setq o (make-overlay (prop-match-beginning prop)
;;                         (prop-match-end prop)
;;                         nil nil 'rear-advance))

;;   (setq func `(lambda (ol &rest args)
;;                 (message "func")
;;                 (,(prop-match-value prop)
;;                  (buffer-substring-no-properties
;;                   (overlay-start ol) (overlay-end ol)))
;;                 (st-insert ',dom))

;;         modify-func
;;         `(lambda (ol after &rest args)
;;            (message "mofiyf")
;;            (when after (,func ol))))

;;   ;; (overlay-put o 'insert-in-front-hooks (list func))
;;   ;; (overlay-put o 'insert-behind-hooks (list func))
;;   (overlay-put o 'modification-hooks (list modify-func)))


;; Inserting overlays with id text properties
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
    (dolist (line (st-format id nil))
      (setq bol (point))
      (insert (concat line "\n"))

      ;; Add overlays where there are id text properties
      (dolist (interval (object-intervals line))
        (-let (((start end (prop ref)) interval))

          ;; If the property is a number, it is a state id
          (when (numberp prop)

            ;; Create an overlay which contains the ref
            (setq o (make-overlay (+ bol start) (+ bol end)))
            (overlay-put o 'st ref)

            ;; Add the overlay to the ref
            (plist-put ref :overlays (cons o (plist-get ref :overlays)))))))


    ;; Go back to the original position
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))







;; Switch to a temp buffer for editing
(setq st--command-advised nil)
(qv/advise :remove command-execute st--command-advice (func &rest args)
  (catch 'return

    ;; If a command was previously advised, don't advise again.
    (when st--command-advised (apply func args) (throw 'return nil))


    (let* ((st--command-advised t)
           (prev-buf (current-buffer))
           (prev-win (selected-window))
           (ref (or (get-text-property (point) 'st) (get-text-property (max 1 (1- (point))) 'st)))
           (dom (plist-get ref :dom))
           (pos (point))
           (buf-text (buffer-string))
           os on-edit after-vars last-line last-pos)

      ;; If not a text node, eval immediately
      (if (not (eq 'text (plist-get dom :type)))
          (progn (apply func args) (throw 'return nil))

        ;; Set up the temp buffer
        (set-buffer "*dom-text*")
        (delete-region (point-min) (point-max)) (remove-overlays)
        (insert buf-text) (goto-char pos)
        (setq-local goal-column (current-column))

        (setq os (--map (make-overlay (1+ (overlay-start it)) (1+ (overlay-end it)) nil nil t)
                        (plist-get ref :overlays)))
        (move-overlay (car os) (overlay-start (car os)) (1- (overlay-end (car os))))

        ;; Execute the command
        (apply func args)

        ;; Make sure still in the buffer and window
        (unless (and (eq (current-buffer) (get-buffer "*dom-text*"))
                     (eq (selected-window) prev-win))
          (throw 'return nil))

        ;; Delete non-overlay text
        (save-excursion
          (goto-char (point-min))
          (dolist (o (--sort (< (overlay-start it) (overlay-start other)) os))
            (setq last-pos (point))
            (goto-char (overlay-start o))
            (when (> (point) last-pos) (delete-region last-pos (point)))
            (goto-char (overlay-end o)))
          (delete-region (point) (point-max))

          (goto-char (point-min))
          (while (bolp) (insert " ") (forward-line)))

        ;; Save the point and mark
        (remove-text-properties (point-min) (point-max) '(st nil st-point-col nil st-mark-col nil))
        (put-text-property (point-at-bol) (point-at-eol) 'st-point-col (max 1 (current-column)))
        ;; (goto-char (mark))
        ;; (put-text-property (point-at-bol) (point-at-eol) 'st-mark-col (current-column))

        ;; Save the buffer contents and other variables
        (setq after-string (buffer-string)
              after-vars (--map (cons it (if (consp it) (funcall (car it)) (symbol-value it)))
                                '(cursor-type)))
        (when (string= after-string "") (message "FAIL!!"))

        ;; Go back to the previous buffer
        (set-buffer prev-buf)

        ;; Insert the tree
        (setq on-edit (plist-get dom :on-edit))
        (when on-edit (funcall on-edit after-string ref))
        (st-insert st-root)

        ;; Load variables from the temp buffer
        (dolist (var after-vars)
          (if (consp (car var))
              (funcall (cdar var) (cdr var))
            (set (car var) (cdr var))))

        ;; Set the point and mark
        ;; (beginning-of-buffer)
        ;; (let ((prop (text-property-search-forward 'st-mark-col)))
        ;;   (when prop (push-mark (+ (prop-match-beginning prop) (prop-match-value prop)))))
        (beginning-of-buffer)
        (let ((prop (text-property-search-forward 'st-point-col)))
          (message "poing! %s" prop)
          (when prop (goto-char (+ (prop-match-beginning prop) (prop-match-value prop)))))
        ))))

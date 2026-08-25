;;; lens.el --- interactive stateful widgets for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'org)
(require 'org-element)
(require 'org-indent)
(require 'seq)
(require 'text-property-search)


(defgroup lens nil
  "Customization group for lens.el."
  :prefix "lens-"
  :group 'tools)


;;; ============================================================
;;; Utils
;;;; Log

(et-declare
 (@alias LensLogLevel Integer))

(et-defvar lens-min-log-level LensLogLevel 2)

(defun lens-send-log (level format-str &rest format-args)
  (declare (et (level LensLogLevel) (format-str String) (format-args ListR<Any>)
               (@return String|Nil)))
  (when (>= level lens-min-log-level)
    (let* ((print-level 6)
           (print-length 5)
           (str (apply #'format format-str format-args)))
      (pcase level
        (1 (message "[DEBUG] %s" str))
        (2 (message "[INFO] %s" str))
        (3 (message "%s" (propertize (concat "[WARN] " str) 'face 'warning)))
        (4 (message "%s" (propertize (concat "[ERROR] " str) 'face 'error)))
        (_ (message "[%s] %s" level str))))))

(defmacro lens-log (type &rest forms)
  (declare (et (@expand)))
  (let* ((level (or (pcase type ('debug 1) ('info 2) ('warn 3) ('error 4))
                    (error "Invalid log type: %s" type)))
         (format-args nil)
         (format-str
          (cl-loop for form in forms
                   for sep = "" then "; "
                   concat (cond ((or (stringp form) (numberp form))
                                 (format "%s%s" sep form))
                                ((and (vectorp form) (eq (length form) 2))
                                 (push (aref form 1) format-args)
                                 (format "%s%s=%%s" sep (aref form 0)))
                                (t
                                 (push form format-args)
                                 (format "%s%s=%%s" sep
                                         (if (and (consp form) (cdr form))
                                             (format "(%s...)" (car form))
                                           form)))))))
    `(lens-send-log ,level ,format-str ,@(reverse format-args))))


;;;; Regions

(et-declare
 (@alias LensRegion [T]
         (TupleR T Integer Integer Integer Integer)))

(defun lens--region-search-forward (beg-prop end-prop &optional val pred)
  "Search forward for a region, and return its value and range.

A region consists of a header, a body, and a footer. The header
and footer are regions which have non-nil and equal values of
BEG-PROP and END-PROP respectively. The body is the area in
between the header and footer.

Returns (val, head-start, head-end, foot-start, foot-end), where
val is the value of the region, and the rest are integers
representing the bounds of the region.

VAL and PRED can be used to search for a particular region,
following the same convention as `text-property-search-forward'."
  (declare (et (beg-prop Var) (end-prop Var) (val Any) (pred (or Nil (fn (Args Any Any))))
               (@return Nil|LensRegion<Any>)))

  (when-let* ((cursor-sensor-inhibit t)
              (beg (text-property-search-forward beg-prop val pred))
              (beg-val (prop-match-value beg))
              (end (text-property-search-forward end-prop beg-val #'eq)))
    (list beg-val
          (prop-match-beginning beg) (prop-match-end beg)
          (prop-match-beginning end) (prop-match-end end))))

(defun lens--region-at-point (beg-prop end-prop)
  "Search for a region containing the point.

The bounds of the region are specified by BEG-PROP and END-PROP,
as described by `lens--region-search-forward'."
  (declare (et (beg-prop Var) (end-prop Var)
               (@return Nil|LensRegion<Any>)))
  (let* ((cursor-sensor-inhibit t)
         (start (point))
         (beg-match (et: Nil|*prop-match nil))
         (end-match (et: Nil|*prop-match nil)))
    (save-excursion
      (and (setq end-match (text-property-search-forward end-prop))
           (setq beg-match (text-property-search-backward beg-prop))
           (>= start (prop-match-beginning beg-match))
           (eq (prop-match-value end-match) (prop-match-value beg-match))
           (list (prop-match-value end-match)
                 (prop-match-beginning beg-match) (prop-match-end beg-match)
                 (prop-match-beginning end-match) (prop-match-end end-match))))))


;;;; String width

(defun lens-string-width (str &optional start end)
  "Calculate the width of STR in columns.

The `string-width' function doesn't work correctly in certain situations
for some reason. For example, it believes an em-dash has width 1."
  (declare (et (str String) (start Nil|Integer) (end Nil|Integer)
               (@return Integer)))
  (if start (string-width str start end)
    (let* ((inhibit-read-only t))
      (/ (string-pixel-width str) (string-pixel-width "a")))))


;;;; Prepend prefix

(defun lens--set-string-prefix (id-prop id-val string prefix)
  "Add PREFIX as a prefix to STRING, with text property ID-PROP=ID-VAL.

If STRING already contains a prefix with the specified id prop and
value, remove it."
  (declare (et (id-prop Var) (id-val Any) (string String) (prefix String)
               (@return String)))
  (if (null string) (propertize prefix id-prop id-val)
    (with-temp-buffer
      (insert string)
      (let ((match (text-property-search-backward id-prop id-val #'eq)))
        (concat (propertize prefix id-prop id-val)
                (if match (substring string (prop-match-end match)) string))))))


;;;; Make sticky

(defun lens--make-sticky (str &optional beg end)
  "Set the stickiness properties of the edges of STR.

The beginning will be sticky if BEG is non-nil, and the end will
be sticky if END is non-nil. Thus, the default behavior is
actually to make both ends of STR not sticky."
  (declare (et (str String) (beg Nil|Integer) (end Nil|Integer)
               (@return String) (@skip)))
  (put-text-property 0 1 'front-sticky beg str)
  (put-text-property (1- (length str)) (length str) 'rear-nonsticky (not end) str)
  str)


;;;; Append face

(defun lens--append-face (beg end face &optional object)
  "Append FACE to both the `face' and `font-lock-face' properties.

If the face already exists, it will not be added.

BEG and END are the beginning and end of the region the face is
added to. If OBJECT is a string, add the properties to it instead
of the buffer."
  (declare (et (beg Integer) (end Integer) (face Var) (object Nil|String)))
  (when face
    (let ((fn `(lambda (f)
                 (cond ((eq ',face f) f)
                       ((or (not (listp f)) (keywordp (car f))) (list f ',face))
                       ((memq ',face f) f)
                       ((append f (list ',face)))))))
      (alter-text-property beg end 'face fn object)
      (alter-text-property beg end 'font-lock-face fn object))))


;;;; Save position

(defmacro lens-save-position (&rest body)
  "Save the line and column of the cursor when executing BODY."
  (declare (et (@progn)))
  `(let* ((line (line-number-at-pos)) (col (current-column)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char (min col (- (pos-eol) (point))))))


;;;; Force edit

(defmacro lens-force-edit (&rest body)
  "Perform the changes in BODY without any limitations."
  (declare (et (@progn)))
  `(let ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (cursor-sensor-inhibit t))
     (combine-after-change-calls
       (atomic-change-group ,@body))))


;;;; Batch post command

(et-declare
 (@alias LensBatchCommand [T] (Function (Args (ListR Any)) Nil)))

(et-defvar lens--batch-post-commands AList<LensBatchCommand<Any>~List<Any>>
           nil
  "An alist mapping functions to lists of arguments.

See `lens-batch-post-command'.")

(defun lens-batch-post-command (command arg)
  "Call COMMAND in a post command with ARG as one of its arguments.

Each call to `lens-batch-post-command' with a particular COMMAND and arg
is associated with a single entry in the cdr of COMMAND. So, if 3 calls
are made with COMMAND, the cdr will be a list of 3 elements, the 3
arguments (in reverse order).

Each command will be called once. Its argument will be the list of
args, (in the order that they were inserted, which is the reverse of
what was stored.)"
  (declare (et (@generics [T])
               (command LensBatchCommand<T>)
               (arg T)
               (@return Nil)))

  (push arg (alist-get command lens--batch-post-commands))
  (add-hook 'post-command-hook #'lens--batch-post-command-callback)
  nil)

(defun lens--batch-post-command-callback ()
  (declare (et (@return Nil)))
  (remove-hook 'post-command-hook #'lens--batch-post-command-callback)
  (let* ((cmds (nreverse lens--batch-post-commands)))
    (setq lens--batch-post-commands nil)

    (dolist (command cmds)
      (with-demoted-errors "Error in batch command: %S"
        (funcall (car command) (nreverse (cdr command)))))))


;;;; Let macro

(defmacro lens-let-body (&rest body)
  "A macro designed to make ui component bodies simpler.

If any of body is (LET-KEYWORD VAR VAL), then this will be compiled to a
let expression containing the rest of the body. LET-KEYWORD can be any
of `:let', `:flet', `:when-let', or `:pcase-let'.

So,
  (:let a 4)
  (:when-let b (1+ a))
  (+ a b)
will be converted to
  (let ((a 4))
    (when-let ((b (1+ a)))
      (+ a b)))

Furthermore, the car of body can be a vector containing custom symbols
to convert to special forms. Each entry should be a cons cell mapping
the desired symbol to a function. When the symbol is used as a function
call in the top level of BODY, this function will be called at compile
time to generate the new expression.

The first argument is a list of Lisp expressions, which is the result of
applying `lens-let-body' to the remaining body. The remaining arguments
are the list of arguments passed to the call. The return value is a list
of Lisp expressions to replace the original body.

For example, the following is expression used for use state:
  [(:use-state
    (lambda (body state-var set-var)
      `((let* ((=use-state= (lens--use-state))
               (,state-var (car =use-state=)))
          (cl-flet ((,set-var (cdr =use-state=)))
            ,@body)))))]

This allows for the following syntax:
  (:use-state count set-count)
  (set-count (1+ count))"
  (declare (et (@expand)))

  (let ((let-keywords
         '((:let . let)
           (:flet . cl-flet)
           (:when-let . when-let)
           (:pcase-let . pcase-let)))
        (custom (when (vectorp (car body)) (append (pop body) nil)))
        exprs func lets)
    ;; Go through the expressions in reverse
    (dolist (line (reverse body))
      (cond ((not (listp line)) (push line exprs))
            ((eq (car line) :let)
             (push (cadr line) lets)
             (push `(setq ,@(cdr line)) exprs))
            ((setq func (alist-get (car line) let-keywords))
             (setq exprs `((,func (,(cdr line)) ,@exprs))))
            ((setq func (alist-get (car line) custom))
             (setq exprs (apply func exprs (cdr line))))
            (t (push line exprs))))
    (cond (lets `(let ,lets ,@exprs))
          ((> (length exprs) 1) (cons #'progn exprs))
          (t (car exprs)))))


;;;; Named lambda

(et-declare
 (@checker lens-named-lambda (name &rest args)
           (let* ((type (if (eq 1 (length args)) (et-checker-sub 2)
                          (et-checker-expansion (cons #'lambda args)
                                                et--checker-recommendation))))
             (if (et-subtype? type (et AnyFn)) type
               (et-fatal (1+ (length args)) "Not a function: %s" type)))))

(defmacro lens-named-lambda (name &rest args)
  (declare (indent 2))
  `(let* ((symbol (make-symbol (format "%s" ,name))))
     (fset symbol ,(if (eq (length args) 1) (car args) (cons #'lambda args)))
     symbol))


;;;; Key bindings

(defun lens-keys (&rest args)
  (declare (et (args ListR<Any>) (@return Keymap)))
  (let ((keymaps (et: List<Keymap> nil))
        (cur-keymap (make-sparse-keymap)))
    (while-let ((next (pop args)))
      (if (not (keymapp next))
          (if (not (and next (car args))) (pop args)
            (define-key cur-keymap (if (stringp next) (kbd next) (pop args)) (pop args)))
        (when (cdr cur-keymap) (push cur-keymap keymaps) (setq cur-keymap (make-sparse-keymap)))
        (push next keymaps)))
    (when (cdr cur-keymap) (push cur-keymap keymaps))
    (if (eq (length keymaps) 1) (car keymaps)
      (cons 'keymap (nreverse keymaps)))))


;;; ============================================================
;;; Lifecycle
;;;; Create

(et-declare
 ;; Events
 (@alias LensEvent (ConsR Symbol ListR<Any>))
 (@alias LensEmitter (fn (Args LensEvent)))
 ;; Host
 (@alias LensHostEdit (fn (Args fn) Nil))
 (@alias LensHost (PList :host-edit LensHostEdit
                         :host-close fn))
 ;; Model
 (@alias LensModel
         (PList :model-on-close Nil|fn
                :model-on-save Nil|fn))
 ;; View
 (@alias LensViewInsert fn)
 (@alias LensViewUpdate (fn LensEvent))
 (@alias LensViewOnClose fn)
 (@alias LensView (PList :view-insert LensViewInsert
                         :view-update LensViewUpdate
                         :view-on-close Nil|LensViewOnClose))
 ;; Lens
 (@alias Lens
         (PList :id String
                :host LensHost
                :model LensModel
                :view LensView)))

(defun lens--init (fn &rest args)
  "Call FN with ARGS.

FN can also be a list (FUNCTION EXTRA-ARGS...). In this case, EXTRA-ARGS
will be provided as additional arguments to FUNCTION appended to the end
of ARGS."
  (declare (et (@generics [A R]) (fn (fn A R)) (args A)
               (@return R)))
  (pcase fn
    ((pred functionp) (apply fn args))
    (`(,(and (pred functionp) f) . ,extra) (apply f (append args extra)))
    (_ (error "Invalid function %s" fn))))

(defun lens-create (host-fn model-fn view-fn)
  "Insert a new lens into the current buffer."
  (declare (et (host-fn (fn (Args Lens) LensHost))
               (model-fn (fn (Args LensEmitter) LensModel))
               (view-fn (fn (Args LensEmitter LensModel) LensView))
               (@return Nil)))

  (let* ((id (format "%05d" (mod (random) (expt 10 6))))
         (lens (et! Lens (list :id id :region nil :model nil :view nil)))

         ;; Initialize the host
         (host (lens--init host-fn lens))
         (success (et: Boolean nil))
         (view (et! LensView nil)))

    (plist-put lens :host host)

    (unwind-protect
        (let* ((view-update (et! LensViewUpdate nil))
               (host-edit
                (et! LensHostEdit
                  (lens-named-lambda (format "lens-%s:host:edit" id)
                                     (or (plist-get host :host-edit) (error "Host must have an edit property")))))

               ;; Initialize the model
               (model-emit (et! LensEmitter
                             (lens-named-lambda (format "lens-%s:model-emit" id) (event)
                                                (lens-log debug "Model emit" id event)
                                                (let* ((update (lambda () (apply view-update event))))
                                                  (lens--emit (lambda () (funcall host-edit update)))))))
               (model (lens--init model-fn model-emit))
               (_ (plist-put lens :model model))

               ;; Initialize the view
               (view-emit (et: LensEmitter
                            (lens-named-lambda (format "lens-%s:view-emit" id) (event)
                                               (lens-log debug "View emit" id event)
                                               (let* ((update (lambda () (apply view-update event))))
                                                 (lens--emit (lambda () (funcall host-edit update)))))))
               (_ (plist-put lens :view (setq view (lens--init view-fn view-emit model))))

               (view-insert (et: LensViewInsert
                              (lens-named-lambda (format "lens-%s:view:insert" id)
                                                 (or (plist-get view :view-insert) (error "View must have an insert property"))))))

          (setq view-update
                (et: LensViewUpdate
                  (lens-named-lambda (format "lens-%s:view:update" id)
                                     (or (plist-get view :view-update) (error "View must have an update property")))))

          ;; Open up the host, inserting the view inside of it
          (lens--track-point (lambda () (funcall host-edit view-insert)))

          (setq success t)
          nil)

      ;; If something failed, close the host
      (unless success
        (funcall (plist-get host :host-close))))))


;;;; Track point

(et-defvar lens-point Marker|Nil nil
  "The point when an event started being handled.

This is defined when an event is being handled. It is initially set to
the current point, for reference by update functions. Update functions
can also modify this value, which will set the position of the point
after the event has been handled.")

(defun lens--track-point (fn)
  (declare (et (@generics [R]) (fn (fn Nil R)) (@return R)))
  (let* ((lens-point (set-marker (make-marker) (point))))
    (unwind-protect (funcall fn)
      (when lens-point (goto-char lens-point)))))


;;;; Emit

(et-defvar lens--emit-queue List<fn> nil
  "Queue of event functions to be called.")

(defun lens--emit (fn)
  (declare (et (fn fn) (@return Nil)))
  (if lens--emit-queue
      (nconc lens--emit-queue (list fn))

    (setq lens--emit-queue (list fn))
    (unwind-protect
        (while lens--emit-queue
          (unwind-protect (lens--track-point (car lens--emit-queue))
            (pop lens--emit-queue)))

      (setq lens--emit-queue nil)))
  nil)


;;;; Remove

(defun lens-remove (lens)
  "Delete the lens at point."
  (declare (et (lens Lens) (@return Nil)))
  (interactive (list (or (car (lens-at-point)) (error "No lens at point"))))

  (funcall (or (plist-get (plist-get lens :model) :model-on-close) #'ignore))
  (funcall (plist-get (plist-get lens :host) :host-edit)
           (or (plist-get (plist-get lens :view) :view-on-close) #'ignore))
  (funcall (plist-get (plist-get lens :host) :host-close))
  nil)

(defun lens-remove-all ()
  "Remove all lenses in the current buffer."
  (declare (et (@return Nil)))
  (interactive)
  (dolist (lens (lenses-in-buffer))
    (with-demoted-errors "Error removing lens: %s"
      (lens-remove lens))))


;;;; Save

(defun lens-save (lens)
  "Call the save function of the model of the lens at point."
  (declare (et (lens Lens) (@return Nil)))
  (interactive (list (car (lens-at-point))))
  (funcall (or (plist-get (plist-get lens :model) :model-on-save) #'ignore))
  nil)

(defun lens-save-all ()
  "Save all lenses in the current buffer."
  (declare (et (@return Nil)))
  (interactive)
  (dolist (lens (lenses-in-buffer))
    (with-demoted-errors "Error saving lens: %s"
      (lens-save lens))))


;;; ============================================================
;;; Hosts
;;;; Lens at point

(et-defvar lens-at-point-functions (List (fn Nil Nil|LensRegion<Lens>)) nil
  "A list of functions for determining the lens at point.

Each host type should define its own function for determining if it
exists at a particular place, and add it to this list in the host
creation function.")

(defun lens-at-point (noerror)
  (declare (et (@generics [N])
               (noerror N)
               (@return (or LensRegion<Lens> (if-nil? N Never Nil)))))
  (or (seq-some #'funcall lens-at-point-functions)
      (unless noerror (error "No lens at point"))))

(defun test-poly ()
  (declare (et (@generics [T]) (@return Number)))
  (:eval (et--repr-to-type (et-repr extends? T Number 1 2) nil)))


(et-defvar lens-in-buffer-functions (List (fn Nil List<Lens>)) nil
  "A list of functions for determining the lenses in the buffer.

Each function should return a list of lenses of a particular host type
in the current buffer. This is used when an action on the buffer would
affect all lenses, such as killing or saving the buffer.")

(defun lenses-in-buffer ()
  "Return a list of all lenses in the current buffer."
  (declare (et (@return List<Lens>)))
  (cl-loop for fn in lens-in-buffer-functions
           nconc (funcall fn)))


;;;; Buffer-region host

(defun lens-region-search-forward (&optional lens)
  (declare (et (@generics [(<= L Nil|Lens)])
               (lens L)
               (@return (or LensRegion<Lens> (if? L Never Nil)))))

  (or (et! LensRegion<Lens>
        (lens--region-search-forward 'lens-begin 'lens-end lens (when lens #'eq)))
      (when lens (error "Lens not found: %s" (plist-get (et! Lens lens) :id)))))

(defun lens-at-point:region ()
  (declare (et (@return LensRegion<Lens>)))
  (et! LensRegion<Lens>
    (lens--region-at-point 'lens-begin 'lens-end)))

(cl-defun lens-host:region (lens &key start end replaced-cell style)
  "A host representing a particular region of the current buffer.

Sometimes, we want the source to be based on the region replaced by the
lens. In this case, `replaced-cell' will be passed as a cons cell.
Creating the region will set the car of the cons cell to the replaced
text. Then, every time the source gets updated, it will set the car of
the cell to the updated text."
  (declare (et (lens Lens) (start Nil|Integer) (end Nil|Integer)
               (replaced-cell (ConsW String Never))
               (style Any)
               (@return LensHost)))

  (add-to-list 'lens-at-point-functions #'lens-at-point:region t)

  (let* ((mark (or (when mark-active (mark)) (point))))
    (unless start (setq start (min (point) mark)))
    (unless end (setq end (max (point) mark))))

  (unless replaced-cell (setq replaced-cell (list nil)))

  (let ((buffer (current-buffer))
        (headers (lens--generate-headers lens style)))

    (setcar replaced-cell (buffer-substring-no-properties start end))

    (lens-force-edit
     (goto-char start)
     (delete-region start end)
     (insert (car headers) (cdr headers)))

    (list
     :host-edit
     (et! LensHostEdit
       (lambda (fn)
         (cl-assert (not (buffer-narrowed-p)))
         (with-current-buffer buffer
           (lens-save-position
            (without-restriction
              (goto-char (point-min))
              (cl-destructuring-bind (_ _hb he fb _fe) (lens-region-search-forward lens)
                (with-restriction he fb
                  (funcall fn))))))))
     :host-close
     (et! LensHostClose
       (lambda ()
         (cl-assert (not (buffer-narrowed-p)))
         (with-current-buffer buffer
           (without-restriction
             (goto-char (point-min))
             (cl-destructuring-bind (_ hb _he _fb fe) (lens-region-search-forward lens)
               (lens-force-edit
                (delete-region hb fe)
                (insert (car replaced-cell)))))))))))


;;;; Headers

(defun lens--generate-headers (lens &optional style)
  "Generate the header and footer strings for a lens.

Returns (HEAD-STRING . FOOT-STRING).

The footer contains a trailing newline, but the header does not."

  (cl-destructuring-bind (&key ((:head-props hps)) ((:foot-props fps)) ((:head-face hf)) ((:foot-face ff)) &allow-other-keys) style
    (let* ((h (apply #'propertize (format "<begin %s>\n" (plist-get lens :id)) 'lens-begin lens 'read-only t hps))
           (f (apply #'propertize (format "<end %s>\n" (plist-get lens :id)) 'lens-end lens 'read-only t fps)))
      ;; Add the head-face and foot-face specified in the style
      (lens--append-face 0 (length h) hf h)
      (lens--append-face 0 (length f) ff f)
      ;; Make the header and footer non-sticky.
      (cons (lens--make-sticky h) (lens--make-sticky f)))))


;;;; Full-buffer region

(defvar-local lens-in-buffer nil
  "The lens taking up the current buffer.")

(defun lens-at-point:buffer ()
  lens-in-buffer)

(cl-defun lens-region:buffer (lens &key name)
  "A region composed of an entire buffer."
  (add-to-list 'lens-at-point-functions 'lens-at-point:buffer)

  (let ((buffer (generate-new-buffer (or name "lens"))))
    (with-current-buffer buffer
      (setq-local lens-in-buffer lens))

    (list
     :edit
     (lambda (fn &rest args)
       (with-current-buffer (or buffer (error "Buffer does not exist"))
         (lens-force-edit (apply fn args))))
     :close
     (lambda ()
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))


;;; ============================================================
;;; Models
;;;; Buffer model

(defvar-local lens--buffer-referencers nil
  "List of event emitters for lenses that reference the current buffer.")

(defvar lens--ignore-buffer-referencer nil
  "Ignore this referencer when refreshing.

This is used when the model is updating the text of the buffer. The emit
function associated with that model is to be ignored to prevent an
infinite loop.")

(defun lens--refresh-buffer-referencers (&rest _after-change-args)
  "Refresh all lenses which reference the current buffer.

This function is designed to be set up as an `after-change-hook',
hance _AFTER-CHANGE-ARGS, although the args are ignored."
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (setq lens--buffer-referencers
          (seq-filter
           (lambda (emit)
             (condition-case _err
                 (unless (eq emit lens--ignore-buffer-referencer)
                   (funcall emit (list :new-text str)))
               (:success t)
               (error (message "Removing buffer watcher") nil)))
           lens--buffer-referencers))
    (setq lens--ignore-buffer-referencer nil)))

(cl-defun lens-model:buffer (emit &key buffer)
  (when (stringp buffer)
    (setq buffer (or (get-buffer buffer) (error "Buffer does not exist: %s" buffer))))
  (unless (buffer-live-p buffer) (error "Invalid buffer: %s" buffer))

  (with-current-buffer buffer
    (push emit lens--buffer-referencers)
    (add-hook 'after-change-functions #'lens--refresh-buffer-referencers nil 'local))

  (list
   :model-on-close
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq lens--buffer-referencers
               (remq emit lens--buffer-referencers)))))
   :model-on-save
   (lambda ()
     (with-current-buffer (or buffer (error "Referenced buffer no longer exists"))
       (when buffer-file-name (save-buffer))))
   :get-text
   (lambda ()
     (with-current-buffer buffer
       (buffer-substring-no-properties (point-min) (point-max))))
   :set-text
   (lambda (new-content)
     (with-current-buffer (or buffer (error "Referenced buffer no longer exists"))
       (add-to-list 'lens--buffer-referencers emit)
       (add-hook 'after-change-functions #'lens--refresh-buffer-referencers nil 'local)

       (let ((lens--ignore-buffer-referencer emit))
         (replace-region-contents (point-min) (point-max) (lambda () new-content)))))))


;;; ============================================================
;;; Edit commands
;;;; Insert

(defvar lens-edit--prefixes nil
  "List of prefixes to apply to inserted text, in reverse order.")

(defvar lens-edit--faces nil
  "List of prefixes to apply to inserted text, in order.")

(defun lens-edit--apply-props (orig-buf)
  "Apply face and prefix properties.

The current buffer is the temporary buffer containing the text.
CONTENT-BUF is the buffer containing the string being modified."

  ;; Apply prefix properties
  (let ((prefix (apply #'concat (reverse lens-edit--prefixes)))
        match beg end face)
    (put-text-property (point-min) (point-max) 'line-prefix prefix)
    (put-text-property (point-min) (point-max) 'wrap-prefix prefix)

    ;; Apply face properties
    (goto-char (point-min))
    (while (setq match (text-property-search-forward 'face))
      (setq face (prop-match-value match))
      (when (or (not (listp face)) (keywordp (car face)))
        (setq face (list face)))

      (setq beg (- (prop-match-beginning match) (point-min)))
      (setq end (- (prop-match-end match) (point-min)))

      (with-current-buffer orig-buf
        (setq beg (+ beg (point-min)) end (+ end (point-min)))
        (put-text-property beg end 'face (append face lens-edit--faces))
        (put-text-property beg end 'font-lock-face (append face lens-edit--faces))))))

(defun lens-edit-insert (&rest args)
  (let ((start (point)))
    (apply #'insert args)

    (with-restriction start (point)
      (lens-edit--apply-props (current-buffer)))))

(defun lens-edit-replace (string)
  (let ((orig-buf (current-buffer))
        tmp-buf)
    (with-temp-buffer
      ;; Replace the contents
      (setq tmp-buf (current-buffer))
      (insert string)
      (with-current-buffer orig-buf (replace-buffer-contents tmp-buf))

      (lens-edit--apply-props orig-buf))))


;;;; Text property

(defun lens--push-property (beg end prop value)
  (if (eq prop 'face)
      (add-face-text-property beg end value)
    (alter-text-property beg end prop
                         (lambda (old)
                           (if (and (consp old) (eq (car old) value))
                               old (cons value old))))))

(defun lens--pop-property (beg end prop value)
  (if (eq prop 'face)
      (alter-text-property
       beg end prop
       (lambda (old)
         (cond ((and (listp old) (not (keywordp (car old)))) (remq value old))
               ((eq old value) nil)
               (t old))))
    (alter-text-property
     beg end prop
     (lambda (old)
       (if (and (consp old) (eq (car old) value))
           (cdr old) old)))))


;;;; Line prefix

(defun lens-remove-text-with-property (string prop)
  "Return STRING with all substrings having text property PROP removed."
  (let ((result "")
        (pos 0)
        (len (length string)))
    (while (< pos len)
      (let ((next (next-single-property-change pos prop string len)))
        (unless (get-text-property pos prop string)
          (setq result (concat result (substring string pos next))))
        (setq pos next)))
    result))

(defun lens--add-line-prefix (beg end id prefix)
  (alter-text-property
   beg end
   'line-prefix
   (lambda (str)
     (let ((pre (concat (propertize prefix id t)
                        (lens-remove-text-with-property str id))))
       (unless (s-blank-p pre) pre)))))

(defun lens--remove-scrollbar (id)
  (lens--add-line-prefix (point-min) (point-max) id ""))


;;; ============================================================
;;; Views
;;;; Raw text view

(defun lens-view:text (emit model)
  (cl-assert (functionp (plist-get model :get-text)) nil "Model must have a get-text function")
  (cl-assert (functionp (plist-get model :set-text)) nil "Model must have a set-text function")

  (let* ((text (funcall (plist-get model :get-text)))
         (emit-last (lambda (events) (funcall emit (car (last events)))))
         (onchange (lambda (&rest _) (lens-batch-post-command emit-last '(:changed t))))
         (propertize
          (lambda ()
            (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
              (put-text-property (point-min) (point-max) prop (list onchange)))))
         (insert
          (lambda ()
            (lens-force-edit
             (delete-region (point-min) (point-max))
             (insert text)
             (funcall propertize)))))

    (list
     :view-insert insert
     :view-update
     (lambda (source event)
       (pcase (list source event)
         (`(model (:new-text ,new))
          (setq text new)
          (lens-save-position (funcall insert)))
         (`(view (:changed t))
          (setq text (buffer-substring-no-properties (point-min) (point-max)))
          (lens-force-edit (funcall propertize))
          (funcall (plist-get model :set-text) text)))))))


;;;; Column view

(defface lens-column-separator '((t (:background "darkred" :extend nil)))
  "Face to use for column separators.")

(defcustom lens-column-separator "\n"
  "String to use as a separator between rows in a column view."
  :type 'string
  :group 'lens)

(defun lens-view:column (emit model subview-fns)
  "A view containing multiple child views laid out vertically.

SUBVIEW-FNS is a list of cons cells (KEY . VIEW-FN). KEY is a keyword
representing a unique id for the view. The format of VIEW-FN matches
that of `lens-create'.

Supported event types:
  (:set-children SUBVIEW-FNS) - Update the child views.
  (:nested CHILD-KEY EVENT) - Send a nested event to a child."

  (let* ((col-id (intern (format "column-%s" (abs (random)))))

         (create-subview
          (pcase-lambda (`(,key . ,view-fn))
            (let ((nested-emit (lambda (event) (funcall emit (list :nested key event)))))
              (cons key (lens--init view-fn nested-emit model)))))

         (make-sep
          (lambda (key)
            (let ((props (get-text-property (1- (point)) 'lens-column-separator)))
              (lens-force-edit
               (when props (delete-char -1))
               (insert
                (apply #'propertize lens-column-separator
                       'face 'lens-column-separator
                       'font-lock-face 'lens-column-separator
                       'read-only t
                       'rear-nonsticky t
                       col-id key
                       props))))))

         (subviews (mapcar create-subview subview-fns))) ;; A list of (KEY . VIEW)

    (list
     :column-id col-id
     :view-insert
     (lambda ()
       (pcase-dolist (`(,key . ,subview) subviews)
         (with-restriction (point-max) (point-max)
           (funcall (plist-get subview :view-insert))
           (goto-char (point-max))
           (funcall make-sep key))))

     :view-update
     (lambda (source event)
       (goto-char (point-min))
       (pcase (list source event)
         ;; Handle a model event
         (`(model ,_)
          (let ((last-end nil)
                (prop-match))
            (goto-char (point-min))
            ;; Loop through the child regions
            (pcase-dolist (`(,key . ,subview) subviews)
              (setq last-end (point))
              (setq prop-match (or (text-property-search-forward col-id)
                                   (error "Column malformed: No ending indicator")))
              (or (eq key (prop-match-value prop-match))
                  (error "Column malformed: Mismatched ending indicator"))
              ;; Recursively call the event
              (with-restriction last-end (prop-match-beginning prop-match)
                (funcall (plist-get subview :update) 'model event)))))

         ;; Handle a set children event
         (`(view (:set-children ,new-subview-fns))
          (let ((new-rev nil)
                (old-tail subviews)
                (last-point nil)
                (prop-match nil)
                (existing-subview nil))
            (goto-char (point-min))
            (pcase-dolist (`(,key . ,svfn) new-subview-fns)
              ;; Check if the child exists in the remaining list
              ;; (easier than searching for the region manually and resetting the point if it can't be found)
              (if (not (alist-get key old-tail))
                  ;; Insert the new view and also add it to the alist of views
                  (progn
                    ;; Maybe this subview existed, but was skipped over, so just reinsert it
                    (setq existing-subview (alist-get key subviews))
                    (push (if existing-subview
                              (progn (lens-log debug "Moving existing subview" key)
                                     (cons key existing-subview))
                            (lens-log debug "Creating new subview" key)
                            (funcall create-subview (cons key svfn)))
                          new-rev)
                    (with-restriction (point) (point)
                      (funcall (plist-get (cdar new-rev) :view-insert))
                      (goto-char (point-max))
                      (funcall make-sep key)))
                ;; The child exists, delete regions until the correct region is found
                (while (progn (setq last-point (point))
                              (setq prop-match
                                    (or (text-property-search-forward col-id)
                                        (error "Column malformed: Missing old key region: %s" key)))
                              (not (eq key (prop-match-value prop-match))))
                  (lens-log debug "Deleting subview" [key (prop-match-value prop-match)])
                  (lens-force-edit (delete-region last-point (point))))
                ;; Remove from the old tail until the correct child is reached
                (while (not (eq key (caar old-tail))) (pop old-tail))
                (push (pop old-tail) new-rev)))
            ;; Set the new subviews
            (setq subviews (nreverse new-rev))))

         ;; Handle an update child event
         (`(view (:nested ,child-key ,event))
          (let ((subview (or (alist-get child-key subviews)
                             (error "Column subview does not exist: %s" child-key)))
                (prop-match nil))
            (goto-char (point-min))
            (or (setq prop-match (text-property-search-forward col-id child-key #'eq))
                (error "Error in column nested event: Column subview region not found: %s" child-key))
            (goto-char (prop-match-beginning prop-match))
            ;; Assert to double check the column is not malformed
            (or (and (looking-at-p "\n") (eq (get-text-property (point) col-id) child-key))
                (error "Error in column nested event: Not looking at column separator before event"))
            (with-restriction (point)
                (or (and (setq prop-match (text-property-search-backward col-id))
                         (prop-match-end prop-match))
                    (point-min))
              (funcall (plist-get subview :view-update) 'view event)
              (goto-char (point-max))
              (funcall make-sep child-key))
            ;; Assert to double check the column did not get malformed performing the event
            (or (and (looking-at-p "\n") (eq (get-text-property (point) col-id) child-key))
                (error "Error in column nested event: Not looking at column separator after event"))
            (lens-force-edit (delete-char 1))))

         (_ (lens-log warn "Column received unexpected event" source event)))))))


;;; ============================================================
;;; Declarative Components
;;;; Subcomponents

(defun lens-declare-subcomponent (view emit model initializer wrap-fn &optional insert?)
  (lens-declare-subcomponents
   view emit model `((:only ,initializer ,wrap-fn))
   (lambda (old new)
     ;; Insert should only be triggered from a true insert, never be
     ;; triggered from change of subcomponents
     (cl-assert (and (null old) (eq (point-min) (point-max))))
     (pcase new
       (`((:only ,_new-view ,insert-fn)) (funcall insert-fn))
       (_ (error "Malformed view alist for single subcomponent"))))
   insert?))

(defun lens-declare-subcomponents (view emit model subcomponents insert &optional insert?)
  "Declare managed subcomponents for this component.

Keep track of the subcomponents over time, sending the appropriate
:new-args events when their arguments change, and performs reinserts
when the subcomponent order changes.

VIEW is the parent view which the subviews are being created in. MODEL
is the model that the children should use. EMIT is the emit function for
VIEW.

SUBCOMPONENTS is a list of (KEY (FUNC ARGS...) WRAP-UPDATE?).

KEY is a unique key to identify the subcomponent. If SUBCOMPONENTS
changes, then the views will be rearranged based on the keys.

\(FUNC ARGS...) is a stateful component spec, where FUNC is a stateful
component function, and ARGS are the extra arguments passed to it after
the view plist.

WRAP-UPDATE is a function (UPDATE-FN) -> nil to notify the particular
subcomponent of an event. The WRAP-UPDATE function should narrow the
buffer to the region in which this particular component lives, and then
call UPDATE-FN.

INSERT is a function (OLD-SUBVIEW-ALIST SUBVIEW-ALIST) -> nil which
should insert the subviews, or handle inserting new or deleting old
subviews if the order of subviews ever changes.

[OLD-]SUBVIEW-ALIST is an ordered list of (KEY SUBVIEW INSERT-FN),
corresponding to the views created from each component in SUBCOMPONENTS.
INSERT-FN simply wraps the `:view-insert' method of the subview with the
corresponding WRAP-UPDATE function, if there is one.

If INSERT? is non-nil, then assume that the buffer is empty and perform
a full insert of all subcomponents.

This function will use the `:subcomponents' and `:update-subcomponent'
field in the view. You should not have multiple calls in the same view
for different subcomponents, as they will overwrite each other.

The `:update-subcomponent' field in the view will be assigned to a
function (KEY EVENT) -> nil, which should be called every time the
parent recieves a (:sub KEY EVENT) event."

  ;; The state is (SUBCOMPONENTS . ALL-SUBVIEWS). All subviews is a list
  ;; of (KEY VIEW (FUNC ARGS...)) containing all subviews created over
  ;; the lifetime of this hook, even if they are not currently
  ;; visible. This is different from SUBVIEW-ALIST, which are only
  ;; current subviews.
  (let* ((state (or (plist-get view :subcomponents)
                    (let ((state (cons subcomponents nil)))
                      (prog1 state (plist-put view :subcomponents state)))))
         (old-subcomponents (car state))
         (all-subviews (cdr state))

         (update-subview
          (lambda (subview wrap-fn event &optional insert)
            (let ((update-fn (plist-get subview (if insert :view-insert :view-update)))
                  (args (if insert nil (list 'view event))))
              (if (null wrap-fn) (apply update-fn args)
                (funcall wrap-fn (lambda () (apply update-fn args)))))))

         ;; Find the appropriate subview and wrap function, and pass it the event
         (update-sub-by-key
          (lambda (key event)
            (let* ((comp (or (alist-get key subcomponents)
                             (error "Subcomponent %s not defined" key)))
                   (wrap-fn (caddr comp))
                   (subview (or (car (alist-get key all-subviews))
                                (error "Subview %s not defined" key))))
              (funcall update-subview subview wrap-fn event))))

         (subcomponents-to-alist
          (lambda (cs)
            (cl-loop for (key _ wrap-fn) in cs
                     collect
                     (let ((subview (or (car (alist-get key all-subviews))
                                        (error "Subview %s has not been created" key))))
                       (list key subview (lambda () (funcall update-subview subview wrap-fn nil t)))))))

         (create-new-subviews
          (lambda ()
            (pcase-dolist (`(,key ,initializer) subcomponents)
              (unless (alist-get key all-subviews)
                ;; In theory, the parent should be allowed to specify
                (let ((emit (lambda (event) (funcall emit (list :sub key event)))))
                  (push (list key (lens--init initializer emit model) initializer)
                        all-subviews)
                  (setcdr state all-subviews)))))))

    (setcar state subcomponents)

    (if insert?
        (progn
          (cl-assert (eq (point-min) (point-max)))
          (funcall create-new-subviews)
          ;; Always set previous com
          (funcall insert nil (funcall subcomponents-to-alist subcomponents)))

      ;; Check if the subcomponents have changed
      (unless (equal subcomponents old-subcomponents)
        ;; Send :new-args to all current subcomponents which exist, but
        ;; whose args do not match the previous value
        (pcase-dolist (`(,key (,func . ,args) ,wrap-fn) subcomponents)
          (pcase-let* ((`(,subview (,old-func . ,old-args)) (alist-get key all-subviews)))
            (when subview
              ;; Ensure that the function did not change
              (when (not (eq func old-func))
                (error "Subcomponent %s function changed %s -> %s" subview func old-func))
              ;; If the args changed, send a :new-args event
              (unless (equal args old-args)
                (lens-log debug "ahh" func args)
                (funcall update-subview subview wrap-fn (cons :new-args args))))))

        ;; Create new subviews that don't exist
        (funcall create-new-subviews)

        ;; Check if the order changed
        (unless (and (eq (length subcomponents) (length old-subcomponents))
                     (cl-loop for (key) in subcomponents
                              for (old-key) in old-subcomponents
                              always (eq key old-key)))
          ;; The order has changed, so call INSERT
          (funcall insert
                   (funcall subcomponents-to-alist old-subcomponents)
                   (funcall subcomponents-to-alist subcomponents)))))

    (plist-put view :update-subcomponent update-sub-by-key)))


;;; ============================================================
;;; Stateful
;;;; Hooks
;;;;; Hook helper

(defun lens--use (view type &rest args)
  (if (plist-get view :initialized)
      ;; On subsequent updates
      (let ((existing (nth (plist-get view :hook-idx) (plist-get view :hooks))))
        (unless (eq type (car existing))
          (error "Expected hook %s but found %s" (car existing) type))
        (plist-put view :hook-idx (1+ (plist-get view :hook-idx)))
        existing)
    ;; On initial insert, set up the hook for the first time
    (let ((new (cons type args)))
      (cl-assert (plist-get view :hook-idx) (length (plist-get view :hooks)))
      (plist-put view :hooks (nconc (plist-get view :hooks) (list new)))
      (plist-put view :hook-idx (1+ (plist-get view :hook-idx)))
      new)))


;;;;; Use handler

(defun lens-use-handler (view name func &optional no-rerender)
  "NAME serves no purpose other than debugging the event."
  (let* ((idx (plist-get view :hook-idx))
         (hook (lens--use view :use-handler idx func no-rerender)))
    (setcar (cddr hook) func)

    (lambda (&rest args)
      (interactive)
      (funcall (plist-get view :emit) `(:hook ,idx ,name ,@args)))))


;;;;; Use state

(defmacro lens-use-state (view initial)
  (declare (indent 1))
  (if (null initial)
      `(lens--use ,view :use-state nil)
    `(let ((view ,view))
       (lens--use view :use-state
                  (unless (plist-get view :initialized) ,initial)))))

(defun lens-set-state (hook value)
  (cl-assert (eq (car hook) :use-state))
  (setcar (cdr hook) value))

(defun lens-get-state (hook)
  (cl-assert (eq (car hook) :use-state))
  (cadr hook))


;;;;; Focus

(defvar-local lens-focused nil
  "The currently focused element in the current buffer.

This is nil, or a plist representing the current focused element.

The plist has the following internally managed properties:
  :view - The current view focused, compared by eq
  :last-point - The position of the cursor after the previous command.

It also can have any of the following properties specified by the
use-focusable hook:
  :update - A function called every time the cursor moves while the
    element is focused. If it returns nil, then unfocus the element. If
    this property is omited, then the element will be immediately
    unfocused every time it is focused.
  :on-focus - Function called when focusing.
  :on-unfocus - Function called when unfocusing.
  :info - A plist allowing additional arguments to be passed to hooks.

All functions are called with one argument. This argument is the result
of (append INFO PLIST), where INFO is the value of the info property
above, and PLIST is the plist of properties listed above.")

(defun lens--focused-post-command ()
  "Handle updates for the currently focused element."
  (if (null lens-focused) (lens-unfocus)

    (when-let* ((fn (plist-get lens-focused :update)))
      (condition-case err
          (funcall fn (append (plist-get lens-focused :info) lens-focused))
        (error (lens-log error "Error fixing focus: %s" err)
               (lens-unfocus))
        (:success (when lens-focused
                    (plist-put lens-focused :last-point (point))))))))

(defun lens--perform-focus-change (focus)
  "Helper function to perform focus changes."
  (unless (eq (plist-get focus :view) (plist-get lens-focused :view))

    (let ((unfocus lens-focused))
      (unwind-protect
          (when-let ((on-unfocus (plist-get unfocus :on-unfocus)))
            (with-demoted-errors "Error in unfocus: %s"
              (funcall on-unfocus (append unfocus (plist-get unfocus :info)))))
        (setq lens-focused nil)))

    (let ((lens-focused focus))
      (when-let ((on-focus (plist-get focus :on-focus)))
        (with-demoted-errors "Error in focus: %s"
          (funcall on-focus (append focus (plist-get focus :info))))))

    (if (null (plist-get focus :update))
        (remove-hook 'post-command-hook #'lens--focused-post-command t)
      (setq lens-focused focus)
      (add-hook 'post-command-hook #'lens--focused-post-command nil t))))

(defun lens-unfocus (&optional noupdate)
  "Unfocus the focused element."
  (interactive)
  (when noupdate (setq lens-focused nil))
  (lens--perform-focus-change nil))

(defun lens--focus (focus-plist)
  "Change focus to the plist FOCUS-PLIST."
  (lens--perform-focus-change
   (apply #'list :last-point (point)
          focus-plist)))


;;;;; Use subcomponent

(defun lens-use-subcomponents (view insert subcomponents)
  (lens-declare-subcomponents
   view (plist-get view :emit) (plist-get view :model)
   subcomponents insert
   (eq (plist-get view :reason) 'insert)))

(defun lens-use-subcomponent (view initializer &optional wrap-fn)
  "Declare a single subcomponent in a stateful view."

  (lens-declare-subcomponent
   view (plist-get view :emit) (plist-get view :model)
   initializer wrap-fn
   (eq (plist-get view :reason) 'insert)))


;;;; Stateful view
;;;;; Calling the function

(defun lens--stateful-view-call-func (view reason)
  "Call the stateful component function.

REASON is what triggered the rerender. This is either `insert',
`new-args', or `handler'."
  (let ((initialized (plist-get view :initialized)))
    ;; First call to stateful function must be an insert
    (cl-assert (or initialized (eq reason 'insert)))
    ;; When inserting, the buffer region should be empty
    (cl-assert (or (not (eq reason 'insert)) (eq (point-min) (point-max))))

    (plist-put view :hook-idx 0)
    (plist-put view :reason reason)
    (apply (plist-get view :func) view (plist-get view :func-args))

    (unless initialized (plist-put view :initialized t))))


;;;;; Update function

(defun lens--stateful-view-update (view source event)
  (pcase (list source event)
    ;; The view changed arguments
    (`(view (:new-args ,func . ,args))
     (cl-assert (eq func (plist-get view :func)))
     (plist-put view :func-args args)
     (lens--stateful-view-call-func view 'new-args))

    (`(view (:sub ,key ,event))
     (funcall (plist-get view :update-subcomponent) key event))

    ;; The event is targetting a particular handler
    (`(view (:hook ,key ,_name . ,args))
     (let (hook-found rerender)
       ;; Search for a handler to call
       (dolist (hook (plist-get view :hooks))
         (when (and (eq (car hook) :use-handler)
                    (eq (cadr hook) key))
           (apply (caddr hook) args)
           (setq hook-found t)
           ;; If the hook has no-rerender enabled, don't set modified to true
           (unless (nth 3 hook) (setq rerender t))))

       (unless hook-found (lens-log warn "Handler event not processed" key))
       ;; Rerun the component function
       (when rerender (lens--stateful-view-call-func view 'handler))))

    (_ (lens-log error "Stateful unsupported event" source event))))


;;;;; Stateful view

(defun lens-view:stateful (emit model func &rest extra-func-args)
  "Create a stateful view based on FUNC.

FUNC is a function which is rerun each time the stateful view recieves
an event that it has a handler set up for. It should return a string to
be inserted, a view to use as the child view, or a list of tuples that
look like (KEY FUNC ARGS...). In the case that it is a list of tuples,
the child view will be a column view, with each tuple representing a
stateful view which will be a child of the column view."
  (let (view)
    (setq view
          (list :view-insert (lambda () (lens--stateful-view-call-func view 'insert))
                :view-update (lambda (s e) (lens--stateful-view-update view s e))
                :view-close (lambda ())
                :hooks nil
                :hook-idx 0
                :func func
                :func-args extra-func-args
                :initialized nil
                :emit emit
                :model model))))


;;;; Stateful macro

(defmacro lens-stateful-defun (name arglist &rest body)
  (declare (doc-string 3) (indent 2))
  `(cl-defun ,name ,(cons '=view= arglist)
     ,@(when (stringp (car body)) (list (pop body)))
     ,(macroexpand-1 `(lens-stateful-body ,@body))))

(defmacro lens-stateful-body (&rest body)
  "Expects the variable `=view=' to be bound in the local scope."
  (macroexpand-1
   `(lens-let-body
     [(:use-handler
       lambda (body name &rest cb)
       (let ((no-rerender (when (eq (car cb) :no-rerender) (pop cb) t)))
         (when (and (> (length cb) 1) (not (and (listp (car cb)) (seq-every-p #'symbolp (car cb)))))
           (error "If :use-handler has more than 2 arguments, the second must be an argument list"))
         (setq cb (if (eq (length cb) 1) (car cb) (cons #'lambda cb)))
         `((let ((,name (lens-use-handler =view= ',name ,cb ,no-rerender)))
             ,@body))))
      (:use-state
       lambda (body var initial)
       (pcase var
         (`[,(and (pred symbolp) state-var) ,(and (pred symbolp) set-var)]
          `((let* ((=use-state= (lens-use-state =view= ,initial))
                   (,state-var (lens-get-state =use-state=))
                   (,set-var (lambda (v) (lens-set-state =use-state= v))))
              (cl-flet ((,set-var ,set-var))
                ,@body))))
         ((pred symbolp)
          `((let* ((=use-state= (lens-use-state =view= ,initial))
                   (,var (lens-get-state =use-state=)))
              (cl-flet ((,var (lambda (arg) (lens-set-state =use-state= arg))))
                ,@body))))
         (_ (error ":use-state var must be a symbol, or vector of two symbols"))))
      (:column
       lambda (body &rest children)
       (unless (null body) (error ":column must appear at the end of a component"))
       ;; Each child is either a form (:key STATEFUL-FUNC ARGS...) or
       ;; an expression which returns a list of these.
       (let ((exprs
              (cl-loop for expr in children
                       if (and (listp expr) (or (keywordp (car expr)) (and (listp (car expr)) (keywordp (caar expr)))))
                       collect `(list (list ,(if (keywordp (car expr)) (car expr) (cons #'list (car expr)))
                                            ,@(cdr expr)))
                       else
                       collect expr)))
         (list (list #'lens-stateful:column '=view= (cons #'append exprs)))))]
     ,@body)))


;;;; Root stateful components
;;;;; Column stateful view type

(lens-stateful-defun lens-stateful:column (rows)
                     "Update function for a column-based stateful view.

VIEW is the stateful view object. ROWS is a list of (KEY STATEFUL-FUNC
ARGS) indicating the new rows for the column.

A column-based stateful view, it means that ROWS is a list of stateful
views, which will be displayed in a column. The rows may change, and
this function will handle inserting the new rows and deleting the old
ones, while leaving the unchanged rows untouched. The exception to this
is if existing rows are reordered, in which case one will have to be
deleted and re-inserted."
                     (:let is-insert (eq 'insert (plist-get =view= :reason)))

                     ;; Has the form (ROWS COL-VIEW)
                     ;; ROWS is a list of (KEY FUNC . ARGS)
                     ;; COL-VIEW is the view used for the column component
                     (:use-state $state nil)
                     (:let old-rows (car $state))
                     (:let col-view (cadr $state))

                     (:use-handler
                      column-emit :no-rerender (event)
                      (funcall (plist-get col-view :view-update) 'view event))

                     (when (xor (plist-get =view= :initialized) col-view)
                       (error "Column view should be initialized iff parent view is initialized"))

                     (cond
                      ((null col-view)
                       (cl-assert is-insert)
                       (cl-assert (eq (point-min) (point-max)))

                       ;; We need to initialize a new column view
                       (let ((new-cs (cl-loop for (key func . args) in rows collect `(,key lens-view:stateful ,func ,@args))))
                         (setq col-view (lens-view:column column-emit (plist-get =view= :model) new-cs))
                         ($state (list rows col-view))
                         (funcall (plist-get col-view :view-insert))))

                      ;; Check if the rows have changed at all, because otherwise we don't need to do anything
                      ((equal old-rows rows)
                       (when is-insert
                         (lens-log debug "Reinserting stateful column")
                         (funcall (plist-get col-view :view-insert))))

                      ;; Update the existing column view
                      ((let ((col-update (plist-get col-view :view-update)))
                         ;; Insert the old column view so that we can update it it
                         (when is-insert
                           (lens-log debug "Reinserting stateful column to update it")
                           (funcall (plist-get col-view :view-insert)))

                         ;; If the order of the keys has changed, we need to send a set-children event
                         (when (or (not (eq (length rows) (length old-rows)))
                                   (cl-loop for (old-key) in old-rows
                                            for (new-key) in rows
                                            thereis (not (eq old-key new-key))))
                           (let ((cs (cl-loop for (key func . args) in rows collect `(,key lens-view:stateful ,func ,@args))))
                             (funcall col-update 'view (list :set-children cs))))
                         ;; Now, we need to send :update-args events to old views whose args changed
                         (pcase-dolist (`(,key ,func . ,args) rows)
                           (pcase-let ((`(,old-func . ,old-args) (alist-get key old-rows)))
                             (cond
                              ;; If there was no corresponding old child, no need to send the event
                              ((null old-func))
                              ;; In the case that a child function changed, don't crash, but don't try to update it
                              ((not (eq func old-func)) (lens-log error "Stateful child function changed" key func old-func))
                              ;; Send a new-args event to the child
                              ((unless (equal args old-args)
                                 (funcall col-update 'view `(:nested ,key (:new-args ,func ,@args))))))))))))


;;;;; String stateful type

(lens-stateful-defun lens-stateful:string (&rest contents)
                     (:use-state [old-contents-rest set-old-contents] nil)
                     (set-old-contents contents)

                     (:let idx nil)
                     (:flet content-to-str (c) (or (if (listp c) (car c) c) ""))
                     (:flet delete-content (c)
                            (let ((str (or (if (listp c) (car c) c) "")))
                              (cl-assert (looking-at-p (regexp-quote str)))
                              (delete-region (point) (+ (point) (length str)))))

                     (goto-char (point-min))
                     (dolist (content contents)
                       ;; Check if the new content is in the list
                       (if (not (setq idx (cl-position content old-contents-rest :test #'equal)))
                           ;; Insert the new content
                           (insert (if (and content (listp content))
                                       (apply #'propertize content)
                                     content))
                         ;; Delete up to the existing version of the content
                         (dotimes (_ idx) (delete-content (pop old-contents-rest)))
                         ;; Ensure the content is there
                         (let ((str (content-to-str content)))
                           (cl-assert (looking-at-p (regexp-quote str)))
                           (forward-char (length str)))))
                     ;; Remove the remaining old content
                     (while old-contents-rest
                       (delete-content (pop old-contents-rest)))
                     (cl-assert (eobp)))


;;;; Examples
;;;;; Example counter

(lens-stateful-defun lens-stateful:counter (&key id onchange)
                     (:use-state $count 1)
                     (:use-handler
                      inc (&optional n)
                      ($count (+ $count (or n 1)))
                      (when onchange (funcall onchange (+ $count (or n 1)))))

                     (when id (lens-log debug "Counter rerendered" id))

                     (:column
                      (:label #'lens-stateful:string (format "Count=%s" $count))
                      (:button #'lens-stateful:button "Click" inc)))

(lens-stateful-defun lens-stateful:counter-column ()
                     (:use-state $count 1)
                     (:use-handler set-count (c) ($count c))

                     (lens-log debug "List rerendered" $count)

                     (:use-state $content "hi there")
                     (:use-handler set-content (c) ($content (upcase c)))

                     (lens-stateful:scroll
                      =view=
                      `(lens-view:stateful
                        lens-stateful:column
                        (,(list :main #'lens-stateful:counter :id "main" :onchange set-count)
                         ,(list :field #'lens-stateful:field $content set-content)
                         ,@(cl-loop for i from 1 to $count
                                    collect `(,(intern (format ":counter-%s" i)) lens-stateful:counter))))
                      :height 10))


;;; ============================================================
;;; Components
;;;; Button

(defun lens-click ()
  "Click on the button at point."
  (interactive)
  (funcall (or (get-text-property (point) 'lens:onclick)
               (error "No button at point"))))

(lens-stateful-defun lens-stateful:button (content onclick &rest props)
                     (lens-stateful:string
                      =view=
                      `(,content lens:onclick ,onclick
                                 keymap (keymap (return . lens-click))
                                 ,@props)))


;;;; Field

(lens-stateful-defun lens-stateful:field (content onchange &rest props)
                     (:use-handler hook-callback (&rest _)
                                   (funcall onchange (buffer-substring-no-properties (point-min) (point-max))))
                     (:let cb (lambda (&rest _) (run-at-time 0 nil hook-callback)))

                     (:use-state $old-content nil)

                     (unless (equal content $old-content)
                       ($old-content content)

                       (lens-force-edit
                        (replace-region-contents
                         (point-min) (point-max)
                         (lambda () content))

                        (set-text-properties
                         (point-min) (point-max)
                         `(
                           modification-hooks (,cb)
                           insert-in-front-hooks (,cb)
                           insert-behind-hooks (,cb)
                           ,@props))

                        (goto-char (point-max))
                        (insert (propertize
                                 "\n"
                                 'lens-column-separator
                                 `(insert-in-front-hooks (,cb)))))))


;;;; Scroll
;;;;; Component

(lens-stateful-defun lens-stateful:scroll
                     (subview-args &key height scrollbar? label? visual-lines?)
                     (:use-state $id (random))

                     (:use-handler
                      update :no-rerender (plist)
                      (if (or (< lens-point (point-min)) (> lens-point (point-max)))
                          (lens-unfocus)
                        (lens--scroll-update plist)))

                     (:use-handler
                      on-unfocus :no-rerender (_)
                      (unless (eq scrollbar? 'always)
                        (lens-force-edit (lens--remove-scrollbar $id))))

                     (:let info
                           (list :id $id
                                 :height (or height 10)
                                 :visual-lines? visual-lines?
                                 :scrollbar? scrollbar?
                                 :label? label?))
                     (:let focus-plist
                           (list :view =view=
                                 :on-focus update
                                 :update update
                                 :on-unfocus on-unfocus
                                 :info info))
                     (:let plist (append info focus-plist))

                     (lens-use-subcomponent
                      =view=
                      subview-args
                      (lambda (cb)
                        (lens--scroll-clear-invisible plist)
                        (unwind-protect (funcall cb)
                          (lens--scroll-make-invisible plist)
                          (lens-force-edit
                           (put-text-property
                            (point-min) (point-max) 'keymap
                            (lens-keys "<tab>" (lambda () (interactive) (lens--focus focus-plist))
                                       "<return>" #'lens-click))))))

                     (lens-force-edit
                      (put-text-property
                       (point-min) (point-max) 'keymap
                       (lens-keys "<tab>" (lambda () (interactive) (lens--focus focus-plist))
                                  "<return>" #'lens-click))))


;;;;; Update

(defun lens--scroll-update (plist)
  (lens--scroll-clear-invisible plist)
  (lens--scroll-make-invisible plist))

(defcustom lens-scroll-hidden-props '((invisible . :id))
  "Alist of properties used to hide text in the scroll area.

If a particular value equals `:id', then the value used will be the id
of the current view."
  :type 'alist)

(cl-defun lens--scroll-clear-invisible ((&key id &allow-other-keys))
  (pcase-dolist (`(,prop . ,val) lens-scroll-hidden-props)
    (lens-force-edit
     (lens--pop-property (point-min) (point-max) prop (if (eq val :id) id val)))))

(cl-defun lens--scroll-make-invisible ((&key view id height visual-lines? scrollbar? &allow-other-keys))
  (pcase-let*
      ((ideal-before (/ (1- height) 2))
       (move-fn (if visual-lines? #'vertical-motion
                  (lambda (n) (- n (forward-line n)))))
       ;; If not in the scrollbox, unfocus
       (visible-beg nil)
       (visible-end nil)
       (lines-before nil)
       (lines-after nil)
       (total-lines nil)
       (hide-fn (lambda (beg end)
                  (pcase-dolist (`(,prop . ,val) lens-scroll-hidden-props)
                    (lens--push-property beg end prop (if (eq val :id) id val))))))

    (lens-force-edit
     (goto-char lens-point)
     ;; Move to the start of the first visible line
     (setq lines-before (- (funcall move-fn (- ideal-before))))
     (setq visible-beg (point))
     ;; Move to the start of the line after the last visible line
     (goto-char lens-point)
     (setq lines-after (1- (funcall move-fn (1+ (- height 1 lines-before)))))
     (setq visible-end (1- (point)))
     ;; Update the overlay obscuring everything after the visible region
     (funcall hide-fn visible-end (point-max))

     (goto-char visible-beg)
     ;; If there were not enough lines after the cursor, try adding more before the cursor
     (let ((more-lines (- height 1 lines-before lines-after)))
       (when (and (eq lines-before ideal-before) (> more-lines 0))
         (setq lines-before (+ lines-before (- (funcall move-fn (- more-lines)))))
         (setq visible-beg (point))))

     ;; Update the overlay obscuring everything before the visible region
     (setq total-lines (1- (line-number-at-pos (point-max))))
     (funcall hide-fn (point-min) (point))

     ;; Show the scrollbar
     ;; The scrollbar represents the distance in logical lines.
     ;; It could be made to represent visual lines, but it would be very slow
     (when (or (eq scrollbar? 'always)
               ;; If scrollbar? is just t, then only show the scrollbar if
               ;; this view is currently focused
               (and scrollbar? (eq view (plist-get lens-focused :view))))
       (lens--make-scrollbar id (point-min) (+ lines-before lines-after 1)
                             total-lines (if (> height total-lines) (+ total-lines height) height)
                             (line-number-at-pos visible-beg)
                             visual-lines?)))))


;;;;; Scrollbar

(defface lens-scrollbar
  '((((class color) (background dark)) :background "#08340e")
    (((class color) (background light)) :background "#66bc7b")
    (t :background "#08340e"))
  "Face for the scrollbar track background."
  :group 'lens)

(defface lens-scrollbar-thumb
  '((((class color) (background dark)) :background "#4f7933")
    (((class color) (background light)) :background "#457e4d")
    (t :background "#4f7933"))
  "Face for the scrollbar thumb (the draggable handle)."
  :group 'lens)

(define-fringe-bitmap 'lens-empty "\x00\x00\x00\x00\x00\x00\x00\x00")

(defun lens--make-scrollbar (id start height total shown first &optional visual)
  "Create a scrollbar in the right margin.

START is the position in the buffer to start creating the scrollbar.
HEIGHT is the height of the scrollbar in visual or logical lines.
If VISUAL is non-nil, use visual lines instead of logical lines.
TOTAL, SHOWN, and FIRST determine the proportions of the scrollbar."
  (lens-log debug "Creating scrollbar" start height total shown first)
  (when (and (> total 0) (> shown 0) (> height 0))
    ;; Naively assume that all lines have the same line-prefix and wrap-prefix
    (let* ((move-fn (if visual #'vertical-motion #'forward-line))
           (thumb-h (max 1 (floor (/ (* shown height) total))))
           (thumb-start (if (>= (+ first shown) total) (- height thumb-h)
                          (floor (/ (* height first) total))))
           (non-thumb (propertize " " 'display '(right-fringe lens-empty lens-scrollbar)))
           (thumb (propertize " " 'display '(right-fringe lens-empty lens-scrollbar-thumb)))
           pos)

      (save-excursion
        (goto-char start)
        ;; Create the top section
        (funcall move-fn thumb-start)
        (lens--add-line-prefix start (point) id non-thumb)

        ;; Create the thumb
        (setq pos (point))
        (funcall move-fn thumb-h)
        (lens--add-line-prefix pos (point) id thumb)

        ;; Create the bottom section
        (setq pos (point))
        (funcall move-fn (1+ (- height thumb-start thumb-h)))
        (lens--add-line-prefix pos (point) id non-thumb)))))


;;; ============================================================
;;; Keymaps

(defcustom lens-focus-map
  (lens-keys
   "<escape>" #'lens-unfocus)
  "Keymap active when any element is focused."
  :type '(cons (const keymap) (repeat (other nil)))
  :group 'lens)

(setf (alist-get 'lens-focused minor-mode-map-alist) lens-focus-map)


;;; ============================================================
;;; End

(provide 'lens)
;;; lens.el ends here

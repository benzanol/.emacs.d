;;; lens.el -*- lexical-binding: t; -*-

(require 'f)
(require 'dash)
(require 'org)
(require 'text-property-search)

;;; Variables
;;;; User Variables

(defvar lens-catch-display-errors t
  "Non-nil if display errors should be caught.

If a display function throws an error, the lens body will show
the error text instead of the normal contents of the lens.

If nil, the error will be thrown as normal, and the lens
redisplay will be cancelled.")

(defvar lens-save-lenses-on-save t
  "If non-nil, save lens source buffers when saving a buffer.")


;;;; Internal Variables

(defvar-local lens--buffer-referencers nil
  "List of lens buffers that reference the current buffer.")

(defvar lens--modified-fields nil
  "List of (:buffer BUF :pos MARKER :undo? BOOL).")


;;; Utilities
;;;; Utils

(defmacro lens-save-position (&rest body)
  "Save the line and column of the cursor when executing BODY."
  `(let* ((line (line-number-at-pos)) (col (current-column)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)))

(defun lens--refresh-buffer (&optional beg end)
  "Called after modifying a buffer region between BEG and END."

  ;; Refresh org indent mode
  (when (and (boundp 'org-indent-mode) org-indent-mode)
    (if (and beg end) (org-indent-add-properties beg end)
      (org-indent-indent-buffer))))

(defmacro lens-silent-edit (&rest body)
  "Perform the changes in BODY without any consequences."
  `(let ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t))
     (save-excursion (atomic-change-group ,@body))))

(defun lens--make-symbol (name &rest props)
  "Create a custom symbol with the base name NAME.

PROPS is a plist of properties to apply to the symbol."
  (let ((sym (make-symbol (format "%s-%05d" name (random 99999)))))
    (while props
      (put sym (pop props) (pop props)))
    sym))

(defun lens--make-sticky (str &optional beg end)
  "Set the stickiness properties of the edges of STR.

The beginning will be sticky if BEG is non-nil, and the end will
be sticky if END is non-nil. Thus, the default behavior is
actually to make both ends of STR not sticky."
  (put-text-property 0 1 'front-sticky beg str)
  (put-text-property (1- (length str)) (length str) 'rear-nonsticky (not end) str)
  str)

(defun lens--append-face (beg end face &optional object)
  "Append FACE to both the `face' and `font-lock-face' properties.

If the face already exists, it will not be added.

BEG and END are the beginning and end of the region the face is
added to. If OBJECT is a string, add the properties to it instead
of the buffer."
  (when face
    (let ((fn `(lambda (f)
                 (cond ((eq ',face f) f)
                       ((or (not (listp f)) (keywordp (car f))) (list f ',face))
                       ((memq ',face f) f)
                       ((append f (list ',face)))))))
      (alter-text-property beg end 'face fn object)
      (alter-text-property beg end 'font-lock-face fn object))))


;;;; Regions

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

  (let* ((inhibit-point-motion-hooks t)
         (beg (text-property-search-forward beg-prop val pred))
         (beg-val (when beg (prop-match-value beg)))
         (end (when beg-val (text-property-search-forward end-prop beg-val #'eq))))
    (when end
      (list beg-val
            (prop-match-beginning beg) (prop-match-end beg)
            (prop-match-beginning end) (prop-match-end end)))))

(defun lens--region-at-point (beg-prop end-prop)
  "Search for a region containing the point.

The bounds of the region are specified by BEG-PROP and END-PROP,
as described by `lens--region-search-forward'."
  (let* ((inhibit-point-motion-hooks t)
         (start (point))
         (hb nil) (he nil)
         (pred (lambda (_nil val)
                 (and val
                      ;; Check if the previous region of BEG-PROP
                      ;; matches the value of END-PROP
                      (setq he (previous-single-property-change (1+ (point)) beg-prop))
                      (setq hb (or (previous-single-property-change he beg-prop) (point-min)))
                      (<= hb start)
                      (eq (get-text-property hb beg-prop) val))))
         ;; Search forward for values of END-PROP, and at each match,
         ;; check if it is the end of a region containing the point
         (end-match (save-excursion (text-property-search-forward end-prop nil pred))))
    (when end-match
      (list (prop-match-value end-match) hb he
            (prop-match-beginning end-match) (prop-match-end end-match)))))


(defun lens-search-forward (&optional val pred)
  "Search forward for a lens region.

VAL and PRED can be used to search for a particular region,
following the same convention as `text-property-search-forward'."
  (lens--region-search-forward 'lens-begin 'lens-end val pred))

(defun lens-at-point (&optional noerror)
  "Return the region of the lens containing the point.

If NOERROR is nil, throw an error if no lens is found."
  (or (lens--region-at-point 'lens-begin 'lens-end)
      (unless noerror (error "No lens at point"))))


;;; Lens Lifecycle
;;;; Creating

(defvar lens-default-style nil
  "Style to use for lenses if no other style is specified.")

(defun lens--generate-headers (lens)
  "Generate the header and footer strings for LENS.

Returns (HEAD-STRING . FOOT-STRING).

The footer contains a trailing newline, but the header does not.

To ensure that the undo-history stores the begin-lens and
end-lens properties, the header and footer contain a random
number. This ensures that the undo system knows that it has
changed, ensuring that it stores the new lens value."
  (-let* (((&plist :head-props hps :foot-props fps :head-face hf :foot-face ff) (get (car lens) :style))
          (rand (format "%05d" (random 99999)))
          (title (funcall (or (plist-get (get (car lens) :source) :title) #'ignore)))
          (h-text (format "[%s] %s" rand (or title "")))
          (h (apply #'propertize h-text 'lens-begin lens 'read-only t hps))
          (f (apply #'propertize (format "[%s]\n" rand) 'lens-end lens 'read-only t fps)))
    ;; Add the head-face and foot-face specified in the style
    (lens--append-face 0 (length h) hf h)
    (lens--append-face 0 (length f) ff f)
    ;; Make the header and footer non-sticky.
    (cons (lens--make-sticky h) (lens--make-sticky f))))

(defun lens--generate-insert-text (lens)
  "Generate the full insert string for LENS."

  (let* ((insert-fn (plist-get (get (car lens) :display) :insert))
         ;; Catch display errors if `lens-catch-display-errors' is non-nil
         (inside (condition-case err (funcall insert-fn (car lens) (copy-tree (caddr lens)))
                   (error (unless lens-catch-display-errors (error err))
                          (propertize (format "\n%s\n" err) 'face 'error 'font-lock-face 'error))))
         (hs (lens--generate-headers lens))
         (style (get (car lens) :style)))

    ;; The first character of the insert text should be a newline,
    ;; which is styled as part of the header
    (add-text-properties 0 1 (plist-get style :head-props) inside)
    (lens--append-face 0 1 (plist-get style :head-face) inside)

    ;; Style the body of the insert text
    (add-text-properties 1 (length inside) (plist-get style :props) inside)
    (lens--append-face 1 (length inside) (plist-get style :face) inside)

    (concat (car hs) inside (cdr hs))))

(defun lens-create (source display &optional style)
  "Insert a new lens into the current buffer.

A lens is a way to display and interact with text data from some
source location (e.g. a buffer, file, region, etc).

A lens object is a 3-tuple: (SPEC, TEXT, STATE). SPEC stores the
SOURCE, DISPLAY, and STYLE objects. TEXT is the raw textual data from the
source location. STATE is parsed from the text data.


SOURCE describes how to keep the lens text in sync with the
source of the data (the buffer/file/etc). It is a plist
containing the following functions:

:init() -> STRING - Return the initial text data from the source.
This is only called when the lens is first created.

:update(TEXT) - Update the source location with the new lens content.
Called with the new lens text whenever the lens is modified.

:replace(TEXT) -> STRING - When the lens is removed, the output
will be inserted where the lens used to be.

:save?(TEXT) - When non-nil, this will be called whenever a save
signal is sent to the lens (e.g. by saving the buffer the lens is
in, if `lens-save-lenses-on-save' is non-nil.)

:title?() -> STRING - Returns a title to include in the lens header.


DISPLAY describes how to translate between the lens text, lens
state, and the output actually displayed by the lens. It is a
plist with the following functions:

:tostate(TEXT) -> STATE - Generate the lens state from the text.
This is only called when the lens is first created, or when a
modification to the data source updates the lens.

:totext(STATE) -> TEXT - Generate the lens text from the state.
This is called whenever the lens is modified.

:insert(STATE) -> STRING - Generate the actual string to display
in the buffer. This is called whenever the lens is redisplayed.


STYLE describes custom formatting for the lens. If STYLE is nil,
the value of `lens-default-style' will be used in its place.
The following are valid style properties:

:face, :head-face, :foot-face - Specify a face to be appended to
the `face' and `font-lock-face' properties.

:props, :head-props, :foot-props - A plist of text properties to
be set on the corresponding regions. Avoid using this to set the
face, as it will trample all other faces in the region."

  (when (lens-at-point t) (error "Cannot create a lens inside of another lens"))
  ;; If `lens--buffer-referencers' is non-nil, then there is a lens
  ;; somewhere referencing the current buffer
  (when lens--buffer-referencers (error "Cannot create a lens in a buffer which is viewed by a lens"))

  (unless style (setq style lens-default-style))
  (let* ((spec (lens--make-symbol "spec" :source source :display display :style style))
         (text (funcall (plist-get source :init)))
         (state (funcall (plist-get display :tostate) text))
         (lens (list spec text state))

         (insert (lens--generate-insert-text lens)))

    (lens-silent-edit (insert insert))

    ;; Before save
    (add-hook 'before-save-hook #'lens--before-save nil 'local)
    ;; Buffer substring filter
    (make-local-variable 'buffer-substring-filters)
    (add-to-list 'buffer-substring-filters #'lens-remove-lenses-from-string)

    (lens--refresh-buffer (point) (+ (point) (length insert)))))


;;;; Modifying

(defun lens-modify (region new &optional external norefresh)
  "Update the lens currently at REGION with the new data NEW.

EXTERNAL specifies if the new value provided was a new text
value, rather than a new state value. In this case, don't call
the update function (the new text is already saved).

NOREFRESH indicates that only the header and footer should be
reinserted."
  (pcase-let* ((`((,spec ,oldtext ,oldstate) ,hb ,he ,fb ,fe) region)
               (display (get spec :display))
               (source (get spec :source))

               ;; Generate the new lens to replace the old lens with
               (`(,newtext ,newstate)
                (if external (list new (funcall (plist-get display :tostate) new))
                  (list (funcall (plist-get display :totext) new) new)))
               (newlens (list spec newtext newstate)))

    ;; If nothing was changed, then leave the current lens as is.
    ;; Replacing the lens when it hasn't changed can lead to issues
    ;; with the undo history
    (unless (and (string= oldtext newtext) (equal oldstate newstate))
      (lens-save-position
       (lens-silent-edit
        ;; Refresh the style properties
        (add-text-properties (1+ he) fb (plist-get (get spec :style) :props))
        (lens--append-face (1+ he) fb (plist-get (get spec :style) :face))

        (if norefresh
            ;; Reinsert only the header and footer
            (let ((strs (lens--generate-headers newlens)))
              (goto-char fb) (delete-region fb fe) (insert (cdr strs))
              (lens--refresh-buffer fb (point))
              (goto-char hb) (delete-region hb he) (insert (car strs))
              (lens--refresh-buffer hb (point)))

          ;; Reinsert only the entire lens
          (let ((insert (lens--generate-insert-text newlens)))
            (delete-region hb fe)
            (insert insert)
            (lens--refresh-buffer hb (point))))))

      ;; Update the data source if this was a direct modification to the lens
      (unless external (funcall (plist-get source :update) newtext)))))


;;;; Removing

(defun lens-remove (region)
  "Remove the lens at REGION."
  (interactive (list (lens-at-point)))
  (lens-unfold (car region))

  (pcase-let* ((`((,spec ,text ,state) ,hb ,he ,fb ,fe) region)
               (replace (funcall (plist-get (get spec :source) :replace) text)))
    (lens-silent-edit
     (delete-region hb fe)
     (remove-overlays (point-min) (point-max) 'lens-box-id spec)
     (insert replace))

    (lens--refresh-buffer hb (+ hb (length replace)))))

(defun lens-run-on-all (func &optional noerror)
  "Run FUNC on all lenses in the buffer.

FUNC will be called with one argument, the region of the lens. If
NOERROR is non-nil, demote errors thrown by FUNC to messages."
  (let (region)
    (save-excursion
      (goto-char (point-min))
      (while (setq region (lens-search-forward))
        (condition-case err (funcall func region)
          (error (unless noerror (message "Error in `lens-run-on-all': %s" (cadr err)))))))))

(defun lens-remove-all ()
  "Remove all lenses in the current buffer."
  (interactive)
  (lens-run-on-all #'lens-remove t))

(defun lens-remove-lenses-from-string (string)
  "Return STRING with all lenses removed."

  ;; If the string contains the beginning or end of a lens, then remove lenses
  (if (or (text-property-not-all 0 (length string) 'lens-begin nil string)
          (text-property-not-all 0 (length string) 'lens-end nil string))
      (with-temp-buffer
        (lens-silent-edit
         (insert string)
         (lens-remove-all)
         (buffer-string)))

    ;; Either it contains no lenses, or just contains some contents of a lens
    string))


;;;; Saving

(defvar-local lens--presave nil
  "Stores the buffer contents before removing the lenses.
Should be nil or a plist of the form:
\(:text TEXT :pos POINT :overlays OVERLAYS)")

(defun lens--before-save ()
  "Remove lenses from the buffer before saving."
  (when (text-property-not-all (point-min) (point-max) 'lens-begin nil)
    (setq lens--presave
          (list :text (buffer-string) :pos (point)
                :overlays (--map (list (overlay-start it) (overlay-end it) (overlay-properties it))
                                 (overlays-in (point-min) (point-max)))))
    (add-hook 'after-save-hook #'lens--after-save-once nil 'local)

    (when lens-save-lenses-on-save
      (lens-run-on-all
       (lambda (region)
         (let* ((lens (car region))
                (text (cadr lens))
                (src (get (car lens) :source)))
           (funcall (plist-get src :update) text)
           (funcall (or (plist-get src :save) #'ignore) text)))
       :noerror))
    (lens-remove-all)))

(defun lens--after-save-once ()
  "After saving, restore the lenses to the buffer."
  (when lens--presave
    (lens-silent-edit
     (remove-overlays (point-min) (point-max))
     (delete-region (point-min) (point-max))
     (insert (plist-get lens--presave :text))
     (dolist (old (plist-get lens--presave :overlays))
       (let ((o (make-overlay (car old) (cadr old)))
             (ps (caddr old)))
         (while ps (overlay-put o (pop ps) (pop ps))))))
    (goto-char (plist-get lens--presave :pos))

    (remove-hook 'after-save-hook #'lens--after-save-once 'local)
    (setq lens--presave nil)

    (set-buffer-modified-p nil)))



;;; Some Sources and Displays
;;;; Region Source

(defun lens-replace-source (str &optional before after)
  "A lens source replacing text STR in the current buffer.

BEFORE and AFTER are strings which were before/after STR in the
buffer, but should not be included in the lens text."
  (setq before (or before "") after (or after ""))
  (list :init (lambda () str)
        :update (lambda (_text))
        :replace (lambda (text) (concat before text after))))


;;;; Buffer Source

(defun lens--add-buffer-referencer (source-buf lens-buf)
  "Mark LENS-BUF as having a lens which references SOURCE-BUF."
  (with-current-buffer source-buf
    (add-to-list 'lens--buffer-referencers lens-buf t)
    (add-hook 'after-change-functions #'lens--refresh-buffer-referencers nil 'local)))

(defun lens--refresh-buffer-referencers (&rest _after-change-args)
  "Refresh all lenses which reference the current buffer.

This function is designed to be set up as an `after-change-hook',
hance _AFTER-CHANGE-ARGS, although the args are ignored."
  ;; Remove killed buffers
  (setq lens--buffer-referencers (-filter #'buffer-live-p lens--buffer-referencers))

  ;; Save information from the source buffer, since we will be moving between buffers
  (let* ((str (string-trim (buffer-substring-no-properties (point-min) (point-max)) "\n*" "\n*"))
         (src-buf (current-buffer)) (src-file buffer-file-name)
         ;; Preicate to check if a lens referenced the source buffer/file
         (lens-pred (lambda (_ lens)
                      (or (eq src-buf (plist-get (get (car lens) :source) :source-buffer))
                          (and src-file (string= src-file (plist-get (get (car lens) :source) :source-file))))))
         position)

    (dolist (lens-buf lens--buffer-referencers)
      (with-current-buffer lens-buf
        (save-excursion
          ;; Search for all matching lenses in the buffer
          (goto-char (point-min))
          (while (setq position (lens-search-forward nil lens-pred))
            (unless (string= (string-trim (cadr (car position)) "\n*" "\n*") str)
              (lens-modify position str :external))))))))

(defun lens--update-source-buffer (buf text)
  "Update a source buffer BUF with modified lens content TEXT."
  (with-current-buffer buf
    (unless (string= text (buffer-string))
      ;; Update the buffer
      (lens-silent-edit
       (remove-overlays (point-min) (point-max))
       (delete-region (point-min) (point-max))
       (insert text))
      ;; Update other lenses
      (lens--refresh-buffer-referencers))))


(defun lens-buffer-source (buf &optional replaced)
  "Create a source referencing buffer BUF and replacing REPLACED."
  (setq buf (get-buffer buf))
  (list :source-buffer buf
        :init
        (lambda ()
          (lens--add-buffer-referencer buf (current-buffer))
          (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
        :update
        (lambda (text)
          (lens--add-buffer-referencer buf (current-buffer))
          (lens--update-source-buffer buf text))
        :replace
        (lambda (_text) (or replaced ""))
        :save
        (lambda (_text)
          (with-current-buffer buf (when buffer-file-name (save-buffer))))
        :title (lambda () (buffer-name buf))))


;;;; File Source

(defvar lens--file-referencers nil "Alist of files to buffers with a file source.")
(defun lens--add-file-referencer (file lens-buf)
  "Mark LENS-BUF as referencing the file FILE."
  (setq file (expand-file-name file) lens-buf (get-buffer lens-buf))
  (let ((assoc (assoc file lens--file-referencers))
        (file-buf (get-file-buffer file)))

    (cond ((null assoc) (push (list file lens-buf) lens--file-referencers))
          ((not (memq lens-buf (cdr assoc))) (push file (cdr assoc))))

    (when file-buf (lens--add-buffer-referencer file-buf lens-buf))))

(add-hook 'find-file-hook 'lens-find-file-hook)
(defun lens-find-file-hook ()
  "After finding a file, determine if any lenses reference it."
  (let ((pair (assoc (expand-file-name buffer-file-name) lens--file-referencers)))
    (dolist (lens-buf (cdr pair))
      (lens--add-buffer-referencer (current-buffer) lens-buf))))

(defun lens-file-source (file &optional replaced)
  "Create a source for referencing FILE, and replacing REPLACED."
  (setq file (expand-file-name file))
  (list :source-file file
        :init
        (lambda ()
          (lens--add-file-referencer file (current-buffer))
          (let ((buf (get-file-buffer file)))
            (if (null buf) (f-read file)
              (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))))
        :update
        (lambda (text)
          (lens--add-file-referencer file (current-buffer))
          (when (get-file-buffer file) (lens--update-source-buffer (get-file-buffer file) text)))
        :replace
        (lambda (_plist) (or replaced ""))
        :save
        (lambda (text)
          (if (null (get-file-buffer file)) (write-region text nil file)
            (with-current-buffer (get-file-buffer file) (save-buffer))))
        :title (lambda () (f-relative file default-directory))))


;;;; Raw display

(bz/face lens-field-header fixed-pitch :fg gray3 :h 0.9)
(defun lens--field (text onchange &optional head foot)
  "Create a controlled, modifyable region of text.

TEXT is the initial content of the region. ONCHANGE is a function
to be called with the new content every time the region is
modified.

The region will be bounded before and after by a read-only header
and footer. HEAD and FOOT can indicate the text of the header and
footer.

Every time the field is modified, it will be marked as modified
in `lens--modified-fields'. Then, in the `post-command-hook', the
new region content will be edited to have the correct properties,
and then the ONCHANGE function will be called."
  (setq head (or head "---\n") foot (or foot "\n---"))
  (let* ((field (lens--make-symbol "field"))
         (hooks '(lens--field-modification-hook)))
    (fset field onchange)
    (put-text-property (1- (length head)) (length head) 'insert-behind-hooks hooks head)

    (dolist (prop '(face font-lock-face))
      (setq head (propertize head prop 'lens-field-header)
            foot (propertize foot prop 'lens-field-header)))

    (concat (lens--make-sticky (propertize head 'field-begin field 'read-only t))
            (propertize text 'modification-hooks hooks 'insert-behind-hooks hooks)
            (lens--make-sticky (propertize foot 'field-end field 'read-only t)))))

(defun lens--field-modification-hook (beg _end)
  "Mark the field starting at BEG as modified."

  (push (list :buffer (current-buffer) :pos (set-marker (make-marker) beg)
              ;; If it was modified by an undo action, mark it as such
              :undo (when (--find (eq (cadr it) #'primitive-undo) (backtrace-frames)) t))
        lens--modified-fields)
  (add-hook 'post-command-hook #'lens--field-modification-callback))

(defun lens--field-modification-callback ()
  "Perform all necessary actions to update the modified fields.

This function is expected to be called in the
`post-command-hook', after one or more fields were modified by
the previous command, as described in `lens--field'."

  (remove-hook 'post-command-hook #'lens--field-modification-callback)
  (let (saved-lenses)
    (dolist (plist (prog1 lens--modified-fields (setq lens--modified-fields nil)))
      (with-current-buffer (plist-get plist :buffer)
        (lens-silent-edit
         (goto-char (plist-get plist :pos))

         (if (plist-get plist :undo)
             ;; If it was an undo action, we only need to update the restored lens
             (pcase-let ((`(,lens ,_hb ,_he ,_fb ,_fe) (lens-at-point t)))
               (when (and lens (not (memq lens saved-lenses)))
                 (push lens saved-lenses)
                 (funcall (plist-get (get (car lens) :source) :update) (cadr lens))))

           ;; Perform the procedure in case there was a proper modification
           (pcase-let ((`(,field ,hb ,he ,fb ,fe) (lens--region-at-point 'field-begin 'field-end)))
             (when field
               (dolist (prop '(modification-hooks insert-behind-hooks))
                 (put-text-property he fb prop '(lens--field-modification-hook)))
               (goto-char he)
               (funcall field (buffer-substring-no-properties he fb))

               (lens--refresh-buffer hb fe)))))))))


(defun lens--raw-display-onchange (text)
  "Value of onchange for `lens--field' used by `lens-raw-display'.

TEXT is the new text of the field."
  (let ((region (lens-at-point)))
    (lens-modify region text nil :norefresh)))

(defun lens--raw-display-insert (_spec state)
  "Generate the content for a raw display with state STATE."
  (let ((field (lens--field state #'lens--raw-display-onchange "\n" "\n")))
    (lens--make-sticky field :before :after)))

(defun lens-raw-display (&optional forward-filter backward-filter)
  "Display spec to display text data verbatim.

FORWARD-FILTER is called on the text data and returns the
displayed text. BACKWARD-FILTER is called on the displayed text
and returns the new value of the text data."
  (list :tostate
        (lambda (text)
          (funcall (or forward-filter #'identity) (string-trim text "\n+" "\n+")))
        :totext (or backward-filter #'identity)
        :insert #'lens--raw-display-insert))


;;;; Generating Uis

(bz/face lens-button custom-button)
(bz/keys lens-button-keymap
  :sparse t
  "<return>" lens-click)

(defun lens-click ()
  "If there is a ui button at point, call its onclick function."
  (interactive)
  (let ((click (get-text-property (point) 'lens-click)))
    (if click (funcall click)
      (error "Nothing to click"))))

(defun lens-string-width (str)
  "Calculate the pixel width of a string STR."
  (if (boundp #'string-pixel-width)
      (/ (string-pixel-width str) (string-pixel-width "a"))
    (require 'shr)
    (/ (shr-string-pixel-width str) (shr-string-pixel-width "a"))))

(defun lens--join-columns (cols &optional sep)
  "Vertically align a list of strings as columns.

COLS is the list of strings to align, and SEP is an additional
string to insert between the columns."
  (let* ((splits (--map (split-string it "\n") cols))
         (line-ct (apply #'max (-map #'length splits)))
         (lines (make-list line-ct ""))
         max-w cell-text)
    (dolist (col-lines splits)
      (setq max-w (apply #'max (-map #'lens-string-width col-lines)))

      (dotimes (i line-ct)
        (setq cell-text (or (nth i col-lines) ""))
        (setf (nth i lines)
              (concat (nth i lines)
                      (or (unless (eq col-lines (car splits)) sep) "")
                      cell-text
                      (make-string (- max-w (lens-string-width cell-text)) ?\s)))))
    (string-join lines (propertize "\n" 'read-only t))))

(defun lens--ui-element-to-string (elem cb)
  "Helper function for `lens--ui-to-string'.

ELEM is the ui element to convert to a string, and CB is the
callback to call after interactions with the ui."
  (pcase elem
    (`(box ,text ,onchange . ,plist)
     (lens--field text
                  (lambda (new)
                    (funcall (or onchange #'ignore) new)
                    (funcall cb (plist-get plist :refresh)))
                  "[begin box]\n" "\n[end box]"))

    (`(row . ,cols)
     (lens--join-columns (--map (lens--ui-element-to-string it cb) cols)
                         (propertize " " 'read-only t)))

    ;; Elements that are also valid columns

    ((or `(string ,str) (and (pred stringp) str))
     (propertize (if (stringp str) str (string-join str "\n"))
                 'read-only t))

    (`(button ,label ,onclick . ,plist)
     (propertize (format " %s " label) 'lens-click
                 (lambda ()
                   (funcall (or onclick #'ignore))
                   (funcall cb (not (plist-get plist :norefresh))))
                 'local-map (list 'keymap lens-button-keymap (current-local-map))
                 'font-lock-face (or (plist-get plist :face) 'lens-button)
                 'read-only t))))

(defun lens--ui-to-string (rows cb)
  "Convert the ui defined by ROWS to an insertable string.

ROWS is a list of ui elements, and CB is the callback to call
after interactions with the ui."
  (let ((row-strs (--map (lens--ui-element-to-string it cb) rows))
        (newline (propertize "\n" 'read-only t)))
    (concat newline (string-join row-strs newline) newline)))


;;;; Ui Display

(defun lens-ui-display (ui)
  "Display spec for custom ui definitions.

UI is a plist with properties :tostate, :totext, and :toui."
  (-let [(&plist :tostate tostate :totext totext :toui toui) ui]
    (list :tostate tostate
          :totext totext
          :insert
          (lambda (spec state-copy)
            (lens--ui-to-string
             (funcall toui state-copy)
             (lambda (&optional refresh)
               (let ((region (lens-at-point)))
                 (unless (eq (caar region) spec) (error "Incorrect lens at point"))
                 (lens-modify region state-copy nil (not refresh)))))))))


;;; Usable functions
;;;; Ui Functions

(defvar lens-ui-alist nil)

(defmacro lens-defui (name &rest body)
  (declare (indent 1) (doc-string 2))
  (let ((docstring (when (stringp (car body)) (pop body))))
    (list #'setf (list #'alist-get (list 'quote name) 'lens-ui-alist)
          (cons #'list (append (list :docstring docstring) body)))))

(defun lens-read-ui (&optional prompt)
  (let ((name (completing-read (or prompt "Ui: ") (mapcar #'car lens-ui-alist))))
    (alist-get (intern name) lens-ui-alist)))


(defun lens--replace-create (beg end src-func display)
  (let ((str (buffer-substring-no-properties beg end)))
    (lens-silent-edit
     (delete-region beg end)
     (lens-create (funcall src-func str) display))))

(defun lens--wrapped-create (hb he fb fe src-func display)
  (let ((head (buffer-substring-no-properties hb he))
        (body (buffer-substring-no-properties he fb))
        (foot (buffer-substring-no-properties fb fe)))
    (lens-silent-edit
     (delete-region hb fe)
     (lens-create (funcall src-func body head foot) display))))


;;;; Creation Functions

(defun lens-create-buffer (beg end buffer)
  (interactive (list (point) (if mark-active (mark) (point)) (read-buffer "Buffer: ")))
  (lens--replace-create beg end (lambda (str) (lens-buffer-source buffer str)) (lens-raw-display)))

(defun lens-create-file (beg end file)
  (interactive (list (point) (if mark-active (mark) (point)) (read-file-name "File: ")))
  (lens--replace-create beg end (lambda (str) (lens-file-source file str)) (lens-raw-display)))

(defun lens-create-ui (beg end ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (or (lens-read-ui) (error "No ui selected"))))
  (lens--replace-create beg end #'lens-replace-source (lens-ui-display ui)))

(defun lens-create-buffer-ui (beg end buffer ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (read-buffer "Buffer: ") (lens-read-ui)))
  (lens--replace-create beg end (lambda (str) (lens-buffer-source buffer str)) (lens-ui-display ui)))

(defun lens-create-new-file (path)
  (interactive (list (read-file-name "Create Lens: " "lenses/")))

  (let* ((rel1 (f-relative path (f-parent buffer-file-name)))
         (rel (if (string-match-p "\\`[/~]" rel1) rel1 (concat "./" rel1)))
         (link (format "[[%s]]\n" rel)))
    (mkdir (f-parent path) t)
    (f-touch path)
    (find-file-noselect path t)
    (lens-create (lens-file-source path link) (lens-raw-display))))


;;; Folding

(defun lens-fold (region)
  "Fold the lens at REGION, or at point."
  (interactive (list (lens-at-point)))
  (remove-overlays nil nil 'lens-fold (car region))

  (pcase-let* ((`(,lens ,_hb ,he ,fb ,fe) region)
               (face (plist-get (get (car lens) :style) :head-face))
               (o1 (make-overlay he (1- fb)))
               (o2 (make-overlay he fe))
               (o3 (when face (make-overlay (1- fb) fb))))
    (overlay-put o1 'lens-fold lens)
    (overlay-put o1 'invisible t)
    (overlay-put o1 'before-string (propertize "..." 'face face))

    (overlay-put o2 'lens-fold lens)
    (overlay-put o2 'intangible t)

    (when o3
      (overlay-put o3 'lens-fold lens)
      (overlay-put o3 'face face))))

(defun lens-unfold (lens)
  "Unfold LENS, or the lens at point."
  (interactive (list (car (lens-at-point))))
  (remove-overlays (point-min) (point-max) 'lens-fold lens))

(defun lens-fold-all ()
  "Fold all lenses in the current buffer."
  (interactive)
  (lens-run-on-all #'lens-fold))

(defun lens-unfold-all ()
  "Unfold all lenses in the current buffer."
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'lens-fold)
      (delete-overlay o))))

(defun lens-fold-toggle (region)
  "Toggle whether the lens at REGION, or at point, is folded."
  (interactive (list (lens-at-point)))
  (if (--find (eq (car region) (overlay-get it 'lens-fold))
              (overlays-in (point-min) (point-max)))
      (lens-unfold (car region))
    (lens-fold region)))

(defun lens-fold-toggle-all ()
  "Toggle whether all overlays in the current buffer are folded.

If all overlays are folded, unfold all overlays. Otherwise, fold
all overlays."
  (interactive)
  (let* ((os (overlays-in (point-min) (point-max)))
         (lenses (-uniq (--map (overlay-get it 'lens-fold) os)))
         (all-folded t))
    (lens-run-on-all
     (lambda (region)
       (unless (memq (car region) lenses)
         (setq all-folded nil))))
    (if all-folded
        (lens-unfold-all)
      (lens-fold-all))))


;;; Auto Mode
;;;; Org Matchers

(defvar lens-auto--org-block-re "^#\\+begin_lens \\([-_a-zA-Z0-9]+\\).*\n\\([^z-a]*?\\)\n#\\+end_lens\n")
(defun lens-auto--org-block ()
  (let ((ui (alist-get (intern (match-string 1)) lens-ui-alist))
        (hb (match-beginning 0)) (he (match-beginning 2))
        (fb (match-end 2)) (fe (match-end 0)))
    (when ui (lens--wrapped-create hb he fb fe #'lens-replace-source (lens-ui-display ui)))))


(defun lens--org-forward-filter (text)
  (replace-regexp-in-string "^\\*+ .*$" "_\\&_" text))

(defun lens--org-backward-filter (text)
  (replace-regexp-in-string "^_\\(\\*+ .*\\)_$" "\\1" text))

(defvar lens-auto--org-link-re (format "^\\(?:%s\\)\n" org-any-link-re))
(defun lens-auto--org-file-link ()
  (let* ((beg (match-beginning 0)) (end (match-end 0))
         (context (org-element-context))
         (link-type (and (eq (car context) 'link) (org-element-property :type context)))
         (path (and link-type (string= link-type "file") (org-element-property :path context))))
    (when path
      (lens--replace-create beg end (lambda (str) (lens-file-source path str))
                            (lens-raw-display #'lens--org-forward-filter
                                              #'lens--org-backward-filter)))))


(defvar lens-auto-matchers:org-mode
  `((,lens-auto--org-link-re lens-auto--org-file-link)
    (,lens-auto--org-block-re lens-auto--org-block)))


;;;; Mode

(defvar-local lens-auto-matchers nil
  "Alist of a regexp to a function.")

(defvar lens-auto-mode-alist
  `((org-mode . ,lens-auto-matchers:org-mode))
  "Alist of major modes to values of `lens-auto-matchers'.")

(define-minor-mode lens-auto-mode
  "Automatically create lenses based on regexps."
  :global nil
  :init-value nil
  (if (not lens-auto-mode) (lens-remove-all)
    (setq lens-auto-matchers (alist-get major-mode lens-auto-mode-alist))
    (lens-auto-insert-all)))

(defun lens-auto-search-forward ()
  (let ((init (point))
        match-pos match-assoc)
    (dolist (assoc lens-auto-matchers)
      (goto-char init)
      (and (re-search-forward (car assoc) nil :noerror)
           (or (null match-pos) (< (match-beginning 0) match-pos))
           (setq match-pos (match-beginning 0) match-assoc assoc)))
    (when match-pos
      (goto-char match-pos)
      (looking-at (car match-assoc)) ;; Set the match data
      match-assoc)))

(defun lens-auto-insert-all ()
  (interactive)
  (let (assoc end)
    (save-excursion
      (lens-remove-all)
      (goto-char (point-min))
      (while (setq assoc (lens-auto-search-forward))
        (setq end (match-end 0))
        (funcall (cadr assoc))
        (goto-char end)))))

(defun lens-auto-at-point ()
  (interactive)
  (let ((target (point))
        done assoc)
    (save-excursion
      (goto-char (point-min))
      (while (not done)
        (setq assoc (lens-auto-search-forward))
        (cond
         ;; If we have passed the target position
         ((or (null assoc) (> (match-beginning 0) target)) (error "No auto lens at point"))
         ;; If we have not yet reached the target position
         ((< (match-end 0) target) (goto-char (match-end 0)))
         ;; If we have found a lens containing the target position
         (t (funcall (cadr assoc)) (setq done t)))))))


;;; Boxes

(bz/face lens-box-overline :o "lightskyblue4")
(bz/face lens-box-underline :u (:color "lightskyblue4"))

(bz/face lens-box-body :bg bg :x t)
(bz/face lens-box-head (lens-box-body lens-box-overline fixed-pitch) :h 0.9 :fg blue :w bold)
(bz/face lens-box-foot (lens-box-body lens-box-underline fixed-pitch) :h 1)

(bz/face lens-box-vert :bg "lightskyblue4")
(bz/face lens-box-padding lens-box-body :bg bg)

(setq lens-box-margin 5)
(setq lens-box-padding 8)
(setq lens-line-prefix
      (let* ((space (lambda (w) (propertize " " 'display `(space :width (,w))))))
        (concat (funcall space lens-box-margin)
                (propertize (funcall space 1) 'face 'lens-box-vert)
                (propertize (funcall space lens-box-padding) 'face 'lens-box-padding))))
(setq lens-head-prefix (let ((s (substring lens-line-prefix)))
                         (prog1 s (add-face-text-property 2 3 'lens-box-overline nil s)
                                (add-face-text-property 2 3 '(:height 1.2) nil s))))
(setq lens-foot-prefix (let ((s (substring lens-line-prefix)))
                         (prog1 s (add-face-text-property 2 3 'lens-box-underline nil s)
                                (add-face-text-property 0 3 '(:height 1) nil s))))

(bz/keys lens-header-map
  :sparse t
  :parent org-mode-map
  [remap bz/fold-toggle] lens-fold-toggle
  )

(setq lens-box-style
      (list :props (list 'bz/line-prefix lens-line-prefix 'bz/wrap-prefix lens-line-prefix)
            :face 'lens-box-body
            :head-props (list 'bz/line-prefix lens-head-prefix 'bz/wrap-prefix lens-head-prefix
                              'local-map lens-header-map 'keymap lens-header-map)
            :head-face 'lens-box-head
            :foot-props (list 'bz/line-prefix lens-foot-prefix 'bz/wrap-prefix lens-foot-prefix)
            :foot-face 'lens-box-foot)
      lens-default-style lens-box-style)

;; (setq lens-default-style nil)

;;; UIs
(lens-defui raw
  :tostate
  (lambda (text)
    (list :text text))
  :totext
  (lambda (st)
    (plist-get st :text))
  :toui
  (lambda (st)
    `((box ,(plist-get st :text)
           ,(@1 (plist-put st :text @1))))))

(lens-defui test
  :tostate
  (lambda (text)
    (list :count (length (car (split-string text "\n")))
          :rest (s-join "\n" (cdr (split-string text "\n")))))
  :totext
  (lambda (st)
    (concat (make-string (max 0 (plist-get st :count)) ?-) "\n" (plist-get st :rest)))
  :toui
  (lambda (st)
    `((row
       (button "/2" ,(@0 (=> (plist-get st :count) / 2)))
       (button "-" ,(@0 (=> (plist-get st :count) - 1)))
       ,(format "#%s" (plist-get st :count))
       (button "+" ,(@0 (=> (plist-get st :count) + 1)))
       (button "x2" ,(@0 (=> (plist-get st :count) * 2))))
      (box ,(plist-get st :rest)
           ,(@1 (plist-put st :rest @1))))))

;; (lens-insert (lens-buffer-source (get-buffer "temp2.org")) (lens-raw-display))

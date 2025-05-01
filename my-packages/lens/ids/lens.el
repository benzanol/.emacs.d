;; SPEC   ::= (:buffer BUF :source SOURCE :text TEXT :padding (ABOVE BELOW) PROPS...)
;;          | (... :source (ui ...) :state STATE :ui UI)
;; SOURCE ::= (buffer BUFFER)
;;            (ui :ui-fn (STATE)=>ROWS :state-fn? (TEXT)=>STATE :text-fn? (STATE)=>TEXT)
;;            (remote)
;;
;; ROW    ::= ELEM
;;            (columns ELEMS)
;;            (box INIT :onchange? (STATE STRING)=>())
;;        & (:keymap? MAP)
;;
;; ELEM   ::= (string TEXT)
;;            (button LABEL :onclick (STATE)=>())
;;            (field INIT :onchange? (STATE STRING)=>())
;;        & (:keymap? MAP)

(require 'dash)
(require 'text-property-search)

;;; Variables

(defvar lens-before-fetch-buffer-hook nil
  "Hook run before getting the contents of a buffer to put in a lens.")

(defvar lens-after-modify-buffer-hook nil
  "Hook run after updating a buffer to reflect a lens.")

(defvar lens-modification-delay 1
  "Number of idle seconds before apply lens modifications.")

(defvar lens-map (make-sparse-keymap)
  "Keymap active anywhere in a lens.")

(defvar lens-header-map (make-sparse-keymap)
  "Keymap active on the top and bottom lines of lenses.")



(bz/face lens-block fixed-pitch :bg bg2 :x t)
(bz/face lens-header :fg gray3 :bg bg2 :x nil)
(bz/face lens-button custom-button)

(bz/keys lens-map :sparse t "RET" lens-click)

;;; Misc functions

(defmacro lens-save-position (&rest body)
  `(with-selected-window (or (get-buffer-window (current-buffer)) (selected-window))
     (let ((line (line-number-at-pos)) (col (current-column)))
       (unwind-protect (progn ,@body)
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (min (point-at-eol) (+ (point) col)))))))

(defun lens--search-forward (&optional spec)
  "Search for the next lens, and return (spec beg-beg beg-end end-beg end-end)."

  (let ((match (text-property-search-forward 'lens spec (when spec #'eq))))
    (when match
      (list (prop-match-value match)
            (progn (goto-char (prop-match-beginning match)) (point))
            (point-at-bol 3) ; Beginning of 2nd line below
            (progn (goto-char (prop-match-end match))
                   (point-at-eol -1)) ; End of 2nd line above
            (point)))))

(defun lens--refresh-buffer (&optional beg end)
  ;; Refresh org indent mode
  (when (and (boundp 'org-indent-mode) org-indent-mode)
    (if (and beg end) (org-indent-add-properties beg end)
      (org-indent-indent-buffer))))


;;; Text property functions

(defun lens-propertize (string &rest props)
  (unless (= 0 (mod (length props) 2))
    (error "Props has an odd length"))

  (dotimes (i (/ (length props) 2))
    (let* ((prop-elem (nth (* i 2) props))
           (prop-list (if (listp prop-elem) prop-elem (list prop-elem)))
           (prop (car prop-list))
           (start (or (cadr prop-list) 0))
           (end (or (caddr prop-list) (length string)))
           (val (nth (1+ (* i 2)) props)))

      (when (< end 0) (setq end (+ end (length string))))
      (when (< start 0) (setq start (+ start (length string))))

      (put-text-property start end prop val string)))
  string)

(defun lens-put-text-properties (beg end plist &optional obj)
  (dotimes (i (/ (length plist) 2))
    (put-text-property (or beg (if obj 0 (point-min)))
                       (or end (if obj (length obj) (point-max)))
                       (nth (* 2 i) plist) (nth (1+ (* 2 i)) plist)
                       obj))
  obj)

(defun lens-add-face (string face &optional beg end)
  (when face
    (let ((func (lambda (v) (if (listp v) (append v (list face)) (list v face)))))
      (remove-text-properties (or beg 0) (or end (length string)) '(face nil) string)
      (alter-text-property (or beg 0) (or end (length string)) 'font-lock-face func string)))
  string)


;;; Generating lens text content

(defun lens-string-width (str)
  "The normal string-width just doesn't work for some reason."
  (if (boundp #'string-pixel-width)
      (/ (string-pixel-width str) (string-pixel-width "a"))
    (require 'shr)
    (/ (shr-string-pixel-width str) (shr-string-pixel-width "a"))))

(defun lens-join-columns (cols &optional sep)
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
    (string-join lines "\n")))

(defun lens--generate-block (top bottom body)
  (concat
   (lens-propertize (format "\n%s\n" top)
                    'read-only t '(rear-nonsticky -1) t
                    '(font-lock-face 1 -1) 'lens-header)
   body
   (lens-propertize (format "\n%s\n" bottom)
                    'read-only t '(rear-nonsticky -1) t
                    '(font-lock-face 1 -1) 'lens-header)))

(defun lens--generate-textbox (text spec elem)
  (let* ((spec-elem (list spec elem))
         (text-props (list 'lens spec 'lens-text spec-elem))
         (insert-fn `(lambda (b e) (lens-put-text-properties b e ',text-props)))
         (modify-fn `(lambda (&rest _) (lens--mark-item-modified ',spec-elem))))

    (nconc text-props (list 'modification-hooks (list modify-fn)
                            'insert-in-front-hooks (list insert-fn modify-fn)
                            'insert-behind-hooks (list insert-fn modify-fn)))

    (lens-put-text-properties 0 nil text-props (substring text))))

(defun lens--generate-ui-element (spec elem)
  (pcase elem
    (`(box ,text . ,_)
     (let ((body (lens--generate-textbox text spec elem)))
       (string-trim (lens--generate-block "[begin box]" "[end box]" body))))

    (`(columns ,cols)
     (lens-join-columns (--map (lens--generate-ui-element spec it) cols)
                        (propertize " " 'read-only t)))

    ;; Elements that are also valid columns

    ((or `(string ,str) (and (pred stringp) str))
     (propertize (if (stringp str) str (string-join str "\n"))
                 'read-only t))

    (`(button ,label . ,plist)
     (propertize label 'lens-onclick (plist-get plist :onclick)
                 'font-lock-face (or (plist-get plist :face) 'lens-button)
                 'read-only t))

    (`(field ,text . ,_)
     (lens-propertize (format "[%s]" (lens--generate-textbox text spec elem))
                      '(read-only 0 1) t '(rear-nonsticky 0 1) t '(read-only -1) t))))

(defun lens--generate-ui (spec rows)
  (string-join (--map (lens--generate-ui-element spec it) rows)
               (propertize "\n" 'read-only t)))

(defun lens--get-buffer (spec)
  (let ((buf-src (pcase (plist-get spec :source)
                   (`(buffer ,buf-src . ,_) buf-src)
                   (_ (error "Not a buffer lens")))))

    (pcase buf-src
      ((pred bufferp) (when (buffer-live-p buf-src) buf-src))
      ((pred stringp) (or (get-file-buffer buf-src)
                          (lens--setup-target-buffer spec (find-file-noselect buf-src))))
      (`(mode ,mode-fn)
       (or (plist-get spec :temp-buffer)
           (setf (plist-get spec :temp-buffer)
                 (with-current-buffer (get-buffer-create (format "*lens-temp-%05d*" (mod (random) 100000)))
                   (insert (plist-get spec :text))
                   (funcall mode-fn)
                   (lens--setup-target-buffer spec (current-buffer)))))))))

(defun lens--generate-buffer-contents (spec)
  (let* ((source (plist-get spec :source))
         (buf-src (cadr source))
         (buf (lens--get-buffer spec))
         text overlays match)

    (cond
     ;; If the buffer is a loop
     ((eq buf (current-buffer)) (propertize "Infinite Loop" 'read-only t 'font-lock-face 'bold))

     ;; If it is a file, but it was closed
     ((and (stringp buf-src) (null buf))
      (lens-propertize "File Closed. Reopen" 'read-only t '(font-lock-face -6) 'link
                       '(lens-onclick -6) `(lambda (_) (lens--setup-target-buffer ',buf-src ',spec))))

     ;; If the buffer was killed
     ((and (bufferp buf) (not (buffer-live-p buf))) (propertize "Buffer Killed" 'read-only t))

     (t
      (with-current-buffer buf
        ;; Prepare the buffer for fetching the text
        (when font-lock-mode (font-lock-ensure))
        (run-hooks 'lens-before-fetch-buffer-hook)

        (setq text (filter-buffer-substring (point-min) (point-max)))
        (setq overlays (overlays-in (point-min) (point-max))))

      (with-temp-buffer
        (insert text) (goto-char (point-min))

        ;; Add overlay faces
        (dolist (ol overlays)
          (when (overlay-get ol 'face)
            (add-face-text-property (overlay-start ol) (overlay-end ol) (overlay-get ol 'face))))

        ;; Convert face properties into font-lock-face properties
        (while (setq match (text-property-search-forward 'face))
          (put-text-property (prop-match-beginning match) (prop-match-end match)
                             'font-lock-face (prop-match-value match)))

        (if (not (plist-get source :read-only))
            (lens--generate-textbox (buffer-string) spec source)

          (put-text-property (point-min) (point-max) 'read-only t)
          (buffer-string)))))))


;;; Creating lenses

(defvar-local lens--buffer-lenses nil "List of lenses viewing the current buffer.")

(defun lens-create (beg end source &rest props)
  "Create a new lens, encompassing the text from BEG to END.

SOURCE defines the content and behavior of the lens.
PROPS is a plist which can contain any of the following properties:
:ui-props, :title, :remote,
:block-face, :body-face,
:header-length, :footer-length"

  (when (text-property-not-all beg end 'lens nil)
    (error "Cannot create a lens on top of another lens"))

  (when lens--buffer-lenses
    (error "Cannot create a lens in a buffer which is viewed by a lens"))

  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)

         (spec `(:source ,source :buffer ,(current-buffer) . ,props))

         (pad-beg (or (plist-get props :header-length) 0))
         (pad-end (or (plist-get props :footer-length) 0))
         (raw-text (buffer-substring-no-properties beg end))
         (no-headers (substring raw-text pad-beg (- (length raw-text) pad-end)))

         (block-face (or (plist-get props :block-face) 'lens-block))
         (caption (or (plist-get props :title) (format "%s" (car source))))
         buf header footer)

    ;; Calculate the padding
    (setq pad-beg (+ pad-beg (progn (string-match "\\`\n*" no-headers)(match-end 0)))
          pad-end (+ pad-end (progn (string-match "\n*\\'" no-headers)
                                    (- (length no-headers) (match-beginning 0)))))
    (plist-put spec :text (substring raw-text pad-beg (- pad-end)))
    (plist-put spec :padding (list (substring raw-text 0 pad-beg)
                                   (substring raw-text (- (length raw-text) pad-end))))

    ;; Run source-specific set up
    (pcase source
      (`(buffer . ,_)
       (setq buf (lens--setup-target-buffer spec (lens--get-buffer spec)))
       (unless (plist-get props :title) (setq caption (buffer-name buf)))

       ;; Error if the lens is viewing itself
       (when (eq buf (current-buffer))
         (lens--cleanup-lens spec)
         (error "Cannot create a lens viewing its own buffer"))

       ;; Make sure that the buffer has no lenses
       (with-current-buffer buf
         (when (text-property-not-all (point-min) (point-max) 'lens nil)
           (if (y-or-n-p "Buffer contains lenses. Delete them?") (lens-remove-all)
             (lens--cleanup-lens spec)
             (error "Cannot create a lens viewing a buffer containing other lenses")))))

      (`(ui . ,fns)
       (let* ((orig (plist-get (or (plist-get props :remote) spec) :text))
              (fn (or (plist-get fns :state-fn) #'identity))
              (state (funcall fn orig (plist-get props :ui-props))))
         (plist-put spec :state state))))

    ;; Generate the full block
    (setq header (propertize (format "[ begin_lens %s ]" caption) 'lens-begin spec))
    (setq footer (propertize "[ end_lens ]" 'lens-end spec))

    (atomic-change-group
      (delete-region beg end)
      (goto-char (min beg end))
      (insert (lens-propertize (lens--generate-block header footer "")
                               'lens spec
                               'keymap lens-header-map
                               '(font-lock-face 1) (list 'lens-header block-face)))
      (lens--update-lens spec))

    ;; Add the before save hook
    (add-hook 'before-save-hook #'lens--before-save nil 'local)

    ;; Add a buffer substring filter
    (add-hook #'filter-buffer-substring-functions #'lens--filter-buffer-substring nil 'local)

    ;; Refresh org indent mode
    (lens--refresh-buffer (min beg end) (point))))


(defun lens--setup-target-buffer (spec buf)
  ;; If the buffer is a file that isn't open, open it
  (with-current-buffer buf
    (add-to-list 'lens--buffer-lenses spec t #'eq)
    (add-hook 'after-change-functions #'lens--mark-current-buffer-modified nil 'local))
  buf)


;;; Modifying lenses

(defvar lens--modified-items nil "List of spec-elem pairs or buffers.")

(defun lens--update-lens (spec)
  "After mutating the state, regenerate the original text and ui.
If the ui is changed, then redisplay the contents of the lens."
  (with-current-buffer (plist-get spec :buffer)
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t)
           new-text)

      (pcase (plist-get spec :source)
        (`(buffer ,buf . ,_) (setq new-text (lens--generate-buffer-contents spec)))
        (`(remote . ,_) (setq new-text (plist-get spec :text)))
        (`(ui . ,ui-fns)
         (let* ((state (plist-get spec :state))
                (new-ui (funcall (plist-get ui-fns :ui-fn) state))
                (new-orig (funcall (or (plist-get ui-fns :text-fn) #'identity) state))
                (remote (plist-get ui-fns :remote)))

           ;; Update the remote/original text based on changes to the state
           (plist-put (or remote spec) :text (string-trim new-orig "\n+" "\n+"))
           ;; Update the remote, or display a failure message if it's unsuccessful
           (when remote
             (condition-case err (lens--update-lens remote)
               (error (setq new-text (propertize "Unable to find remote." 'read-only t)))))

           ;; Determine whether the action warrants a redisplay
           (unless new-text
             (if (equal new-ui (plist-get spec :ui)) (message "UI Unchanged")
               (message "UI Changed")
               (plist-put spec :ui new-ui)
               (setq new-text (lens--generate-ui spec new-ui)))))))

      (when new-text
        ;; Propertize the new text
        (lens-add-face new-text (plist-get spec :body-face))
        (lens-add-face new-text (or (plist-get spec :block-face) 'lens-block))
        (put-text-property 0 (length new-text) 'lens spec new-text)
        (put-text-property 0 (length new-text) 'local-map lens-map new-text)

        ;; Replace the old body with the new text
        (lens-save-position
         (goto-char (point-min))
         (pcase-let ((`(,_ ,bb ,be ,eb ,ee) (or (lens--search-forward spec) (error "Failed to find lens"))))
           (atomic-change-group
             (goto-char be) (delete-region be eb) (insert new-text)
             )
           (lens--refresh-buffer bb (+ bb (- be bb) (length new-text) (- ee eb)))))))))

(defun lens-update-buffer-lenses (&optional buf)
  (dolist (spec (buffer-local-value 'lens--buffer-lenses (or buf (current-buffer))))
    (condition-case err
        (lens--update-lens spec)
      (error (message "Error updating lens: %s" (cadr err))
             (lens--cleanup-lens spec)))))


(defun lens--mark-item-modified (item)
  "Item is either a buffer or (spec elem)"
  (add-to-list 'lens--modified-items item 'append #'eq)

  (cancel-function-timers #'lens--update-modified-items)
  (run-with-timer lens-modification-delay nil #'lens--update-modified-items))

(defun lens--mark-current-buffer-modified (&rest _)
  (when lens--buffer-lenses (lens--mark-item-modified (current-buffer))))

(defun lens--update-modified-items ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        modified-specs content buf m)

    ;; First, loop through each modified item, and enact its modifications.
    ;; Afterward, loop through each affected lenses, and update its content.

    (dolist (item lens--modified-items)
      (condition-case err
          (pcase item
            ;; If the item is a buffer, add its lenses to the list of modified specs
            ((pred bufferp)
             (dolist (spec (buffer-local-value 'lens--buffer-lenses item))
               (add-to-list 'modified-specs spec t #'eq)))

            ;; If the item is a buffer lens, update the buffer contents
            (`(,spec (buffer . ,_))
             ;; If the buffer is gone, display the buffer gone message
             (if (null (setq buf (lens--get-buffer spec)))
                 (add-to-list 'modified-specs spec t #'eq)

               ;; Make sure the buffer is still updating the lens
               (lens--setup-target-buffer spec buf)

               ;; Get the inner text of the buffer lens
               (with-current-buffer (plist-get spec :buffer)
                 (save-excursion
                   (goto-char (point-min))
                   (pcase-let ((`(,_ ,_ ,be ,eb ,_) (lens--search-forward spec)))
                     ;; The contents doesn't need to be filtered, since lenses should never contain other lenses anyway
                     (setq content (buffer-substring-no-properties be eb)))))

               ;; Update the target buffer contents
               (with-current-buffer buf
                 (lens-remove-all)
                 (lens-save-position (delete-region (point-min) (point-max)) (insert content))
                 (run-hooks 'lens-after-modify-buffer-hook)
                 ;; Add the buffers other lenses to the modified specs list
                 (dolist (buf-spec lens--buffer-lenses)
                   (add-to-list 'modified-specs buf-spec t #'eq)))))

            ;; If the item is a ui element, update its cadr (text content)
            (`(,spec ,elem)
             ;; Get the element text
             (with-current-buffer (plist-get spec :buffer)
               (save-excursion
                 (goto-char (point-min))
                 (setq m (or (text-property-search-forward 'lens-text item #'eq)
                             (error "Unable to find element")))))

             (setf (cadr elem) (buffer-substring-no-properties (prop-match-beginning m) (prop-match-end m)))
             (funcall (plist-get elem :onchange) (plist-get spec :state) (cadr elem))
             (add-to-list 'modified-specs spec t #'eq)))

        (error (message "Error updating modified item: %s" (cdr err)))))

    ;; Update the contents of each affected lens
    (dolist (spec modified-specs)
      (condition-case err (lens--update-lens spec)
        (error (message "Error updating modified lens: %s" (cadr err))
               (lens--cleanup-lens spec))))

    (setq lens--modified-items nil)))

(defun lens-click ()
  (interactive)
  (let ((spec (get-text-property (point) 'lens))
        (onclick (get-text-property (point) 'lens-onclick)))
    (unless (and onclick spec) (error "No button at point."))

    (funcall onclick (plist-get spec :state))
    (lens--update-lens spec)))


;;; Removing lenses

(defun lens--replacement-text (spec)
  (let ((padding (plist-get spec :padding)))
    (concat (car padding) (plist-get spec :text) (cadr padding))))

(defun lens--cleanup-lens (spec)
  (pcase (plist-get spec :source)
    (`(buffer . ,_)
     (when (buffer-live-p (lens--get-buffer spec))
       (with-current-buffer (lens--get-buffer spec)
         (setq lens--buffer-lenses (delq spec lens--buffer-lenses)))))))

(defun lens-remove (spec &optional temporary)
  (interactive (list (or (get-text-property (point) 'lens)
                         (error "No lens at point"))))
  (with-current-buffer (plist-get spec :buffer)
    (save-excursion
      (goto-char (point-min))
      (pcase-let ((inhibit-read-only t) (inhibit-modification-hooks t)
                  (`(,spec ,beg ,_ ,_ ,end) (lens--search-forward spec)))
        (unless temporary (lens--cleanup-lens spec))
        (delete-region beg end)
        (insert (lens--replacement-text spec))))

    (unless temporary (lens--refresh-buffer))))

(defun lens-remove-all (&optional temporary)
  (interactive)

  (let ((inhibit-read-only t) (inhibit-modification-hooks t)
        spec beg end)

    (save-excursion
      (goto-char (point-min))
      (while (pcase-setq `(,spec ,beg ,_ ,_ ,end) (lens--search-forward))
        (unless temporary (lens--cleanup-lens spec))
        (delete-region beg end)
        (insert (lens--replacement-text spec))))

    (unless temporary (lens--refresh-buffer))))


;;; When to remove lenses

(add-hook 'before-revert-hook #'lens-remove-all)

;;; Saving buffers

(defvar-local lens-presave-string nil)
(defvar-local lens-presave-pos nil)

(defvar lens-save-linked-buffers t
  "If nil, don't save buffers associated with lenses when saving.")

(defun lens--before-save ()
  (when (text-property-not-all (point-min) (point-max) 'lens nil)
    (setq lens-presave-string (buffer-string) lens-presave-pos (point))

    (add-hook 'after-save-hook #'lens--after-save-once nil 'local)

    (let ((inhibit-read-only t) (inhibit-modification-hooks t)
          (saved-buffers (list (current-buffer)))
          spec beg end buf remote)
      (goto-char (point-min))

      (while (pcase-setq `(,spec . ,_) (lens--search-forward))
        (lens-remove spec 'temp)

        (when lens-save-linked-buffers
          (let ((lens-dont-save-linked-buffers nil))

            ;; Save the linked buffer, if there is one
            (and (setq buf (pcase (plist-get spec :source)
                             (`(buffer . ,_) (lens--get-buffer spec))
                             (`(ui . ,_) (plist-get (plist-get spec :remote) :buffer))))
                 (buffer-live-p buf)
                 (not (memq buf saved-buffers))
                 (with-current-buffer buf
                   (push buf saved-buffers)
                   (when buffer-file-name (save-buffer))))))))

    (lens--refresh-buffer)))

(defun lens--after-save-once ()
  (let ((inhibit-read-only t) (inhibit-modification-hooks t))
    (delete-region (point-min) (point-max))
    (insert lens-presave-string)
    (goto-char lens-presave-pos)

    (remove-hook 'after-save-hook #'lens--after-save-once 'local)
    (setq lens-presave-string nil lens-presave-pos nil)

    (set-buffer-modified-p nil)))


;;; Filter buffer substring

(defun lens--remove-lenses-from-string (string)
  ;; If the string contains the beginning or end of a lens, then remove lenses
  (if (or (text-property-not-all 0 (length string) 'lens-begin nil string)
          (text-property-not-all 0 (length string) 'lens-end nil string))
      (let ((inhibit-read-only t) (inhibit-modification-hooks t))
        (with-temp-buffer
          (insert string)
          (lens-remove-all 'temp)
          (buffer-string)))

    ;; Either it contains no lenses, or just contains some contents of a lens
    (remove-text-properties 0 (length string) '(lens nil) string)
    string))


(defun lens--filter-buffer-substring (fun start end delete)
  (lens--remove-lenses-from-string (funcall fun start end delete)))


;;; Useful functions

(defun lens-get (prop &optional lens)
  "Get the value of property PROP for LENS, or the lens at point."
  (setq lens (or lens (get-text-property (point) 'lens)
                 (error "No lens at point")))
  (plist-get lens prop))

(defmacro lens-with-buffer (&rest body)
  `(with-current-buffer (or (pcase (lens-get :source) (`(buffer ,buf . ,_) buf))
                            (error "Not in a buffer lens"))
     . ,body))


;;; Types of inserting

(defvar lens-custom-uis nil)
(defmacro lens-defui (name &rest body)
  (declare (indent 1) (doc-string 2))

  ;; Remove docstring
  (let ((docstring (when (stringp (car body)) (pop body))))
    `(setf (alist-get ',name lens-custom-uis)
           ,(list '\` (append (list :docstring docstring) body)))))

(defun lens-insert-ui (beg end name &rest props)
  (interactive
   (let ((uis (or (-map #'car lens-custom-uis) (error "No interactive uis defined"))))
     (list (point) (if mark-active (mark) (point))
           (intern (completing-read "Insert Ui: " uis nil t)))))

  (let* ((fns (or (alist-get name lens-custom-uis)
                  (error "Ui %s does not exist" name))))
    (apply #'lens-create beg end (cons 'ui fns) props)))

(defun lens-insert-mirror (beg end buffer)
  (interactive (list (point) (if mark-active (mark) (point))
                     (read-buffer "Insert Buffer View: " nil t)))
  (lens-create beg end (list 'buffer (get-buffer buffer))))

(defun lens-insert-remote (beg end)
  (interactive (list (point) (if mark-active (mark) (point))))
  (lens-create beg end (list 'remote)))


;;; Lens auto mode

(defvar lens-auto-regexp "^#\\+lens: \\([^\s\t\n]+\\)\\([^\t\n]*\\)")

(defvar lens-auto-include-header t)

(define-minor-mode lens-auto-mode
  "Auto insert lenses."
  :global nil
  :init-value nil
  (cond
   ((not lens-auto-mode) (lens-remove-auto-lenses))

   ;; Lens auto mode needs org mode to parse elements
   ((not (derived-mode-p 'org-mode))
    (setq lens-auto-mode nil)
    (error "Auto insert is only supported in org mode"))

   (t
    ;; Insert all auto lenses
    (let ((case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp lens-auto-regexp nil 'noerror)
          (lens-auto-insert)))))))

(defun lens-auto-insert ()
  (interactive)

  (unless (derived-mode-p 'org-mode)
    (error "Auto insert is only supported in org mode"))

  (save-excursion
    (beginning-of-line)
    (unless (let ((case-fold-search nil)) (looking-at lens-auto-regexp))
      (error "No auto lens at point")))

  (let* ((header-beg (max (point-min) (1- (match-beginning 0))))
         (body-beg (match-end 0))
         (lens-beg (if lens-auto-include-header header-beg body-beg))
         ;; Only for when the header is included in the range of the lens
         (header-len (when lens-auto-include-header (- body-beg header-beg)))
         ;; Parse the auto lens line
         (ui-name (intern (match-string 1)))
         (ui-props (split-string (match-string 2) " " t))
         ;; Parse the next org element to determine the scope of the new lens
         (elem-plist (save-excursion (forward-line 1) (cadr (org-element-at-point))))
         (lens-end (plist-get elem-plist :contents-end)))

    ;; Insert the lens ui
    (lens-insert-ui lens-beg lens-end ui-name
                    :ui-props ui-props
                    :header-length header-len
                    :auto t)))

(defun lens-remove-auto-lenses ()
  (interactive)

  (let (spec)
    (save-excursion
      (goto-char (point-min))
      (while (pcase-setq `(,spec . ,_) (lens--search-forward))
        (when (plist-get spec :auto)
          (lens-remove spec))))))


;;; Integrations

(add-hook 'lens-after-modify-buffer-hook 'lens-flycheck-defer)
(defun lens-flycheck-defer ()
  (when flycheck-mode
    (unless (flycheck-deferred-check-p)
      (flycheck-buffer-deferred)
      (run-with-idle-timer
       0.5 nil
       `(lambda () (with-current-buffer ,(current-buffer)
                     (flycheck-perform-deferred-syntax-check)))))))

(add-hook 'flycheck-after-syntax-check-hook 'lens-update-buffer-lenses)

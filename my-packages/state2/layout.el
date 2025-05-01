;;; Fmt

(defun s2--fmt-to-string (fmt)
  (if (or (stringp fmt) (keywordp (car fmt)))
      (s2--fmt-node-to-string fmt)
    (mapconcat #'s2--fmt-node-to-string fmt "")))

(bz/face s2-text)
(defun s2--fmt-node-to-string (node)
  (if (stringp node)
      (let ((copy (substring node)))
        (add-face-text-property 0 (length copy) 's2-text nil copy)
        copy)

    (let ((str (mapconcat #'s2--fmt-node-to-string (plist-get node :children) ""))
          (face (s2--create-fmt-face (plist-get node :style))))
      (add-face-text-property 0 (length str) face t str)
      str)))

(defun s2--create-fmt-face (style)
  (-let (((&plist :bold bold :italic italic :color color :size size) style)
         (face (list 'face)))

    (when bold   (nconc face (list :weight (if (eq bold t) 'bold 'normal))))
    (when italic (nconc face (list :slant (if (eq italic t) 'italic 'normal))))
    (when color  (nconc face (list :foreground color)))
    (when size   (nconc face (list :height (* size 1.0))))

    (cdr face)))


(defun s2--string-to-fmt (text)
  ;; Ensures object-intervals returns something even if the string has no properties
  (add-face-text-property 0 (length text) nil t text)

  (let (nodes faces)
    ;; Go through the list of the text prop intervals
    (pcase-dolist (`(,start ,end ,props) (object-intervals text))
      (setq faces (plist-get props 'face))
      ;; Check if "faces" is a single face, and set it to a list
      (when (or (not (listp faces)) (keywordp (car faces)))
        (setq faces (list faces)))

      (push (s2--generate-fmt-interval (substring-no-properties text start end) faces) nodes))

    (setq nodes (reverse nodes))

    (pcase (length nodes)
      (0 "")
      (1 (car nodes))
      (_ nodes))))

(defun s2--generate-fmt-interval (string faces)
  (let (style prop val)
    ;; Loop through faces that are lists
    (dolist (face (seq-filter #'listp faces))
      (when (listp faces)

        ;; Go through the properties of the face
        (dotimes (i (/ (length face) 2))
          (setq prop (nth (* i 2) face) val (nth (1+ (* i 2)) face))
          (pcase prop
            (:foreground (setq style (cons :color (cons val style))))
            (:weight (setq style (cons :bold (cons (eq val 'bold) style))))
            (:slant (setq style (cons :italic (cons (eq val 'italic) style))))))))

    (if (null style) string (list :children (list string) :style style))))


;;; Layout Utils

(defun s2--pixel-space (width &optional height)
  (let* ((height-spec (when height `(:height (,height))))
         (spec `(space :width (,width) . ,height-spec)))
    (if (eq width 0) "" (propertize " " 'display spec))))


(defun s2--layouts-to-strings (layouts path &optional root)
  (let ((func (if root #'s2--root-layout-to-string #'s2--sub-layout-to-string)))

    (--map (let ((prev (nth (1+ it) layouts))
                 (string (funcall func (nth it layouts) (cons it path))))
             (if (and root (plist-get prev :wrapBack))
                 (concat (substring string 0 -1) " ")
               string))

           (number-sequence 0 (1- (length layouts))))))

(defun s2--column-concat (str1 str2)
  (let* ((left-ls (split-string str1 "\n")) (right-ls (split-string str2 "\n"))
         (wids (mapcar #'shr-string-pixel-width left-ls))
         (max-wid (+ 5 (apply #'max (cons 0 wids))))
         ;; If the right side wraps, it should wrap indented
         (wrap-space (s2--pixel-space max-wid)))
    (mapconcat
     (lambda (idx)
       ;; If left taller than right, the last few lines are just left
       (if (>= idx (length right-ls)) (nth idx left-ls)
         ;; Concat left + padding + right with wrap to correct width
         (concat (or (nth idx left-ls) "")
                 (s2--pixel-space (- max-wid (or (nth idx wids) 0)))
                 (propertize (nth idx right-ls) 'wrap-prefix wrap-space))))
     (number-sequence 0 (1- (max (length left-ls) (length right-ls))))
     "\n")))

(defun s2--prepend-line-prefix (string prefix)
  (if (string= string "") string
    (let ((concat-fn `(lambda (existing) (concat ,prefix (or existing "")))))
      (alter-text-property 0 (length string) 'line-prefix concat-fn string)
      (alter-text-property 0 (length string) 'wrap-prefix concat-fn string)
      string)))


(defun s2--generate-keymap (bindings)
  (let ((new-map (make-sparse-keymap)))
    (dolist (bind bindings)
      (let ((symbol (make-symbol (plist-get bind :name))))
        (fset symbol `(lambda () (interactive) (s2--activate-keybind ',bind ',path)))
        (define-key new-map (kbd (plist-get bind :key)) symbol)))
    new-map))

(defun s2--append-keybinds (string bindings path)
  (let ((new-map (s2--generate-keymap bindings)))

    (alter-text-property
     0 (length string) 'keymap
     (lambda (existing)
       (if (null existing) new-map
         (list 'keymap existing new-map)))
     string)
    string))


(defun s2--add-path-text-prop (path string)
  (alter-text-property
   0 (length string) 's2-path
   (lambda (existing) (or existing path))
   string)

  (propertize string (intern (s-join "-" (mapcar #'number-to-string path))) t))



;;; Boxes

(setq s2-box-left-padding 15)

(bz/face s2-vertical-line :bg gray3)

(bz/face s2-box-top-line :overline gray3 :extend t :height 1)
(bz/face s2-box-bottom-line :overline gray3 :extend t :height 0.3)
;; :fg bg gets rid of annoying specs when in variable-pitch mode

(bz/face s2-indent-overline :overline gray3 :extend t)

(setq s2--outline-function #'s2--outline-function-box)
(setq s2--outline-function #'s2--outline-function-indent)

(defun s2--outline-string (str &optional indent-face)
  (let ((box (funcall s2--outline-function str indent-face)))
    ;; Add 1 to the depth
    (alter-text-property 0 (length box) 's2-depth (lambda (n) (1+ (or n 0))) box)
    box))

(defun s2--outline-function-box (str &optional indent-face)
  (let* ((vert (propertize (s2--pixel-space 1 1) 'face 's2-vertical-line))
         (indent (propertize (s2--pixel-space s2-box-left-padding 1) 'face indent-face))
         (content (s2--prepend-line-prefix (concat str) (concat vert indent)))
         ;; Rear nonsticky so that inserting text at the beginning of the box doesn't inherit
         (top (concat vert (propertize "\n" 'face `(s2-box-top-line ,indent-face))))
         (btm (propertize "\n" 'face 's2-box-bottom-line)))
    (concat (propertize top 's2-intangible t)
            content
            (propertize btm 's2-intangible t))))

(defun s2--outline-function-indent (str &optional indent-face)
  (let* ((vert (propertize (s2--pixel-space 1 1) 'face 's2-vertical-line))
         (indent (propertize (s2--pixel-space s2-box-left-padding 1) 'face indent-face)))
    (s2--prepend-line-prefix str (concat vert indent))))


;;; Layout to string

(bz/face s2-box-background-1 :bg bg2 :extend t)
(bz/face s2-box-background-2 :bg bg2 :extend t)

;; Root layout strings end in newlines
(defun s2--root-layout-to-string (layout path)
  (s2--add-path-text-prop
   path
   (pcase (plist-get layout :type)
     ("outline"
      (let* ((rows (plist-get layout :rows))
             (string (if (null rows) "\n" (s-join "" (s2--layouts-to-strings rows path 'root))))
             (box (if (null path) string (s2--outline-string string))))

        (when (plist-get layout :keybinds)
          (s2--append-keybinds box (plist-get layout :keybinds) path))

        box))

     ("box"
      (let* ((map (s2--generate-keymap (plist-get layout :keybinds)))
             (surround-face (list 's2-box-background-1))
             (content (s2--fmt-to-string (plist-get layout :text)))
             (newline (propertize "\n" 'face surround-face
                                  'intangible (> (length content) 0)))
             (unboxed (propertize (concat content newline) 's2-box path 's2-box-face surround-face))
             (boxed (if (plist-get layout :wrapBack) unboxed (s2--outline-string unboxed surround-face))))
        (propertize boxed 'keymap map)))

     (_ (concat (s2--sub-layout-to-string layout path)
                (propertize "\n" 'intangible t))))))

;; Sub layout strings DON'T end in newlines
(defun s2--sub-layout-to-string (layout path)
  (s2--add-path-text-prop
   path
   (pcase (plist-get layout :type)
     ("rows" (s-join "" (s2--layouts-to-strings (plist-get layout :rows) path)))

     ("columns"
      (let* ((cols (s2--layouts-to-strings (plist-get layout :columns) path)))
        (if (null cols) "" (-reduce #'s2--column-concat cols))))

     ("text"
      (let ((str (s2--fmt-to-string (plist-get layout :text))))
        (when (eq str "") (setq str "_"))
        ;; Use memq instead of plist-get because the menu could be nil
        (if (not (memq :menu layout)) str
          (propertize str 's2-button (list :path path :menu (plist-get layout :menu)))))))))


;;; Inserting layouts

(defvar s2-info nil)

(bz/face s2-title :w bold :h 1.4 :u t)
(bz/face s2-sid :w bold :h 1.0)
(bz/face s2-buffer)

(defun s2--insert-info (info)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)
        (str (s2--root-layout-to-string (plist-get info :layout) nil))
        (ol-count 0)
        beg end match ol)

    (setq-local s2-info info)

    (delete-region (point-min) (point-max))
    (remove-overlays)

    (insert (propertize (plist-get (plist-get info :state) :title) 'face 's2-title))
    (insert " ")
    (insert (propertize (format "- %s"(plist-get info :sid)) 'face 's2-sid))

    (insert (propertize "\n" 'intangible t 'line-height 1.5))

    (insert str)
    (put-text-property (point-min) (point-max) 'read-only t)

    ;; Insert overlays for box outlines
    (beginning-of-buffer)
    (while (setq match (text-property-search-forward 's2-intangible t t))
      (put-text-property (1- (prop-match-beginning match)) (prop-match-end match) 'intangible ol-count)
      (setq ol-count (1+ ol-count)))

    ;; Insert overlays around boxes
    (beginning-of-buffer)
    (setq ol-count 0)
    (while (setq match (text-property-search-forward 's2-box))
      ;; The property spans 1 beyond the end, so empty strings get the overlay too
      (setq beg (prop-match-beginning match) end (1- (prop-match-end match)))
      (put-text-property beg end 'read-only nil)

      ;; So you can insert at the beginning
      (put-text-property (1- beg) beg 'rear-nonsticky t)

      (setq ol (make-overlay beg end nil nil t))
      (overlay-put ol 'face (get-text-property beg 's2-box-face))
      (overlay-put ol 'line-prefix (get-text-property beg 'line-prefix))
      (overlay-put ol 'wrap-prefix (get-text-property beg 'wrap-prefix))
      (overlay-put ol 's2-path (prop-match-value match))
      (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
        (overlay-put ol prop '(s2--box-modification-hook)))

      ;; Odd face
      (when (= 1 (mod ol-count 2))
        (setcar (overlay-get ol 'face) 's2-box-background-2))

      (setq ol-count (1+ ol-count)))

    ;; Go to the focused path
    ;; (message "FOCSU+%s" (plist-get info :focus))

    (beginning-of-buffer)
    (let* ((inhibit-point-motion-hooks t)
           (focus (reverse (plist-get info :focus)))
           (path-sym (intern (s-join "-" (mapcar #'number-to-string focus))))
           (match (when focus (text-property-search-forward path-sym t #'equal))))
      (if (null match) (forward-line 1)
        (goto-char (prop-match-beginning match))))

    ;; Move off of intangible
    (when (get-text-property (point) 'intangible)
      (text-property-search-forward 'intangible))

    (setq-local buffer-undo-list nil)))

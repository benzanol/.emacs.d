;;; Layout Utils

(defun lens--pixel-space (width &optional height)
  (let* ((height-spec (when height `(:height (,height))))
         (spec `(space :width (,width) . ,height-spec)))
    (if (eq width 0) "" (propertize " " 'display spec))))


(defun lens--layouts-to-strings (layouts path &optional root)
  (let ((func (if root #'lens--root-layout-to-string #'lens--sub-layout-to-string)))

    (--map (let ((prev (nth (1+ it) layouts))
                 (string (funcall func (nth it layouts) (cons it path))))
             (if (and root (plist-get prev :wrapBack))
                 (concat (substring string 0 -1) " ")
               string))

           (number-sequence 0 (1- (length layouts))))))

(defun lens--column-concat (str1 str2)
  (let* ((left-ls (split-string str1 "\n")) (right-ls (split-string str2 "\n"))
         (wids (mapcar #'shr-string-pixel-width left-ls))
         (max-wid (+ 5 (apply #'max (cons 0 wids))))
         ;; If the right side wraps, it should wrap indented
         (wrap-space (lens--pixel-space max-wid)))
    (mapconcat
     (lambda (idx)
       ;; If left taller than right, the last few lines are just left
       (if (>= idx (length right-ls)) (nth idx left-ls)
         ;; Concat left + padding + right with wrap to correct width
         (concat (or (nth idx left-ls) "")
                 (lens--pixel-space (- max-wid (or (nth idx wids) 0)))
                 (propertize (nth idx right-ls) 'wrap-prefix wrap-space))))
     (number-sequence 0 (1- (max (length left-ls) (length right-ls))))
     "\n")))

(defun lens--prepend-line-prefix (string prefix)
  (if (string= string "") string
    (let ((concat-fn `(lambda (existing) (concat ,prefix (or existing "")))))
      (alter-text-property 0 (length string) 'line-prefix concat-fn string)
      (alter-text-property 0 (length string) 'wrap-prefix concat-fn string)
      string)))


(defun lens--generate-keymap (bindings)
  (let ((new-map (make-sparse-keymap)))
    (dolist (bind bindings)
      (let ((symbol (make-symbol (plist-get bind :name))))
        (fset symbol `(lambda () (interactive) (lens--activate-keybind ',bind ',path)))
        (define-key new-map (kbd (plist-get bind :key)) symbol)))
    new-map))

(defun lens--append-keybinds (string bindings path)
  (let ((new-map (lens--generate-keymap bindings)))

    (alter-text-property
     0 (length string) 'keymap
     (lambda (existing)
       (if (null existing) new-map
         (list 'keymap existing new-map)))
     string)
    string))


(defun lens--add-path-text-prop (path string)
  (alter-text-property
   0 (length string) 'lens-path
   (lambda (existing) (or existing path))
   string)

  (propertize string (intern (s-join "-" (mapcar #'number-to-string path))) t))



;;; Boxes

(setq lens-box-left-padding 15)

(bz/face lens-vertical-line :bg gray3)

(bz/face lens-box-top-line :overline gray3 :extend t :height 1)
(bz/face lens-box-bottom-line :overline gray3 :extend t :height 0.3)
;; :fg bg gets rid of annoying specs when in variable-pitch mode

(bz/face lens-indent-overline :overline gray3 :extend t)

(setq lens--outline-function #'lens--outline-function-box)
(setq lens--outline-function #'lens--outline-function-indent)

(defun lens--outline-string (str &optional indent-face)
  (let ((box (funcall lens--outline-function str indent-face)))
    ;; Add 1 to the depth
    (alter-text-property 0 (length box) 'lens-depth (lambda (n) (1+ (or n 0))) box)
    box))

(defun lens--outline-function-box (str &optional indent-face)
  (let* ((vert (propertize (lens--pixel-space 1 1) 'face 'lens-vertical-line))
         (indent (propertize (lens--pixel-space lens-box-left-padding 1) 'face indent-face))
         (content (lens--prepend-line-prefix (concat str) (concat vert indent)))
         ;; Rear nonsticky so that inserting text at the beginning of the box doesn't inherit
         (top (concat vert (propertize "\n" 'face `(lens-box-top-line ,indent-face))))
         (btm (propertize "\n" 'face 'lens-box-bottom-line)))
    (concat (propertize top 'lens-intangible t)
            content
            (propertize btm 'lens-intangible t))))

(defun lens--outline-function-indent (str &optional indent-face)
  (let* ((vert (propertize (lens--pixel-space 1 1) 'face 'lens-vertical-line))
         (indent (propertize (lens--pixel-space lens-box-left-padding 1) 'face indent-face)))
    (lens--prepend-line-prefix str (concat vert indent))))


;;; Layout to string

(bz/face lens-box-background-1 :bg bg2 :extend t)
(bz/face lens-box-background-2 :bg bg2 :extend t)

;; Root layout strings end in newlines
(defun lens--root-layout-to-string (layout path)
  (lens--add-path-text-prop
   path
   (pcase (plist-get layout :type)
     ("outline"
      (let* ((rows (plist-get layout :rows))
             (string (if (null rows) "\n" (s-join "" (lens--layouts-to-strings rows path 'root))))
             (box (if (null path) string (lens--outline-string string))))

        (when (plist-get layout :keybinds)
          (lens--append-keybinds box (plist-get layout :keybinds) path))

        box))

     ("box"
      (let* ((map (lens--generate-keymap (plist-get layout :keybinds)))
             (surround-face (list 'lens-box-background-1))
             (content (lens--fmt-to-string (plist-get layout :text)))
             (newline (propertize "\n" 'face surround-face
                                  'intangible (> (length content) 0)))
             (unboxed (propertize (concat content newline) 'lens-box path 'lens-box-face surround-face))
             (boxed (if (plist-get layout :wrapBack) unboxed (lens--outline-string unboxed surround-face))))
        (propertize boxed 'keymap map)))

     (_ (concat (lens--sub-layout-to-string layout path)
                (propertize "\n" 'intangible t))))))

;; Sub layout strings DON'T end in newlines
(defun lens--sub-layout-to-string (layout path)
  (lens--add-path-text-prop
   path
   (pcase (plist-get layout :type)
     ("rows" (s-join "" (lens--layouts-to-strings (plist-get layout :rows) path)))

     ("columns"
      (let* ((cols (lens--layouts-to-strings (plist-get layout :columns) path)))
        (if (null cols) "" (-reduce #'lens--column-concat cols))))

     ("text"
      (let ((str (lens--fmt-to-string (plist-get layout :text))))
        (when (eq str "") (setq str "_"))
        ;; Use memq instead of plist-get because the menu could be nil
        (if (not (memq :menu layout)) str
          (propertize str 'lens-button (list :path path :menu (plist-get layout :menu)))))))))


;;; Inserting layouts

(defvar lens-info nil)

(bz/face lens-title :w bold :h 1.4 :u t)
(bz/face lens-sid :w bold :h 1.0)
(bz/face lens-buffer)

(defun lens--insert-info (info)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)
        (str (lens--root-layout-to-string (plist-get info :layout) nil))
        (ol-count 0)
        beg end match ol)

    (setq-local lens-info info)

    (delete-region (point-min) (point-max))
    (remove-overlays)

    (insert (propertize (plist-get (plist-get info :state) :title) 'face 'lens-title))
    (insert " ")
    (insert (propertize (format "- %s"(plist-get info :sid)) 'face 'lens-sid))

    (insert (propertize "\n" 'intangible t 'line-height 1.5))

    (insert str)
    (put-text-property (point-min) (point-max) 'read-only t)

    ;; Insert overlays for box outlines
    (beginning-of-buffer)
    (while (setq match (text-property-search-forward 'lens-intangible t t))
      (put-text-property (1- (prop-match-beginning match)) (prop-match-end match) 'intangible ol-count)
      (setq ol-count (1+ ol-count)))

    ;; Insert overlays around boxes
    (beginning-of-buffer)
    (setq ol-count 0)
    (while (setq match (text-property-search-forward 'lens-box))
      ;; The property spans 1 beyond the end, so empty strings get the overlay too
      (setq beg (prop-match-beginning match) end (1- (prop-match-end match)))
      (put-text-property beg end 'read-only nil)

      ;; So you can insert at the beginning
      (put-text-property (1- beg) beg 'rear-nonsticky t)

      (setq ol (make-overlay beg end nil nil t))
      (overlay-put ol 'face (get-text-property beg 'lens-box-face))
      (overlay-put ol 'line-prefix (get-text-property beg 'line-prefix))
      (overlay-put ol 'wrap-prefix (get-text-property beg 'wrap-prefix))
      (overlay-put ol 'lens-path (prop-match-value match))
      (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
        (overlay-put ol prop '(lens--box-modification-hook)))

      ;; Odd face
      (when (= 1 (mod ol-count 2))
        (setcar (overlay-get ol 'face) 'lens-box-background-2))

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

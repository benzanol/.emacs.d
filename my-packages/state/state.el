(setq st-state-buffer "*state*")


(define-derived-mode st-mode nil "State"
  (use-local-map st-mode-map)
  )

(qv/keys st-mode-map
  :sparse t
  "<normal> <return>" st-click
  )


;;; Requests

;;;; Json parse
(defun st--json-parse (json)
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read-from-string json)))

;;;; Json encode
(defun st--json-encode-plist-entry (plist idx)
  (let ((key (substring (symbol-name (nth idx plist)) 1))
        (value (st--json-encode (nth (1+ idx) plist))))
    (format "\"%s\":%s" key value)))

(defun st--json-encode (obj)
  (if (or (null obj) (not (listp obj))) (json-encode obj)
    (if (and (eq (mod (length obj) 2) 0)
             (--every (keywordp (nth it obj)) (number-sequence 0 (1- (length obj)) 2)))
        ;; Encode as object
        (format "{%s}" (mapconcat (lambda (i) (st--json-encode-plist-entry obj i))
                                  (number-sequence 0 (1- (length obj)) 2) ","))
      ;; Encode as an array
      (format "[%s]" (mapconcat #'st--json-encode obj ",")))))


;;;; Process

(setq st--directory "~/Test/Node/androidjs/stdout/")

(setq st--process nil)

(defun st-start-process ()
  (interactive)

  (when (process-live-p st--process) (kill-process st--process))

  (message "Starting state process")

  (setq st--process
        (let ((default-directory st--directory))
          (make-process
           :name "state"
           :buffer "*state-process*"
           :command '("node" "main.js")))))


;;;; Sending requests

(defun st--request (type &rest body)
  (let* ((id (random (expt 2 31)))
         (request (list :id id :type type :body body))
         (default-directory st--directory)
         response-str response)

    ;; (unless (process-live-p st--process)
    ;;   (if (y-or-n-p "Process is not active. Start it now?")
    ;;       (st-start-process) (error "Process is not active")))

    (setq response-str
          (shell-command-to-string
           (format "echo %s > request_pipe && cat < response_pipe"
                   (shell-quote-argument (st--json-encode request)))))

    (setq response (st--json-parse response-str))

    (cond ((not (eq id (plist-get response :id)))
           (error "Invalid response! Expected id %s" id))
          ((eq json-false (plist-get response :success))
           (error "State backend error: %s" (plist-get response :error)))
          (t (plist-get response :body)))))


;;; Menu

(defun st--read-menu-question (question)
  (pcase (plist-get question :type)
    ("text" (list (read-string (concat (plist-get question :prompt) " ")
                               (plist-get question :initial))))
    ("tree" (-let* (((&plist :prompt prompt :choices choices) question)
                    (response (completing-read prompt (mapcar #'car choices)))
                    (followups (cdr (assoc response choices))))
              (cons response (mapcar #'st--read-menu-question followups))))))


;;; Fmt
;;;; Parse fmt to text

(defun st--parse-fmt (fmt)
  (if (or (stringp fmt) (keywordp (car fmt)))
      (st--parse-fmt-node fmt)
    (mapconcat #'st--parse-fmt-node fmt "")))

(defun st--parse-fmt-node (node)
  (if (stringp node)
      (let ((copy (substring node)))
        (add-face-text-property 0 (length copy) 'variable-pitch t copy)
        copy)

    (let ((str (mapconcat #'st--parse-fmt-node (plist-get node :children) ""))
          (face (st--create-fmt-face (plist-get node :style))))
      (add-face-text-property 0 (length str) face t str)
      str)))

(defun st--create-fmt-face (style)
  (-let (((&plist :bold bold :italic italic :color color :size size) style)
         (face (list 'face)))

    (when bold   (nconc face (list :weight (if (eq bold t) 'bold 'normal))))
    (when italic (nconc face (list :slant (if (eq italic t) 'italic 'normal))))
    (when color  (nconc face (list :foreground color)))
    (when size   (nconc face (list :height (* size 1.0))))

    (cdr face)))


;;;; Parse

;;; Commands
;;;; Github

(defun st-pull ()
  (interactive)
  (when (y-or-n-p "Are you sure? This will overwrite your local repo.")
    (message (st--request "git-pull"))))

(defun st-push ()
  (interactive)
  (message (st--request "git-push")))


;;;; Create

(defun st-new (template title)
  (interactive
   (let ((templates (st--request "list-templates")))
     (list (completing-read "Template: " templates)
           (read-string "Title: "))))

  (let ((display-info (st--request "create" :template template :title title)))
    (st--display display-info)))


;;;; Open

(defun st--select-state (tag)
  (let ((titles (--map (format "%s: %s" (plist-get it :sid) (plist-get it :title))
                       (st--request "list-states" :tag tag))))

    (string-to-number (car (split-string (completing-read "Title: " titles) ":")))))

(defun st-open (sid)
  (interactive (list (st--select-state "all")))
  (let ((display-info (st--request "get-display" :sid sid)))
    (st--display display-info)))

(defun st-open-tag (tag)
  (interactive (list (st--request "list-tags")))
  (st-open (st--select-state tag)))


;;; Display

;;;; Display info

(defvar-local st-modifications nil)

(defun st--display (info)
  (switch-to-buffer st-state-buffer)
  (unless (eq major-mode 'st-mode) (st-mode))
  (setq-local st-sid (plist-get info :sid))
  (setq-local st-modifications nil)

  (let ((str (st--parse-layout-to-string (plist-get info :layout)))
        (inhibit-modification-hooks t)
        (inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (remove-overlays)

    (insert str)
    (st--insert-text-overlays)
    ))


;;;; Parse layout to string

(defun st--parse-layout-to-strings (layout)
  (-let* ((parsed (st--parse-layout layout nil))
          ((&plist :prefix prefix :content content :height height) parsed))

    (--map (let* ((text (or (nth it content) "")) (pre (nth it prefix))
                  (line (if pre (propertize text 'line-prefix pre) text))
                  (pre-wid (if pre (st--string-width pre) 0))
                  prop-match before-str line-prefix)
             line
             ;; (with-temp-buffer
             ;;   (insert line) (goto-char (point-min))
             ;;   (while (setq prop-match (text-property-search-forward 'st-path))
             ;;     (setq before-str (buffer-substring (point-min) (prop-match-beginning prop-match)))
             ;;     (setq before-str (propertize before-str 'line-prefix nil))
             ;;     (put-text-property (prop-match-beginning prop-match) (prop-match-end prop-match) 'wrap-prefix
             ;;                        (st--pixel-space (+ pre-wid (st--string-width before-str)))))
             ;;   (buffer-string))
             )
           (number-sequence 0 (1- height)))))

;;;; Parse layout to prefix and content

;; :height - height in lines
;; :prefix? - (string)
;; :content? - (string)

;; The path is in reverse order for lisp reasons
(defun st--parse-layout (layout path)
  (pcase (plist-get layout :type)
    ("grabber" (list :height 0))
    ("text" (st--create-text-layout
             (let ((str (st--parse-fmt (plist-get layout :text))))
               (if (eq (length str) 0) " " str))
             (member (plist-get layout :edit) '("text" "style"))
             nil ;; (when (eq t (plist-get layout :outline)) (list :background (qv/color bg3) :extend t))
             'st-path path
             'st-type (intern (or (plist-get layout :edit) "readonly"))))
    ("button" (st--create-text-layout
               (st--parse-fmt (plist-get layout :label))
               'tangible
               (if (string= (plist-get layout :style) "icon") 'shadow '(:box t))
               'st-path path 'st-type 'button 'st-menu (plist-get layout :menu)))

    ("outline" (let* ((inner (st--parse-layout (plist-get layout :inner) (cons 0 path)))
                      (lines (plist-get inner :prefix))
                      (vert-line (propertize (st--pixel-space 1) 'face '(:background "gray50")))
                      (new-prefix (--map (concat vert-line "    " (or (nth it lines) ""))
                                         (number-sequence 0 (1- (plist-get inner :height))))))
                 (plist-put inner :prefix new-prefix)))

    ("rows"
     (let* ((layouts (plist-get layout :rows))
            (height 0) prefix content row row-prefix row-content)

       (dotimes (row-num (length layouts))
         (setq row (st--parse-layout (nth row-num layouts) (cons row-num path))
               row-prefix (plist-get row :prefix) row-content (plist-get row :content))
         (dotimes (i (plist-get row :height))
           (push (or (nth i row-prefix) "") prefix)
           (push (or (nth i row-content) "") content)
           (setq height (1+ height))))
       ;; Remove trailing empty strings from content
       (while (string= (car content) "") (pop content))

       (list :height height :prefix (reverse prefix) :content (reverse content))))

    ("columns"
     (let* ((layouts (plist-get layout :columns))
            (cols (--map (st--parse-layout (nth it layouts) (cons it path))
                         (number-sequence 0 (1- (length layouts)))))
            (height (apply #'max 0 (--map (plist-get it :height) cols)))
            (prefix (make-list height "")) (content (make-list height ""))
            ;; Lines of non-prefix text: either 0, 1, or >1
            (non-prefix-lines 0)
            (max-pixels 0)
            col-prefix col-content)

       (dolist (col cols)
         (setq col-prefix (plist-get col :prefix) col-content (plist-get col :content))

         ;; The maximum line width
         (setq max-pixels
               (apply #'max (--map (st--string-width (apply #'concat it))
                                   (-zip-lists prefix content))))

         ;; Align each line to the max width
         (dotimes (i height)
           (let* ((p (nth i prefix)) (c (nth i content))
                  (pad (+ 5 (- max-pixels (st--string-width (concat p c))))))
             (if (string= c "")
                 (setf (nth i prefix) (concat p (st--pixel-space pad)))
               (setf (nth i content) (concat c (st--pixel-space pad))))))

         (pcase non-prefix-lines
           (0 (setq prefix (st--merge-line-lists prefix col-prefix))
              (setq content (st--merge-line-lists content col-content prefix)))

           (1 (setq prefix (st--merge-line-lists prefix (cons "" (cdr col-prefix))))
              (setq content (st--merge-line-lists
                             content (cons (concat (or (car col-prefix) "")
                                                   (or (car col-content) ""))
                                           (cdr col-content))
                             prefix)))
           (_ (setq content (st--merge-line-lists
                             content (--map (concat (or (nth it col-prefix) "")
                                                    (or (nth it col-content) ""))
                                            (number-sequence 0 height))
                             prefix))))

         (setq non-prefix-lines (max non-prefix-lines (length col-content))))

       (list :height height :prefix prefix :content content)))))


(defun st--create-text-layout (text tangible &optional face &rest props)
  (with-current-buffer st-state-buffer
    (let* ((str (st--parse-fmt text))
           (formatted
            (progn (when face (add-face-text-property 0 (length str) face nil str))
                   (apply #'propertize str props)))
           (lines (split-string formatted "\n"))
           (h (length lines)))
      (list (if tangible :content :prefix) lines :height h))))


;;;; Text utils


(defun st--pixel-space (width)
  (if (eq width 0) "" (propertize " " 'display `(space :width (,width))))
  ;; (if (eq width 0) "" (propertize " " 'display `(space :width ,width)))
  )

(defun st--string-width (str)
  (shr-string-pixel-width str)
  ;; (string-width str)
  )


(defun st--pad-lines (lines &optional width)
  (unless width (setq width (apply #'max (mapcar #'st--string-width lines))))
  (--map (st--pixel-space (- width (st--string-width))) lines))

(defun st--merge-line-lists (left-ls right-ls &optional prefixes)
  (let ((height (max (length left-ls) (length right-ls))))
    (--map (let ((left-str (or (nth it left-ls) "")))
             (concat left-str (or (nth it right-ls) "")))
           (number-sequence 0 (1- height)))))

(defun st--prepend-wrap-prefix (str))


;;;; Inserting overlays

(defun st--all-prop-matches (prop value)
  "Return a list of matches where the value of prop is #'equal to
value in order of appearance."

  (let (match matches)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward prop value #'equal))
        (push match matches))
      (reverse matches))))

(defun st--insert-text-overlays ()
  (let ((pred (lambda (_ v) (memq v '(text style))))
        already-inserted match path type)

    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'st-type nil pred))
        (setq path (get-text-property (prop-match-beginning match) 'st-path))
        (setq type (get-text-property (prop-match-beginning match) 'st-type))

        (when (and path (not (member path already-inserted)))
          (push path already-inserted)
          (st--insert-text-overlay match path type))))))

(defun st--insert-text-overlay (prop-match path type)
  (when-let* ((matches (st--all-prop-matches 'st-path path))
              (all-connected
               (--every (eq (1+ (prop-match-end it))
                            (prop-match-beginning (nth (1+ it-index) matches)))
                        (butlast matches)))
              (ol (make-overlay (prop-match-beginning (car matches))
                                (prop-match-end (car (last matches)))
                                nil nil t)))

    (overlay-put ol 'st-path path)
    (overlay-put ol 'st-type type)
    (overlay-put ol 'face (list :background (qv/color bg3) :extend t
                                ;; :underline (:color "blue" :style wave)
                                ))

    (overlay-put ol 'modification-hooks '(st--onedit-modification-hook))
    (overlay-put ol 'insert-behind-hooks '(st--onedit-modification-hook))
    (overlay-put ol 'insert-in-front-hooks '(st--onedit-modification-hook))))


;;;; Editting

(defun st--onedit-modification-hook (ol after &rest args)
  (when after (add-to-list 'st-modifications ol 'append)))

(defun st-click ()
  (interactive)

  (if (not (eq (get-text-property (point) 'st-type) 'button))
      (st--apply-edits-and-refresh)

    (let* ((path (get-text-property (point) 'st-path))
           (menu (get-text-property (point) 'st-menu))
           (answers (mapcar #'st--read-menu-question menu))
           (action (list :type "button" :answers answers))
           (request (list :sid st-sid :path (reverse path) :action action)))

      (add-to-list 'st-modifications request 'append)
      (st--apply-edits-and-refresh))))


(defun st--apply-edits-and-refresh ()
  (let ((inhibit-read-only t) (inhibit-modification-hooks t)
        (line (line-number-at-pos)) (col (current-column))
        requests display-info t1 t2 t3 t4)

    ;; Generate a list of requests
    (dolist (ol st-modifications)
      (if (not (overlayp ol)) (push ol requests)
        (let* ((path (overlay-get ol 'st-path))
               (str (buffer-substring (overlay-start ol) (overlay-end ol)))
               (act (pcase (overlay-get ol 'st-type)
                      ('text (list :type "text" :text str))
                      ('style (list :type "text" :text str))
                      (_ (error "st-type must be either text or style")))))
          (push (list :sid st-sid :path (reverse path) :action act) requests))))
    (setq st-modifications nil)

    (delete-region (point-min) (point-max))

    (dolist (request requests)
      (setq display-info (apply #'st--request "edit" request)))

    (st--display display-info)

    ;; Return to the original position
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))

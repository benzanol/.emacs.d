(require 'color)

;;; Mode

(bz/keys s2-mode-map
  :sparse t
  [remap bz/click] s2-click
  [remap bz/toggle-fold] s2-toggle-fold
  "<insert> <C-return>" ((qvk-normal-keymode) (s2-click))

  "C-c C-r" s2-reload
  "C-c C-o" s2-restart-and-reopen
  )

(define-derived-mode s2-mode nil "State2"
  "State minor mode 2.0"
  (use-local-map s2-mode-map)
  (display-line-numbers-mode 0))


;;; Json

(defun s2--json-parse (json)
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read-from-string json)))

(defun s2--json-encode-plist-entry (plist idx)
  (let ((key (substring (symbol-name (nth idx plist)) 1))
        (value (s2--json-encode (nth (1+ idx) plist))))
    (format "\"%s\":%s" key value)))

(defun s2--json-encode (obj)
  (if (or (null obj) (not (listp obj))) (json-encode obj)
    (if (and (eq (mod (length obj) 2) 0)
             (--every (keywordp (nth it obj)) (number-sequence 0 (1- (length obj)) 2)))
        ;; Encode as object
        (format "{%s}" (mapconcat (lambda (i) (s2--json-encode-plist-entry obj i))
                                  (number-sequence 0 (1- (length obj)) 2) ","))
      ;; Encode as an array
      (format "[%s]" (mapconcat #'s2--json-encode obj ",")))))


;;; Requests

(defvar s2--directory "~/Documents/Programming/Node/androidjs/stdout/")
(defvar s2--process nil)

(defun s2-start-process ()
  (interactive)

  (when (process-live-p s2--process) (kill-process s2--process))

  (message "Starting state process")

  (setq s2--process
        (let ((default-directory s2--directory))
          (make-process
           :name "state"
           :buffer "*state-process*"
           :command '("node" "main.js")))))

(defun s2--request (type &rest body)
  (let* ((id (random (expt 2 31)))
         (request (list :id id :type type :body body))
         (default-directory s2--directory)
         response-str response)

    (unless (process-live-p s2--process)
      (if (y-or-n-p "Process is not active. Start it now?")
          (s2-start-process) (error "Process is not active")))


    ;; Synchronously wait for the response, freezing emacs in the process
    (setq response-str
          (shell-command-to-string
           (format "echo %s > request_pipe && cat < response_pipe"
                   (shell-quote-argument (s2--json-encode request)))))

    (setq response (s2--json-parse response-str))

    (cond ((not (eq id (plist-get response :id)))
           (error "Invalid response! Expected id %s" id))
          ((eq json-false (plist-get response :success))
           (error "State backend error: %s" (plist-get response :error)))
          (t (plist-get response :body)))))


;;; Editing

(defun s2--read-menu-question (question)
  (pcase (plist-get question :type)
    ("text" (list (read-string (concat (plist-get question :prompt) " ")
                               (plist-get question :initial))))
    ("tree" (-let* (((&plist :prompt prompt :choices choices) question)
                    (response (completing-read (concat prompt " ") (mapcar #'car choices)))
                    (followups (cdr (assoc response choices))))
              (cons response (mapcar #'s2--read-menu-question followups))))))

(defun s2-click ()
  (interactive)
  ;; If there is an edit, submit it
  (if s2-pending-box-overlay (s2--apply-pending-edit)

    (let ((button (get-text-property (point) 's2-button)))

      ;; Try the previous character (on the same line)
      (when (and (not button) (not (bolp)))
        (setq button (get-text-property (1- (point)) 's2-button)))

      ;; Try searching the current line
      (unless button
        (let ((prop-match (save-excursion (text-property-search-forward 's2-button))))
          (when (and prop-match (<= (prop-match-beginning prop-match) (point-at-eol)))
            (setq button (prop-match-value prop-match)))))

      (unless button (error "No button at point"))

      (let* ((path (plist-get button :path))
             (menu (plist-get button :menu))
             (answers (mapcar #'s2--read-menu-question menu))
             (action (list :type "button" :answers (or answers [])))
             (updated (s2--request 'edit :sid (plist-get s2-info :sid)
                                   :path (or (reverse path) [])
                                   :action action)))
        (s2-reload updated)))))

(defun s2--activate-keybind (bind path)
  (when s2-pending-box-overlay (s2--apply-pending-edit))

  (let* ((menu (plist-get bind :menu))
         (answers (mapcar #'s2--read-menu-question menu))
         (action (list :type "key" :name (plist-get bind :name) :answers (or answers [])))
         (updated (s2--request 'edit :sid (plist-get s2-info :sid)
                               :path (or (reverse path) [])
                               :action action)))
    (s2-reload updated)))


(defvar s2-pending-box-overlay nil
  "The overlay for the box which has pending modifications")

(bz/face s2-pending-box-background :bg ,(color-lighten-name (bz/color bg) 6) :extend t)

(defun s2--box-modification-hook (ol after &rest args)
  (when after
    ;; If a different box is pending, apply the edit
    (when (and s2-pending-box-overlay (not (eq ol s2-pending-box-overlay)))
      (s2--apply-pending-edit))

    (setcar (overlay-get ol 'face) 's2-pending-box-background)

    (setq s2-pending-box-overlay ol)
    (add-hook 'post-command-hook #'s2--apply-pending-edit-post-command)))

;; Designed to be run from post-command-hook
(defun s2--apply-pending-edit-post-command ()
  (let ((ol s2-pending-box-overlay))
    (if (not (and ol (overlay-buffer ol))) (setq ol nil)
      (unless (or (minibufferp)
                  (and (eq (overlay-buffer ol) (current-buffer))
                       (>= (point) (overlay-start ol))
                       (<= (point) (overlay-end ol))))
        (s2--apply-pending-edit)
        (remove-hook 'post-command-hook #'s2--apply-pending-edit-post-command)))))

(defun s2--apply-pending-edit ()
  (let ((pre-path (get-text-property (point) 's2-path))
        (ol s2-pending-box-overlay))
    (when (and ol (overlay-buffer ol))

      (with-current-buffer (overlay-buffer ol)
        (let* ((path (overlay-get ol 's2-path))
               (str (buffer-substring (overlay-start ol) (overlay-end ol)))
               (fmt (s2--string-to-fmt str))
               (action (list :type "box" :text fmt))
               (updated (s2--request 'edit :sid (plist-get s2-info :sid)
                                     :path (or (reverse path) [])
                                     :action action)))

          ;; Keep track of where the point moved
          (when pre-path (plist-put updated :focus (reverse pre-path)))

          (s2-reload updated)))

      (setq s2-pending-box-overlay nil))))


;;; Commands

(defvar s2-state-buffer "*state*")
(defun s2--open-info (info)
  (switch-to-buffer s2-state-buffer)

  (unless (eq major-mode 's2-mode) (s2-mode))

  ;; Allows intangible to work
  (setq-local inhibit-point-motion-hooks nil)

  (setq-local buffer-face-mode-face 's2-buffer)
  (buffer-face-mode)

  (setq-local line-spacing 0.1)

  (s2--insert-info info))

(defun s2-reload (&optional info)
  (interactive)

  ;; Fetch the new info if it isn't provided
  (s2--insert-info (or info s2-info (error "No info!"))))

(defun s2-restart-and-reopen ()
  (interactive)
  (s2-start-process)
  (s2-open (plist-get s2-info :sid)))


(defun s2-new (template title)
  (interactive
   (let ((templates (s2--request "list-templates")))
     (list (completing-read "Template: " templates)
           (read-string "Title: "))))

  (let ((display-info (s2--request "create" :template template :title title)))
    (s2--open-info display-info)))


(defun s2--read-state (tag)
  "Read an SID"
  (let ((titles (--map (format "%s: %s" (plist-get it :sid) (plist-get it :title))
                       (s2--request "list-states" :tag tag))))

    (string-to-number (car (split-string (completing-read "Title: " titles) ":")))))

(defun s2-open (sid)
  (interactive (list (s2--read-state "all")))
  (let ((display-info (s2--request "get-display" :sid sid)))
    (s2--open-info display-info)))

(defun s2-open-tag (tag)
  (interactive (list (completing-read "Tag: " (s2--request "list-tags") nil t)))
  (s2-open (s2--read-state tag)))

(defun s2-delete (sid)
  (interactive (list (s2--read-state "all")))
  (when (numberp sid) (s2--request "delete" :sid sid)))

(defun s2-undo (sid)
  (interactive (list (or (plist-get s2-info :sid) (error "Not in a state!"))))
  (let ((new-info (s2--request "undo" :sid sid)))
    (if new-info
        (s2--insert-info new-info)
      (message "End of history!"))))


(defun s2-pull ()
  (interactive)
  (when (y-or-n-p "Are you sure? This will overwrite your local repo.")
    (message (s2--request "git-pull"))))

(defun s2-push ()
  (interactive)
  (message (s2--request "git-push")))

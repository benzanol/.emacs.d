
;; The horizontal rules are marked with state-beg/state-end = (LEVEL SID)

(defun lens-save ()
  (interactive)
  (let ((pos (point))
        (max-indent 0)
        (pred (lambda (lvl spec) (eq lvl (car spec))))
        match beg end md spec)

    (beginning-of-buffer)
    (remove-overlays)

    ;; Determine the max indent
    (while (setq match (text-property-search-forward 'state-beg))
      (setq max-indent (max max-indent (car (prop-match-value match)))))

    ;; Loop from the largest indent to the smallest indent
    (dolist (lvl (number-sequence max-indent 1 -1))
      (beginning-of-buffer)

      (while (setq beg (text-property-search-forward 'state-beg lvl pred))
        ;; Beg and End are prop matches of the beginning and ending horizontal rules
        (setq spec (prop-match-value beg)
              end (text-property-search-forward 'state-end spec #'eq)
              md (buffer-substring-no-properties (prop-match-end beg) (prop-match-beginning end)))

        ;; Put the content into the saved cache
        (setf (plist-get (gethash (cadr spec) lens--states) :value)
              (with-temp-buffer (insert md) (lens--parse-markdown)))

        ;; Replace it with the sid indicator
        (let ((inhibit-read-only t))
          (delete-region (prop-match-beginning beg) (prop-match-end end))
          (insert (format "<%s>\n" (cadr spec))))))

    ;; Save the top level markdown
    (beginning-of-buffer)
    (setf (plist-get (gethash lens-sid lens--states) :value) (lens--parse-markdown))

    ;; Insert back the buffer contents
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (lens--insert-nested-markdown lens-sid)
    (beginning-of-buffer) (forward-char (1- pos))))


(bz/face lens-indent-line :h 0.1 :bg black)
(bz/face lens-footer :o t :x t :h 0.5)
(bz/face lens-header :o t :x t :bg "#EEEEF0")
(defun lens--indent-string (level &optional overline)
  (let ((str (s-join "" (make-list level #("   "
                                           0 1 (display (space :height (1) :width 1))
                                           1 2 (face lens-indent-line)
                                           2 3 (display (space :height (1) :width 2)))))))
    (if (not overline) str
      (add-face-text-property 1 (length str) 'lens-header 'append str)
      (propertize str 'intangible t))))

(defun lens--insert-nested-markdown (top-sid)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (remove-overlays))

  (setq-local lens-sid top-sid)
  (insert (lens--generate-markdown (plist-get (lens--get-state top-sid) :value)))

  (beginning-of-buffer)
  (while (search-forward-regexp "^<\\([0-9]+\\)>$" nil t)
    (let* ((sid (string-to-number (match-string 1)))
           (indent (1+ (or (get-text-property (point) 'indent) 0)))
           (indent-str (lens--indent-string indent))
           (state-spec (list indent sid 'loop)))

      ;; Make sure there aren't nested sids
      (unless (or (eq sid top-sid) (--find (eq sid (overlay-get it 'sid)) (overlays-at (point))))
        (beginning-of-line)

        ;; Insert the actual content, with horizontal rules above and below
        (save-excursion
          (delete-region (point) (1+ (point-at-eol)))
          (insert (concat (propertize (concat (lens--indent-string 1 t)
                                              (propertize (format "[%s]\n" sid) 'face 'lens-header))
                                      'state-beg state-spec
                                      'read-only t 'rear-nonsticky t 'front-sticky t)
                          (propertize (lens--generate-markdown (plist-get (lens--get-state sid) :value))
                                      'indent indent)
                          (propertize "\n" 'state-end state-spec 'face 'lens-footer
                                      'read-only t 'front-sticky t 'rear-nonsticky t)))))))

  ;; Insert indent overlays
  (let (beg end spec ol indent-str)
    (beginning-of-buffer)

    (while (setq beg (text-property-search-forward 'state-beg))
      (setq spec (prop-match-value beg)
            end (save-excursion (text-property-search-forward 'state-end spec #'eq))
            ol (make-overlay (prop-match-end beg) (prop-match-beginning end) nil nil 'grow)
            indent-str (lens--indent-string (car spec)))

      (overlay-put ol 'priority (car spec))
      (overlay-put ol 'line-prefix indent-str)
      (overlay-put ol 'wrap-prefix indent-str))))


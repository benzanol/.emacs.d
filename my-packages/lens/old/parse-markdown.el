(defun lens--parse-markdown ()
  (save-excursion
    (let ((point 0) props body starts)
      (beginning-of-buffer)
      ;; Read the props
      (when (looking-at "---$")
        (while (progn (forward-line) (not (looking-at "---$")))
          (search-forward ": " (point-at-eol))
          (let* ((key (intern (buffer-substring (point-at-bol) (- (point) 2))))
                 (value (buffer-substring-no-properties (point) (point-at-eol))))
            (push (cons key value) props)))
        (forward-line)
        ;; Go to the next nonempty line
        (while (eolp) (forward-line)))

      ;; Parse from the end of the props as a section of level 0
      (cons props (lens--parse-markdown-section 0)))))

(defun lens--parse-markdown-section (level)
  ;; Skip blank lines
  (while (and (eolp) (not (eobp))) (forward-line))

  (let (heading items head)
    ;; Loop until at the end of the buffer OR there is a heading greater than LEVEL
    (while (progn (setq heading (and (looking-at "#+") (- (match-end 0) (point))))
                  (and (not (eobp)) (or (null heading) (> heading level))))
      (cond
       ;; Looking at a link
       ((and (null heading) (looking-at "<\\([0-9]+\\)>$"))
        (push (list 'state (string-to-number (match-string 1))) items)
        (forward-line))

       ;; Add to the content lines
       ((null heading)
        (push (buffer-substring-no-properties (point) (point-at-eol)) items)
        (forward-line))

       ((> heading (1+ level)) (error "Skipped heading level"))
       ;; Parse the nested heading
       (t (push (cons (buffer-substring-no-properties (+ (point) heading 1) (point-at-eol))
                      (progn (forward-line) (lens--parse-markdown-section (1+ level))))
                items))))

    ;; Combine consecutive lines and then trim them
    (setq items (reverse items))
    (setq head items)
    (while head
      (when (stringp (car head))
        (while (stringp (cadr head))
          (setcar head (concat (car head) "\n" (cadr head)))
          (setcdr head (cddr head)))
        (setcar head (string-trim (car head))))
      (pop head))

    items))


(defun lens--generate-markdown (struct)
  (concat (if (null (car struct)) ""
            (->> (car struct)
                 (--map (format "%s: %s" (car it) (cdr it)))
                 (-sort #'string<)
                 (s-join "\n")
                 (format "---\n%s\n---\n\n")))
          (string-trim
           (lens--generate-markdown-section (cdr struct) 1)
           "\n" "\n")
          "\n"))

(defun lens--generate-markdown-section (section level)
  (let ((content "") secs)
    ;; Add all links and strings as lines to the content
    (while (pcase (car section)
             ((pred stringp) (setq content (concat content "\n" (car section) "\n\n")))
             (`(state ,sid) (setq content (format "%s<%s>\n\n" content sid))))
      (pop section))

    (setq secs (--map (format "%s %s\n\n%s\n\n" (make-string level ?#) (car it)
                              (string-trim (lens--generate-markdown-section (cdr it) (1+ level))))
                      section))
    (concat content (s-join "" secs))))


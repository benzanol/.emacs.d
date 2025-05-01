(defun lens--parse-org ()
  (save-excursion
    (let (props)
      (beginning-of-buffer)

      ;; Read the props
      (while (looking-at "#\\+\\([a-zA-Z0-9]+\\): \\(.*\\)$")
        (push (cons (intern (match-string 1))
                    (org-no-properties (match-string 2)))
              props)
        (forward-line))

      ;; Parse from the end of the props as a section of level 0
      (cons props (lens--parse-org-section 0)))))

(defun lens--parse-org-section (level)
  ;; Skip blank lines
  (while (and (eolp) (not (eobp))) (forward-line))

  (let ((case-fold-search t)
        heading rev-lines rev-headings)
    ;; Loop until at the end of the buffer OR there is a heading greater than LEVEL
    (while (progn (setq heading (when (looking-at "\\*+") (length (match-string 0))))
                  (and (not (eobp)) (or (null heading) (> heading level))))
      (cond
       ;; TODO: Parse nested lists as a structure

       ;; If looking at a link (with description)
       ((and (null heading) (looking-at "\\[\\[\\([^][\n]+\\)\\]\\[\\([^][\n]+\\)\\]\\]"))
        (push (list 'link (org-no-properties (match-string 1)) (org-no-properties (match-string 2)))
              rev-lines)
        (forward-line))

       ;; If looking at a link (without description)
       ((and (null heading) (looking-at "\\[\\[\\([^][\n]+\\)\\]\\]"))
        (push (list 'link (org-no-properties (match-string 1))) rev-lines)
        (forward-line))

       ;; If looking at a code block
       ((and (null heading) (looking-at "#\\+begin_src \\([^ \n]+\\)\\(?: \\(.*\\)\\)?"))
        (let ((lang (match-string 1)) (props (match-string 2)) (eol (point-at-eol)))
          (search-forward-regexp "^#\\+end_src$")
          (push `(block
                  ,(org-no-properties lang)
                  ,(buffer-substring-no-properties (1+ eol) (1- (point-at-bol)))
                  ;; Only include a fourth list element if props is non-nil
                  ,@(when props (list (org-no-properties props))))
                rev-lines)
          (forward-line 1)))

       ;; Add to the content lines
       ((null heading)
        (push (buffer-substring-no-properties (point) (point-at-eol)) rev-lines)
        (forward-line))

       ((> heading (1+ level)) (error "Skipped heading level"))
       ;; Heading = 1+level, so parse the nested heading
       (t (push (cons (buffer-substring-no-properties (+ (point) heading 1) (point-at-eol))
                      (progn (forward-line) (lens--parse-org-section (1+ level))))
                rev-headings))))

    ;; Trim and reverse the rev-lines
    (while (equal (car rev-lines) "") (pop rev-lines))
    (setq rev-lines (reverse rev-lines))
    (while (equal (car rev-lines) "") (pop rev-lines))

    (cons rev-lines (reverse rev-headings))))


(defun lens--generate-org (struct)
  (let ((body (string-trim (lens--generate-org-section (cdr struct) 1) "\n" "\n")))
    ;; If there are no properties, just have the body
    (if (null (car struct)) (concat body "\n")

      ;; If there are properties, put newlines between them and the body
      (concat (if (null (car struct)) ""
                (->> (car struct)
                     (--map (format "#+%s: %s" (car it) (cdr it)))
                     (-sort #'string<)
                     (s-join "\n")))
              "\n\n" body "\n"))))

(defun lens--generate-org-section (section level)
  (let* ((lines (--map (pcase it
                         (`(link ,target ) (format "[[%s]]" target))
                         (`(link ,target ,desc) (format "[[%s][%s]]" target desc))
                         ((or `(block ,lang ,code) `(block ,lang ,code ,props))
                          (format "#+begin_src %s%s\n%s\n#+end_src"
                                  lang (if props (concat " " props) "") code))
                         (str str))
                       (car section)))
         (content (s-join "\n" lines)))

    (setq secs (--map (format "%s %s\n%s" (make-string level ?*) (car it)
                              (lens--generate-org-section (cdr it) (1+ level)))
                      (cdr section)))

    (cond ((not (string= content "")) (apply #'concat content "\n\n" secs))
          (secs (apply #'concat secs))
          (t "\n"))))

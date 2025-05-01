;; Jason:
;; (number . NUMBER)
;; (string . STRING)
;; (list ELEMS...)
;; (object (KEY . VAL) ...)
;; (table (ROWS...) (COLS...) ((CELLS...)...))


(setq jason-indent 4)


;; Turn all numbers into (number . NUMBER) and strings into (string . STRING)
(defun jasonify (object)
  (--tree-map
   (cond ((stringp it) (cons 'string it))
         ((numberp it) (cons 'number it))
         (t it))
   object))

;;; Inserting

(defun jason-insert (jason)
  (delete-region (point-min) (point-max))
  (dolist (line (jason-to-lines jason))
    (insert line)
    (newline)))


(defun jason-to-lines (jason)
  (pcase (car jason)
    ('string (jason-propertize (split-string (cdr jason) "\n") jason))
    ('number (jason-propertize (list (number-to-string (cdr jason))) jason))
    ('list
     (--> (cdr jason)
          (mapcar 'jason-to-lines it) ; Create a list of lists of lines
          (jason-flatten it ",") ; A list of lines separated by ","
          (--map (concat (make-string jason-indent ?\s) it) it) ; Indent each line
          (append (list "[") it (list "]"))
          (jason-propertize it jason)))
    ('object
     (--> (cdr jason)
          (mapcar (lambda (pair)
                    (let ((lines (jason-to-lines (cdr pair)))
                          (prefix (propertize (format "%s: " (car pair)) 'face 'bold)))
                      (jason-propertize
                       (cons (concat prefix (car lines))
                             (--map (concat (make-string jason-indent ?\s) it) (cdr lines)))
                       (cdr pair))))
                  it)
          (jason-flatten it ",") ; A list of lines separated by ","
          (--map (concat (make-string jason-indent ?\s) it) it) ; Indent each line
          (append (list "{") it (list "}"))
          (jason-propertize it jason)))))


;;; Highlighting

(qv/face jason-level-1 :bg bg :x t)
(qv/face jason-level-2 :bg bg3 :x t)
(qv/face jason-level-3 :bg gray3 :x t)
(qv/face jason-level-4 :bg gray2 :x t)
(qv/face jason-level-5 :bg gray1 :x t)

(setq jason-level-faces '(jason-level-1 jason-level-2 jason-level-3 jason-level-4 jason-level-5))

(defun jason-highlight ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'jason-highlight t)
  (let ((jasons (reverse (get-text-property (point) 'jason)))
        prop-match props idx o)
    (save-excursion
      (goto-char (point-min))
      (while (setq prop-match (text-property-search-forward 'jason))
        (setq props (prop-match-value prop-match))
        (setq idx 0)
        (dolist (j jasons)
          (when (memq j props)
            (setq --dolist-tail-- nil)
            (setq idx (mod (- (length jasons) 1 (-elem-index j jasons))
                           (length jason-level-faces)))
            (setq o (make-overlay (prop-match-beginning prop-match)
                                  (prop-match-end prop-match)))
            (overlay-put o 'face (nth idx jason-level-faces))
            (overlay-put o 'jason-highlight t)))))))


;;; Utils

(defun jason-propertize (strings jason)
  ;; Clone the strings
  (setq strings (mapcar 'substring strings))

  (dolist (s strings)
    (let ((start 0)
          (prop (get-text-property 0 'jason s)))
      (dotimes (i (length s))
        (when (not (eq prop (get-text-property i 'jason s)))
          (unless (memq jason prop)
            (put-text-property start i 'jason (cons jason prop) s))
          (setq start i)
          (setq prop (get-text-property i 'jason s))))
      (unless (memq jason prop)
        (put-text-property start (length s) 'jason (cons jason prop) s))))
  strings)

(defun jason-flatten (lists separator)
  (->> lists
       (--map (cons separator it)) ; Add the separator to the beginning of each sub-list
       (-flatten-n 1) ; Flatten the lists into a single list
       cdr)) ; Remove the first separator

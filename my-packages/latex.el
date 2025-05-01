(qv/keys ltx-map
  :sparse t
  "/" ltx-slash
  )

(qv/face ltx :family "FreeSerif")
(qv/face ltx-overline :overline t)

(defvar-local ltx-expr nil)
(define-derived-mode ltx-mode nil "Ltx Mode"
  (use-local-map ltx-map)
  (setq-local ltx-expr nil)
  (add-hook 'post-command-hook #'ltx-highlight-expr nil t)
  )

;;; Highlight expr
(defvar-local ltx-current-face nil)
(defun ltx-highlight-expr ()
  (interactive)
  (let ((face (get-text-property (point) 'expr-face)))
    (when (not (eq face ltx-current-face))
      (when ltx-current-face (setf (nth 1 ltx-current-face) nil))
      (when face (setf (nth 1 face) 'highlight))
      (setq ltx-current-face face))))


;;; Insert expr

(defun ltx-insert-expr (expr)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)
        (lines (ltx-expr-lines expr)))
    (remove-overlays (point-min) (point-max))
    (delete-region (point-min) (point-max))
    (setq qv/ls lines)
    (insert (s-join "\n" (reverse lines)))))

(setq ltx-symbol-alist
      '())

(defun ltx-expr-lines (expr)
  (let ((lines (ltx-expr-lines-base expr))
        (face (list :inherit nil)))
    (dolist (line lines)
      (alter-text-property 0 (length line) 'expr (lambda (e) (or e expr)) line)
      (alter-text-property 0 (length line) 'expr-face (lambda (e) (or e face)) line)
      (add-face-text-property 0 (length line) face nil line))
    lines))

(defun ltx-expr-lines-base (expr)
  (pcase (car expr)
    ('text (list (propertize (cadr expr) 'face 'ltx)))
    ('list (-reduce 'ltx--concat-lines (mapcar #'ltx-expr-lines (cdr expr))))
    ('frac (let* ((top (ltx--concat-lines (ltx--concat-lines '(" ") (ltx-expr-lines (cadr expr))) '(" ")))
                  (btm (ltx--concat-lines (ltx--concat-lines '(" ") (ltx-expr-lines (caddr expr))) '(" ")))
                  (padded (ltx--pad-lines-center (append btm top)))
                  (top-btm (nth (1- (length btm)) padded)))

             (message "Making overline %s" top-btm)
             (add-face-text-property 0 (length top-btm) 'ltx-overline nil top-btm)
             padded
             ))
    ))


;;; Utils
(defun ltx--pixel-space (width)
  (if (eq width 0) "" (propertize " " 'display `(space :width (,width))))
  ;; (make-string width ?\s)
  )

(defun ltx--pixel-width (string)
  (cond ((eq (length string) 0) "")
        ((boundp #'string-pixel-width) (string-pixel-width string))
        (t (shr-string-pixel-width string))))

(defun ltx--pad-lines-center (lines)
  (let ((max-w (apply #'max (mapcar #'ltx--pixel-width lines))))
    (--map (let* ((wid (- max-w (ltx--pixel-width it)))
                  (half-space (ltx--pixel-space (/ wid 2))))
             (concat half-space it half-space))
           lines)))

(defun ltx--concat-lines (ls1 ls2)
  (let ((max-l1 (apply #'max (mapcar #'ltx--pixel-width ls1)))
        (max-l2 (apply #'max (mapcar #'ltx--pixel-width ls2))))
    (--map (concat (or (nth it ls1) (ltx--pixel-space max-l1))
                   (or (nth it ls2) (ltx--pixel-space max-l2)))
           (number-sequence 0 (1- (max (length ls1) (length ls2)))))))


(defun ltx-slash ()
  ()
  )

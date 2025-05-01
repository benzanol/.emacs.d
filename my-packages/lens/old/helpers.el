(defun lens-set-tag (state tag &optional remove)
  "REMOVE can be nil (add), 'toggle (toggle), or non-nil (remove)"
  (let* ((tags (lens-get-tags state))
         (contains (memq tag tags)))

    (when (eq add 'toggle)
      (setq remove (if contains 'remove nil)))

    ;; Check that the action would actually be doing something
    (when (or (and contains remove)
              (and (not contains) (not remove)))
      (if remove
          (delq tag tags)
        (push tag tags))

      (setf (alist-get 'tags (car state))
            (s-join " " (mapcar #'symbol-name tags))))))

(defun lens-get-tags (state)
  (let* ((tags-str (or (alist-get 'tags (car state)) "")))
    (mapcar #'intern (split-string tags-str " "))))

(defun lens-has-tag (state tag)
  (memq tag (lens-get-tags state)))


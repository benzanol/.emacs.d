(defun st--format (dom &optional vars)
  (let ((all-vars (append (plist-get dom :vars) vars)))

    (pcase (plist-get dom :type)
      ('const (split-string (plist-get dom :content) "\n"))

      ('text (split-string
              (propertize (plist-get dom :content)
                          'face (list :box t)
                          ;; Reference the plist itself stored in dom
                          'modify `(lambda (s) (plist-put ',dom :content s)))
              "\n"))

      ('div (let ((prefix (alist-get 'div-prefix all-vars)))
              (--map (if prefix (concat prefix it) it)
                     (flatten-list (--map (st--format it all-vars)
                                          (plist-get dom :children)))))))))

;; Add modification hooks using overlays
(defun st-insert (dom)
  (let ((line (line-number-at-pos))
        (col (current-column))
        (inhibit-modification-hooks t)
        (inhibit-read-only t)

        bol prop o func modify-func intervals)

    ;; Remove the previous contents of the buffer
    (delete-region (point-min) (point-max))
    (remove-overlays)

    (dolist (line (st--format dom))
      (insert line)
      (insert "\n"))

    (goto-char (point-min))
    (while (setq prop (text-property-search-forward 'modify))
      (setq o (make-overlay (prop-match-beginning prop)
                            (prop-match-end prop)
                            nil nil 'rear-advance))

      (setq func `(lambda (ol &rest args)
                    (message "func")
                    (,(prop-match-value prop)
                     (buffer-substring-no-properties
                      (overlay-start ol) (overlay-end ol)))
                    (st-insert ',dom))

            modify-func
            `(lambda (ol after &rest args)
               (message "mofiyf")
               (when after (,func ol))))

      ;; (overlay-put o 'insert-in-front-hooks (list func))
      ;; (overlay-put o 'insert-behind-hooks (list func))
      (overlay-put o 'modification-hooks (list modify-func)))

    (run-with-timer
     0 nil
     (lambda (line col)
       (goto-char (point-min))
       (forward-line (1- line))
       (forward-char col))
     line col)))

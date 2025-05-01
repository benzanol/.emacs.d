;;; Add an overlay to the header stars
(bz/hook outline-view-change-hook bz/org-header-overlay-timer :remove
  (run-with-timer 0 nil 'bz/org-update-header-overlay))

(defun bz/org-update-header-overlay (&rest args)
  (save-excursion
    ;; Go to the previous heading when called with hook, but not from org indent mode
    (unless args (forward-line 1) (outline-previous-visible-heading 1))

    (remove-overlays (line-beginning-position) (line-end-position) 'hide-stars t)
    (beginning-of-line)
    (when (and (not (looking-at "\\*+ *$"))
               (search-forward-regexp org-heading-regexp (line-end-position) t))
      (let ((o (make-overlay (match-beginning 1) (1+ (match-end 1)) nil t nil)))
        (overlay-put o 'hide-stars t)
        (overlay-put o 'invisible t)
        (overlay-put o 'intangible t)
        (overlay-put o 'modification-hooks
                     '((lambda (ov &rest args) (delete-overlay ov))))

        (overlay-put
         o 'after-string
         (propertize
          (concat #("⦿ " 0 1 (display ((height 0.7) (raise 0.15))))
                  ;; (concat #("⦿"
                  ;; (if (let ((cur (org-current-level)))
                  ;;       (save-excursion
                  ;;         (or (= 1 (forward-line 1))
                  ;;             (and (looking-at org-heading-regexp)
                  ;;                  (<= (org-current-level) cur)))))
                  ;;     #("⦿ " 0 1 (display ((height 0.7) (raise 0.15))))
                  ;;   (if (outline-invisible-p (line-end-position))
                  ;;       #("› " 1 2 (display (height 1.3)))
                  ;;     (propertize "ˬ" 'face '(:weight bold) 'display '((raise 0.3)))))
                  (propertize " " 'display '(raise 0.3)))
          'face '(:inherit bz/org-header :underline nil)))))))

;;; Indent contents of headings
(bz/advise :remove org-indent-set-line-properties
           bz/oislp-override (level indentation &optional heading)
  (bz/org-update-header-overlay 'dont-go-back)
  (when (not heading) ; When it isn't a heading
    (if (save-excursion ; If inside of a drawer, indent doubly
          (let* ((p (line-end-position))
                 (s (search-backward-regexp org-drawer-regexp nil t))
                 (e (search-forward-regexp org-property-end-re nil t))
                 (h (search-backward-regexp org-heading-regexp s t)))
            (and s e (not h) (> e p))))
        (setq level (+ 2 level))
      (setq level (+ 1 level))))

  (let* ((line (aref org-indent--text-line-prefixes level))
         (wrap (aref bz/org-indent--wrap-prefixes level)))
    (add-text-properties
     (line-beginning-position) (line-beginning-position 2)
     `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

(bz/face bz/org-truncate-wrap :fg gray2 :h 1.1)

;;; Idk

(defun org-indent-set-line-properties (level indentation &optional heading)
  (let* ((line (aref org-indent--text-line-prefixes level)))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,line wrap-prefix ,line)))
  (forward-line))

;;; Prefixes
(bz/advise :override org-indent--compute-prefixes
           bz/org-indent-guide--compute-prefixes ()
  (let ((prefixes (make-vector org-indent--deepest-level nil))
        (wraps (make-vector org-indent--deepest-level nil)))
    (dotimes (i org-indent--deepest-level)
      (aset prefixes i
            (if (= i 0) ""
              (concat (aref prefixes (1- i))
                      (bz/org-indent-guide-string i))))
      (unless (eq i 0)
        (aset wraps (1- i) (concat (substring (aref prefixes i) 0 -1) "    "))))

    (setq org-indent--heading-line-prefixes prefixes)
    (setq org-indent--inlinetask-line-prefixes prefixes)
    (setq org-indent--text-line-prefixes prefixes)
    (setq bz/org-indent--wrap-prefixes wraps)
    nil))


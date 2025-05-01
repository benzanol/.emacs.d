(setq s2-fold-indicator (propertize " ..." 'face 'shadow))

(defun s2-fold-block ()
  (interactive)

  (save-excursion
    (let ((depth (or (get-text-property (point) 's2-depth) 0))
          (block-start (point-at-eol))
          block-end prop-match ol)

      (forward-line 1)
      (unless (< depth (get-text-property (point) 's2-depth))
        (error "Not looking at an indented block"))

      (setq prop-match (text-property-search-forward 's2-depth depth (lambda (d p) (>= d (or p 0)))))
      (setq block-end (if prop-match (prop-match-beginning prop-match) (point-max)))

      (setq ol (make-overlay block-start (- block-end 2)))
      (overlay-put ol 's2-fold t)
      (overlay-put ol 'invisible t)
      (overlay-put ol 'before-string (concat s2-fold-indicator "\n")))))

(defun s2-unfold-block ()
  (interactive)
  (let ((ol (--find (overlay-get it 's2-fold) (overlays-at (point-at-eol)))))
    (if ol (delete-overlay ol)
      (error "Not folded"))))

(defun s2-toggle-fold ()
  (interactive)
  (let ((ol (--find (overlay-get it 's2-fold) (overlays-at (point-at-eol)))))
    (if ol (delete-overlay ol)
      (s2-fold-block))))

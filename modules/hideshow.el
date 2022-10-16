(qv/package hideshow)

(qv/hook hs-minor-mode-hook qv/outline-disable
  (when hs-minor-mode (outline-minor-mode 0)))

(defmacro qv/save-column (&rest forms)
  `(let ((col (current-column)))
     ,@forms
     (beginning-of-visual-line)
     (forward-char col)))

(defun qv/hs-toggle (&optional all)
  (interactive)
  (save-excursion
    (if (hs-overlay-at (line-end-position))
        (progn (hs-show-block)
               (unless all (forward-line 1) (hs-hide-level 1)))

      (beginning-of-visual-line)
      (let ((min (point)))
        (forward-line 1)
        (hs-find-block-beginning)
        (when (>= (point) min)
          (hs-hide-block))))))

(defun qv/hs-toggle-defun ()
  (interactive)
  (qv/save-column
   (end-of-line)
   (search-backward-regexp "^[^ \t\n]")
   (qv/hs-toggle)))

(qv/keys hs-minor-mode-map
  :sparse t
  :keymode normal
  "SPC" (qv/hs-toggle 'all)
  "S-SPC" qv/hs-toggle
  [remap qvk-down] (@ qv/hs-down (end-of-visible-line) (next-line))
  [remap qvk-up] (@ qv/hs-up (beginning-of-visual-line) (forward-line -1)
                             (beginning-of-visual-line)
                             (when goal-column
                               (if (>= goal-column (- (line-end-position) (line-beginning-position)))
                                   (end-of-line) (forward-char goal-column))))
  [remap qvk-down4] (dotimes (i 4) (qv/hs-down))
  [remap qvk-up4] (dotimes (i 4) (qv/hs-up))

  "g z r" (@ qv/hs-show (qv/save-column (hs-show-all)))
  "g z m" (@ qv/hs-hide (qv/save-column (hs-hide-all)))
  "g z k" beginning-of-defun
  "g z j" end-of-defun
  "g z h" (qv/save-column (hs-hide-block))
  "g z l" hs-hide-level)

;; Treat comments as if they are just more code
(defun hs-inside-comment-p () nil)

(qv/package hideshow)

(defmacro qv/save-column (&rest forms)
  `(let ((col (current-column)))
     ,@forms
     (beginning-of-visual-line)
     (forward-char col)))

(qv/keys hs-minor-mode-map
  :sparse t
  "SPC" (@ qv/hs-toggle
           (qv/save-column
            (beginning-of-visual-line)
            (if (hs-overlay-at (line-end-position)) (hs-show-block)
              (forward-line 1) (hs-hide-block))))
  "S-SPC" (@ qv/hs-toggle-defun
             (qv/save-column
              (end-of-line)
              (search-backward-regexp "^[^ \t\n]")
              (qv/hs-toggle)))
  "g z r" (@ qv/hs-show (qv/save-column (hs-show-all)))
  "g z m" (@ qv/hs-hide (qv/save-column (hs-hide-all)))
  "g z k" beginning-of-defun
  "g z j" end-of-defun
  "g z h" hs-hide-block
  "g z l" hs-hide-level)

;; Treat comments as if they are just more code
(defun hs-inside-comment-p () nil)

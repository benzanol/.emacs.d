(bz/package outline)

(bz/hook outline-minor-mode-hook bz/outline-mode-setup
  (when outline-minor-mode (hs-minor-mode 0))
  (bz/outline-hide))

(defun bz/outline-after-move-line ()
  (ignore-errors
    (when (outline-invisible-p (1- (point)))
      (beginning-of-visual-line)
      (end-of-line))))

;;; Outline keybindings
(bz/keys outline-minor-mode-map
  :sparse t
  [remap bz/up] (@ bz/outline-up (unwind-protect (bz/up) (bz/outline-after-move-line)))
  [remap bz/down] (@ bz/outline-down (unwind-protect (bz/down) (bz/outline-after-move-line)))

  [remap bz/fold-toggle] outline-toggle-children

  [remap bz/fold-show] outline-show-all
  [remap bz/fold-hide]
  (@ bz/outline-hide
     (save-excursion (beginning-of-buffer)
                     (outline-hide-sublevels (if =arg= 100 1))))

  [remap bz/fold-toggle-all] (if (--find (eq (overlay-get it 'invisible) 'outline) (overlays-in (point-min) (point-max)))
                                 (outline-show-all) (bz/outline-hide))

  [remap bz/fold-level]
  (@ bz/outline-fold-level
     (outline-show-entry) (outline-show-children) (next-line)))

;;; Switch between hideshow
(bz/keys *
  "C-x C-h" (if hs-minor-mode
                (progn (outline-minor-mode) (message "Outline mode"))
              (hs-minor-mode) (message "Hideshow mode")))

;;; Add a custom character for outline text
(set-display-table-slot
 standard-display-table
 'selective-display (string-to-vector " ➾"))

;;; Automatically move off of overlay
;; Redundant because of 'stay on same line' functions
;; (bz/hook post-command-hook bz/move-off-outline-overlays
;;   (when (or outline-minor-mode (eq major-mode 'org-mode))
;;     (when (or (outline-invisible-p)
;;               (save-excursion (goto-char (1- (point)))
;;                               (outline-invisible-p)))
;;       (beginning-of-visual-line)
;;       (end-of-line))))

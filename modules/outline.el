(qv/package outline)

;;; Outline keybindings
(qv/keys outline-minor-mode-map
  :sparse t
  32 outline-toggle-children
  "g z r" outline-show-all
  "g z j" outline-next-heading
  "g z k" outline-previous-heading
  "g z s" outline-show-subtree
  "g z m" (defun qv/outline-hide (level)
              (interactive "P")
	        (save-excursion
		      (beginning-of-buffer)
              (outline-hide-sublevels
		       (if level 100 4))))
  "g z h" ((when (eq (outline-back-to-heading) t) (outline-up-heading 1))
	       (outline-hide-subtree))
  "g z l" ((outline-show-entry) (outline-show-children) (next-line)))

;;; Add a custom character for outline text
(set-display-table-slot
 standard-display-table 
 'selective-display (string-to-vector " ➾"))

;;; Prog mode
(qv/hook prog-mode-hook qv/prog-mode-outline-setup
  (outline-minor-mode 1)
  (setq-local outline-level 'outline-level)
  (setq-local outline-regexp "^\\(\t\\| \\)*;;;+ \\|^\\(\t\\| \\)*///+ "))

;;; Automatically move off of overlay
;; Redundant because of 'stay on same line' functions
(defun qv/move-off-outline-overlay ()
  (when outline-minor-mode
    (when (or (outline-invisible-p)
              (save-excursion (goto-char (1- (point)))
                              (outline-invisible-p)))
      (beginning-of-visual-line)
      (end-of-line))))

;;(add-hook 'post-command-hook 'qv/move-off-outline-overlay)

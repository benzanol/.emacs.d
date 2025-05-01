(bz/package hideshow)

(bz/hook hs-minor-mode-hook bz/hs-mode-setup
  (when hs-minor-mode (outline-minor-mode 0))
  (bz/hs-hide-all))

(defmacro bz/save-column (&rest forms)
  `(let ((col (current-column)))
     ,@forms
     (beginning-of-visual-line)
     (forward-char col)))

(defun bz/hs-toggle (&optional all)
  (interactive)
  (save-excursion
    (if (hs-overlay-at (point-at-eol))
        ;; Unfold
        (if all (while (hs-overlay-at (point-at-eol)) (hs-show-block 'end))
          (hs-show-block) (forward-line 1)
          (call-interactively 'hs-hide-level))

      ;; Fold
      (setq goal-column 0)
      (let ((min (line-beginning-position)) successive beg lasto last-indent)
        (forward-line 1)
        (while (and (or all (not successive))
                    (progn (setq beg (hs-find-block-beginning))
                           (and beg (>= beg min))))
          (when lasto (overlay-put lasto 'after-string (concat "\n" last-indent)))

          (setq successive t)
          ;; (beginning-of-line)
          (hs-hide-block 'end)
          (setq lasto (car (overlays-at (1- (point))))
                last-indent (buffer-substring (point-at-bol) (point)))
          (end-of-line)
          ;; Previous line
          (setq min (line-beginning-position 0)))

        (when (and (not all) lasto)
          (overlay-put lasto 'after-string (concat "\n" last-indent)))))))

(defun bz/hs-toggle-defun ()
  (interactive)
  (bz/save-column
   (end-of-line)
   (search-backward-regexp "^[^ \t\n]")
   (bz/hs-toggle)))

(bz/keys hs-minor-mode-map
  :sparse t

  [remap bz/right]
  (@ bz/hs-right
     (or (ignore-errors
           (let* ((o (hs-already-hidden-p)))
             (when (and o (eq (point) (overlay-start o)))
               (goto-char (overlay-end o)))))
         (bz/right)))
  [remap bz/right4] (@ bz/hs-right4 (dotimes (i 4) (bz/hs-right)))
  [remap bz/left]
  (@ bz/hs-left
     (or (ignore-errors
           (let* ((o (save-excursion (backward-char) (hs-already-hidden-p))))
             (when (and o (eq (overlay-end o) (point)))
               (goto-char (overlay-start o)) (bz/left))))
         (bz/left)))
  [remap bz/left4] (@ bz/hs-left4 (dotimes (i 4) (bz/hs-left)))

  [remap bz/fold-toggle] (@ bz/hs-toggle-full (bz/hs-toggle 'all))
  [remap bz/fold-toggle-small] bz/hs-toggle
  [remap bz/fold-toggle-all]
  (@ bz/hs-toggle-buffer
     (if (--find (overlay-get it 'hs) (overlays-in (point-min) (point-max)))
         (bz/hs-show-all) (bz/hs-hide-all)))

  [remap bz/fold-hide] (@ bz/hs-hide-all (bz/save-column (hs-hide-all)))
  [remap bz/fold-show] (@ bz/hs-show-all (bz/save-column (hs-show-all)))
  [remap bz/fold-level] hs-hide-level)

;; Treat comments as if they are just more code
(bz/advise :override hs-inside-comment-p ignore)


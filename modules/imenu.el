(setq imenu-list-buffer-name "*imenu*")


(qv/face imenu-list-entry-subalist-face-0 variable-pitch :fg blue :h 1.1 :u nil)
(qv/face imenu-list-entry-face-1 :fg gray1)
(qv/face hl-line nil :bg gray3)

(define-minor-mode qv/imenu-mode
  "Display imenu buffer in side window"
  :keymap qv/imenu-map
  :global t
  (if qv/imenu-mode
      (progn
        (switch-to-buffer (imenu-list-get-buffer-create))
        (add-hook 'post-command-hook #'qv/imenu-update))
    (remove-hook 'post-command-hook #'qv/imenu-update)))

(qv/keys imenu-list-major-mode-map
  :sparse t
  :keymode normal
  "RET" qv/imenu-goto)

(qv/keys qv/imenu-map
  :sparse t
  "C-M-j" qv/imenu-next
  "C-M-k" qv/imenu-previous)

(defun qv/imenu-goto ()
  (interactive)
  (when (markerp (cdr (imenu-list--find-entry)))
    (let* ((m (cdr (imenu-list--find-entry)))
           (buf (marker-buffer m))
           (win (get-buffer-window buf)))
      (if win (select-window win)
        (switch-to-buffer buf))
      (goto-char (marker-position m)))))

(defun qv/imenu-update ()
  (unless (memq this-command '(qv/imenu-next qv/imenu-previous qv/imenu-goto))
    (when-let* ((cur (current-buffer))
                (buf (get-buffer imenu-list-buffer-name))
                (win (get-buffer-window buf)))
      (run-with-timer 0 nil 'imenu-list-update))))

(defun qv/imenu-next (&optional back)
  (interactive)
  (let (m)
    (with-selected-window (get-buffer-window imenu-list-buffer-name)
      (while (progn (line-move-visual (if back -1 1))
                    (not (or (markerp (cdr (imenu-list--find-entry)))
                             (bobp) (eobp)))))
      (setq m (cdr (imenu-list--find-entry)))
      (hl-line-mode 1))
    (when (and (markerp m) (eq (current-buffer) (marker-buffer m)))
      (goto-char (marker-position m)))))

(defun qv/imenu-previous ()
  (interactive)
  (qv/imenu-next t))

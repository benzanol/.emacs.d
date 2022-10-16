;;;; Center Cursor
(setq-default scroll-margin 100000)
(setq-default maximum-scroll-margin 1.0)

(qv/hook post-command-hook qv/recenter
  (when recenter-cursor-mode
	(ignore-errors
      (let ((cursor (point)))
        (recenter)
        (goto-char cursor)))))

(define-minor-mode recenter-cursor-mode
  "Recenter the cursor after every command."
  t)

(defun disable-recenter-cursor ()
  (interactive)
  (recenter-cursor-mode 0))

(mapcar (lambda (hook) (add-hook hook 'disable-recenter-cursor))
        '(minibuffer-setup-hook
          calendar-mode-hook))

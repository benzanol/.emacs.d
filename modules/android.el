(qv/keys tool-bar-map
  :sparse t
  [execute-command]
  ,'(menu-item "Command" execute-extended-command
               :image (image :type xpm :file "~/Downloads/Bear.xpm")
               :help "Execute a fancy shmancy command"))

(qv/face tool-bar mode-line)
(qv/face menu :fg fg :bg bg2)

(qv/keys my-menu
  :sparse t
  [my] ,'(menu-item "Command" execute-extended-command :help "Something..."))

(define-key global-map [menu-bar my-menu] (cons "Mine" my-menu))




(setq qv/highlight-word-delay 0.5)

(qv/keys *
  [down-mouse-1] (setq qv/mouse-down-time (current-time))

  [mouse-1]
  (defun qv/mouse-click (e)
    (interactive "e")
    (deactivate-mark)
    (select-window (car (cadr e)))
    (goto-char (cadr (cadr e)))
    (when (> (time-to-seconds (subtract-time (current-time) qv/mouse-down-time)) 0.3)
      (save-excursion (backward-word) (set-mark-command nil))
      (forward-word)))

  [drag-mouse-1]
  (defun qv/mouse-drag (e)
    (interactive "e")
    (let ((start (cadr (nth 1 e))) (end (cadr (nth 2 e))))
      (if (not mark-active)
          ;; If no mark is active, select from start to end of drag
          (progn (set-mark start) (goto-char end))

        ;; If mark is active, move whichever end is closer
        (when (> (abs (- start (point))) (abs (- start (mark))))
          (exchange-point-and-mark))
        (goto-char end)))))

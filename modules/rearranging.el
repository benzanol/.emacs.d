(defun qv/window-move-right ()
  (interactive)
  (qv/window-move nil t))

(defun qv/window-move-left ()
  (interactive)
  (qv/window-move nil nil))

(defun qv/window-move-down ()
  (interactive)
  (qv/window-move t t))

(defun qv/window-move-up ()
  (interactive)
  (qv/window-move t nil))

(defun qv/window-move (vertical forward)
  (when (one-window-p) (error "Only one window"))
  (let ((win (selected-window))
        (win-state (window-state-get (selected-window)))
        (tree (car (window--subtree (window-parent))))
        (split-direction
         (if vertical (if forward 'below 'above) (if forward 'right 'left))))
    ;; Create a split window to move the current window to
    (select-window
     ;; If the movement is in the same direction as the current list
     (if (eq (car tree) vertical)

         ;; If there are two windows, and they should be swapped
         (if (and (eq (window-child-count (window-parent)) 2)
                  (xor forward (eq (selected-window) (window-last-child (window-parent))))
                  (if (window-next-sibling)
                      (window-live-p (window-next-sibling))
                    (window-live-p (window-prev-sibling))))
             (let* ((other-win (if forward (window-next-sibling) (window-prev-sibling))))
               (split-window other-win nil split-direction))

           ;; If the window is at the end of the current stack, move it out if possible
           (if (or (and forward (eq (selected-window) (window-last-child (window-parent))))
                   (and (not forward) (eq (selected-window) (window-child (window-parent)))))
               (if (window-parent (window-parent))
                   (split-window (window-parent (window-parent)) nil split-direction)
                 (error "Nowhere to move"))

             ;; Move the window along the current list
             (let* ((next-win (if forward (window-next-sibling) (window-prev-sibling)))
                    (next-win (if (window-live-p next-win) next-win
                                (window-last-child next-win))))
               (split-window next-win nil
                             (if vertical 'right 'below)))))

       ;; Move the window out of the current stack
       (split-window (window-parent) nil split-direction)))

    (delete-window win)
    (window-state-put win-state (selected-window))))

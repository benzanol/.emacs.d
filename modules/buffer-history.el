(defvar bz/switch-window-hook nil)

(setq bz/current-window (selected-window)
      bz/last-window (selected-window)
      bz/buffer-history (buffer-list)
      bz/buffer-history-pos 0
      bz/buffer-history-pos-modified nil)


;; After calling bz/buffer-history-back:
;; 1. Point bz/buffer-history-pos to the correct index of the new buffer in bz/buffer-history
;; 2. Set bz/buffer-history-pos-modified to true
;; 3. Switch to the new buffer WITHOUT rearranging bz/buffer-history
;; 4. Post command hook sets pos-modified to nil but does NOT update pos
;; 5. Window state change hook sees that pos is not zero, and does nothing
;; 6. REGULAR ACTION
;; 7. Post command hook sees that pos-modified is nil, and sets pos to zero
;; 8. Window state change hook sees that pos is zero, and puts the settled on buffer at the start of history


;; window-state-change-hook runs AFTER post-command-hook
(bz/hook (window-state-change-hook buffer-list-update-hook) bz/update-window-variables
  (ignore-errors
    (when (= bz/buffer-history-pos 0)
      ;; Remove dead buffers
      (setq bz/buffer-history (seq-filter #'buffer-live-p bz/buffer-history))

      ;; Check if the window changed
      (unless (or (minibufferp nil) (eq bz/current-window (selected-window)))
        (setq bz/last-window bz/current-window
              bz/current-window (selected-window))
        (run-hook-with-args 'bz/switch-window-hook))

      ;; Move the current buffer to the head of the buffer history
      (unless (or (minibufferp nil) (eq (current-buffer) (car bz/buffer-history)))
        (setq bz/buffer-history (cons (current-buffer) (delq (current-buffer) bz/buffer-history)))))))

(defun bz/buffer-history-back (&optional filter)
  "Filter is a function to only switch to a buffer if the filter is satisfied."
  (interactive)

  ;; By default, find the first non-displayed buffer
  (let* ((rotated-bufs (-rotate (- (1+ bz/buffer-history-pos)) bz/buffer-history))
         (func (or filter (lambda (b) (null (get-buffer-window b)))))
         (rotated-idx (-find-index func rotated-bufs)))

    (if rotated-idx
        (setq bz/buffer-history-pos (mod (+ rotated-idx (1+ bz/buffer-history-pos)) (length bz/buffer-history))
              bz/buffer-history-pos-modified t)

      ;; If no new buf, switch to the original buffer
      (setq bz/buffer-history-pos 0))

    ;; The window state update hook will automatically update the buffer history
    (switch-to-buffer (nth bz/buffer-history-pos bz/buffer-history))

    (add-hook 'post-command-hook #'bz/buffer-history-forget-pos)))

(defun bz/buffer-history-cancel ()
  (interactive)
  (setq bz/buffer-history-pos 0 bz/buffer-history-pos-modified nil)
  (switch-to-buffer (car bz/buffer-history)))


(defun bz/buffer-history-forget-pos ()
  (if bz/buffer-history-pos-modified
      (setq bz/buffer-history-pos-modified nil)
    (setq bz/buffer-history-pos 0)
    (remove-hook 'post-command-hook #'bz/buffer-history-forget-pos)
    (bz/update-window-variables)))

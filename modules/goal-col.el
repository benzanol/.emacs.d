;; An effificent way to track the cursor column without slowing down scrolling

(setq-local qv/last-position '(nil . nil))

(defun qv/set-goal-col ()
  (interactive)
  (ignore-errors
    (if truncate-lines
        (let ((line (line-number-at-pos))
              (col (current-column)))
          (if (or (not (eolp))
                  (> col goal-column)
                  ;; Same line but different position
                  (and (eq (car qv/last-position) line)
                       (not (eq (cdr qv/last-position) col))))
              (setq-local goal-column (current-column)))
          (setq-local qv/last-position (cons line col)))
      (setq goal-column nil))))

(add-hook 'post-command-hook 'qv/set-goal-col)

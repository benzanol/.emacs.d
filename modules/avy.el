(bz/package avy)
(bz/package dash)

(setq avy-keys
      (mapcar (lambda (s) (aref s 0))
              (split-string
               ;; All except q
               "asdfghjklwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM"
               "" t)))

;; Press q to cancel
(push ?q avy-escape-chars)

(defun bz/avy-word-in-line ()
  "Docstring is pretty cool"
  (interactive)
  (avy-with avy-goto-char
    (avy-jump "\\<."
              :beg (line-beginning-position)
              :end (line-end-position))))

(defun bz/avy-dired-file ()
  (interactive)

  (let ((win (selected-window))
        cands pos file)
    (dolist (win (window-list))
      (with-selected-window win
        (when (derived-mode-p 'dired-mode)

          (save-excursion
            (goto-char (point-min))
            (while (eq 0 (forward-line 1))
              (when (dired-get-filename nil t)
                (push (cons (point) win) cands)))))))

    (setq pos (avy-process (reverse cands)))

    (save-excursion
      (goto-char pos)
      (setq file (dired-get-filename))

      (when (file-directory-p file)
        (dired-subtree-toggle)
        (setq file nil)))

    (select-window win)

    (when file (find-file file))))

;; (bz/face avy-lead-face   :bg bg3 :fg fg :w bold :b (:color ,(bz/color gray2)))
;; (bz/face avy-lead-face-0 :bg bg3 :fg fg :w bold :b (:color ,(bz/color gray2)))
;; (bz/face avy-lead-face-1 :bg bg3 :fg fg :w bold :b (:color ,(bz/color gray2)))
;; (bz/face avy-lead-face-2 :bg bg3 :fg fg :w bold :b (:color ,(bz/color gray2)))
(bz/face avy-lead-face fixed-pitch :fg highlight :bg nil :w bold)
(bz/face avy-lead-face-0 avy-lead-face :fg nil :bg nil)
(bz/face avy-lead-face-1 avy-lead-face :fg nil :bg nil)
(bz/face avy-lead-face-2 avy-lead-face :fg nil :bg nil)

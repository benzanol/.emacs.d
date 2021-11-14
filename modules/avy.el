(qv/package avy)

(setq avy-keys
      (mapcar (lambda (s) (aref s 0))
              (split-string
               "asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM"
               "" t)))

(defun qv/avy-word-in-line ()
  (interactive)
  (avy-with avy-goto-char
    (avy-jump "\\<."
              :beg (line-beginning-position)
              :end (line-end-position))))

(qv/key modeal-normal-map "g f" qv/avy-word-in-line)

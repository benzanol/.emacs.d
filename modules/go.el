(defun bz/go-fmt ()
  (interactive)
  (bz/save-position
   (shell-command-on-region (point-min) (point-max) "gofmt" nil t)))

(setf (alist-get 'go-mode bz/indent-command-alist) nil)

(bz/require camel-to-snake)


(defvar bz/lua-process)

(defun bz/lua-set-terminal ()
  (interactive)
  (setq bz/lua-process (get-buffer-process (current-buffer))))

(defun bz/lua-eval (beg end)
  (interactive
   (cond (mark-active (list (point) (mark)))
         ((looking-back "[])}]")
          (list (point-at-eol) (save-excursion (backward-sexp) (point-at-bol))))
         ((list (point-at-bol) (point-at-eol)))))

  (process-send-string bz/lua-process (concat (buffer-substring beg end) "")))

(bz/keys lua-mode-map
  "C-e" bz/lua-eval)


(bz/hook lua-mode-hook bz/lua-mode-setup
  (camel-to-snake-mode 1))

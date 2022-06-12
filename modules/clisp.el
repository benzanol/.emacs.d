(defvar clr-term-process nil
  "Process of the clronite terminal.")

(defun clr-set-terminal ()
  (interactive)
  (setq clr-term-process
        (get-buffer-process (current-buffer))))

(defun clr-send-string (str)
  ;; Remove newlines from beginning and end of str
  (setq str (replace-regexp-in-string "\\`\n+" "" str))
  (setq str (replace-regexp-in-string "\n+\\'" "" str))

  ;; Put in brackets if multiple lines, then add newline to the end
  (term-send-string clr-term-process (concat str "\n")))

(defun org-babel-execute:clisp (body params)
  (clr-send-string body)
  nil)

(defun clr-send-buffer (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (clr-send-string (buffer-string))))

(defun clr-send-last-sexp ()
  (interactive)
  (let ((pos (point)))
    (backward-sexp)
    (clr-send-string
     (buffer-substring-no-properties
      (point) pos))
    (goto-char pos)))

(defun clr-send-region (beg end)
  (interactive (list (mark) (point)))
  (clr-send-string
   (buffer-substring-no-properties
    beg end)))

(defun clr-send-defun ()
  (interactive)
  (save-excursion
    (end-of-line)
    (search-backward-regexp "^[^\t\n ]")
    (clr-send-region (point) (progn (forward-sexp) (point)))))
    

(defvar clr-default-buffers nil
  "Buffers to send to clronite.")

(defun clr-add-default-buffer ()
  (interactive)
  (add-to-list 'clr-default-buffers (current-buffer))
  (message "%s" (mapcar 'buffer-name clr-default-buffers)))

(defun clr-remove-default-buffer ()
  (interactive)
  (setq clr-default-buffers (remove (current-buffer) clr-default-buffers))
  (message "%s" (mapcar 'buffer-name clr-default-buffers)))

(defun clr-send-default-buffers ()
  (interactive)
  (clr-send-string
   (format "{%s}\n" (mapconcat (lambda (b) (with-current-buffer b (buffer-string)))
                               clr-default-buffers "\n"))))

(defun clr-eval (&optional arg)
  (interactive "p")
  (cond (mark-active (clr-send-region (mark) (point)))
        ((eq arg 4) (clr-send-buffer))
        ((eq arg 16) (clr-send-default-buffers))
        (t (clr-send-last-sexp))))

(qv/keys clr-map
  :sparse t
  "C-e" clr-eval
  "C-M-x" clr-send-defun)

(define-minor-mode clr-mode
  "Clronite repl interaction"
  nil
  :global nil
  :keymap clr-map)

(add-hook 'lisp-mode-hook 'clr-mode)

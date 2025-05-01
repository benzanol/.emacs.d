(defvar pyr-term-process nil
  "Process of the clronite terminal.")

(defun pyr-set-terminal ()
  (interactive)
  (setq pyr-term-process
        (get-buffer-process (current-buffer))))

(defun pyr-send-string (str)
  ;; Remove newlines from beginning and end of str
  (setq str (replace-regexp-in-string "\n\s*\n" "\n" str))

  ;; Put in brackets if multiple lines, then add newline to the end
  (term-send-string pyr-term-process (concat str "\n")))

(defun org-babel-execute:python (body params)
  (pyr-send-string body)
  nil)

(defun pyr-send-buffer (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (pyr-send-string (buffer-string))))

(defun pyr-send-last-sexp ()
  (interactive)
    (pyr-send-string
     (buffer-substring-no-properties
      (line-beginning-position)
      (line-end-position))))

(defun pyr-send-region (beg end)
  (interactive (list (mark) (point)))
  (pyr-send-string
   (buffer-substring-no-properties
    beg end)))

(defun pyr-send-defun ()
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((start (prog1 (search-backward-regexp "^[^\t\n ]") (end-of-line)))
          (end (progn (search-forward-regexp "^[^\t\n ]") (forward-line -1) (point))))
    
    (pyr-send-region start end))))
    

(defvar pyr-default-buffers nil
  "Buffers to send to clronite.")

(defun pyr-add-default-buffer ()
  (interactive)
  (add-to-list 'pyr-default-buffers (current-buffer))
  (message "%s" (mapcar 'buffer-name pyr-default-buffers)))

(defun pyr-remove-default-buffer ()
  (interactive)
  (setq pyr-default-buffers (remove (current-buffer) pyr-default-buffers))
  (message "%s" (mapcar 'buffer-name pyr-default-buffers)))

(defun pyr-send-default-buffers ()
  (interactive)
  (pyr-send-string
   (format "{%s}\n" (mapconcat (lambda (b) (with-current-buffer b (buffer-string)))
                               pyr-default-buffers "\n"))))

(defun pyr-eval (&optional arg)
  (interactive "p")
  (cond (mark-active (pyr-send-region (mark) (point)))
        ((eq arg 4) (pyr-send-buffer))
        ((eq arg 16) (pyr-send-default-buffers))
        (t (pyr-send-last-sexp))))

(qv/keys pyr-map
  :sparse t
  "C-e" pyr-eval
  "C-M-x" pyr-send-defun)

(define-minor-mode pyr-mode
  "Clronite repl interaction"
  nil
  :global nil
  :keymap pyr-map)

(add-hook 'lisp-mode-hook 'pyr-mode)

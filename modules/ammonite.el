(defvar amm-term-process nil
  "Process of the ammonite terminal.")

(defun amm-set-terminal ()
  (interactive)
  (setq amm-term-process
        (get-buffer-process (current-buffer))))

(defun amm-send-string (str)
  ;; Remove newlines from beginning and end of str
  (setq str (replace-regexp-in-string "\\`\n+" "" str))
  (setq str (replace-regexp-in-string "\n+\\'" "" str))

  ;; Put in brackets if multiple lines, then add newline to the end
  (setq str (if (and (string-match-p "\n" str)
                     (not (string= (substring str 0 1) "{")))
                (format "\n{\n%s\n}\n" str)
              (concat str "\n")))
  (term-send-string amm-term-process str))

(defun org-babel-execute:scala (body params)
  (amm-send-string body)
  nil)

(defun amm-send-buffer (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (amm-send-string (buffer-string))))

(defun amm-send-last-sexp ()
  (interactive)
  (let ((pos (point)))
    (backward-sexp)
    (amm-send-string
     (buffer-substring-no-properties
      (line-beginning-position) pos))
    (goto-char pos)))

(defun amm-send-region (beg end)
  (interactive (list (mark) (point)))
  (amm-send-string
   (buffer-substring-no-properties
    beg end)))

(defun amm-send-defun ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (amm-send-region
     (if (string-match-p "[ \t\n}]" (buffer-substring (point) (1+ (point))))
         (search-backward-regexp "^[^ \t\n]") (point))
     (progn (forward-char 1) (or (search-forward-regexp "^[^ \t\n].*" nil t)
                                 (line-end-position))))))

(defvar amm-default-buffers nil
  "Buffers to send to ammonite.")

(defun amm-add-default-buffer ()
  (interactive)
  (add-to-list 'amm-default-buffers (current-buffer))
  (message "%s" (mapcar 'buffer-name amm-default-buffers)))

(defun amm-remove-default-buffer ()
  (interactive)
  (setq amm-default-buffers (remove (current-buffer) amm-default-buffers))
  (message "%s" (mapcar 'buffer-name amm-default-buffers)))

(defun amm-send-default-buffers ()
  (interactive)
  (amm-send-string
   (format "{%s}\n" (mapconcat (lambda (b) (with-current-buffer b (buffer-string)))
                               amm-default-buffers "\n"))))

(defun amm-eval (&optional arg)
  (interactive "p")
  (cond (mark-active (amm-send-region (mark) (point)))
        ((eq arg 4) (amm-send-buffer))
        ((eq arg 16) (amm-send-default-buffers))
        (t (amm-send-last-sexp))))

(qv/keys amm-map
  :sparse t
  "C-e" amm-eval
  "C-M-x" amm-send-defun)

(define-minor-mode amm-mode
  "Ammonite repl interaction"
  nil
  :global nil
  :keymap amm-map)

(add-hook 'scala-mode-hook 'amm-mode)

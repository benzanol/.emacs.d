;;; Minibuffer Functions
;;;; Read File Name
(defun qv/find-file-up-directory (&optional force)
  (interactive)
  (ignore-errors
    (if (or force (string= (buffer-substring (1- (point)) (point)) "/"))
        (let ((current (expand-file-name (minibuffer-contents))))
          (delete-region (line-end-position) (line-beginning-position))
          (insert (replace-regexp-in-string
                   (expand-file-name "~") "~"
                   (file-name-directory (substring current 0 -1)))))
      (backward-delete-char 1))))

(setq qv/find-file-map (make-sparse-keymap))
(setq qv/find-file-mode nil)
(push (cons 'qv/find-file-mode qv/find-file-map) minor-mode-map-alist)
(qv/define-keys qv/find-file-map
  "DEL" qv/find-file-up-directory
  "<C-backspace>" (qv/find-file-up-directory 'force)
  "C-/" ((delete-region (line-beginning-position) (line-end-position)) (insert "/"))
  "C-`" ((delete-region (line-beginning-position) (line-end-position)) (insert "~/")))

(defun qv/read-file-advice (func &rest args)
  (let ((qv/find-file-mode t))
    (apply func args)))
(advice-add 'read-file-name :around 'qv/read-file-advice)


;;;; Read Unicode
;; Copied verbatim from counsel but to use default completion
(defun qv/insert-unicode-char ()
  (interactive)
  (completing-read 
   "Unicode Char: "
   (let* (cands
          (table (ucs-names))
          (fmt (lambda (name code)
                 (let ((cand (format "%06X %-58s %c" code name code)))
                   (put-text-property 0 1 'code code cand)
                   (push cand cands)))))
     (if (not (hash-table-p table))
         (dolist (entry table)
           (funcall fmt (car entry) (cdr entry)))
       (maphash fmt table)
       (setq cands (nreverse cands)))
     (sort cands #'string-lessp))))

;;; Swiper
(defun qv/swiper ()
  (interactive)
  (let ((lines '()))
    (-as-> nil %
           (buffer-substring (point-min) (point-max))
           (split-string % "\n")
           (mapcar (lambda (n)
                     (format "%s  %s"
                             (propertize (number-to-string n) 'face 'line-number)
                             (nth (1- n) %)))

                   (number-sequence 1 (length %)))
           (completing-read "Jump to Line: " % nil t)
           (split-string % "  ")
           (car %)
           (string-to-number %)
           (goto-line %))))

;;; End of Messages Buffer
(defun tail-f-msgs ()
  "Go to the enf of Messages buffer."
  (let ((msg-window (get-buffer-window "*Messages*")))
    (if msg-window
        (with-current-buffer (window-buffer msg-window)
          (set-window-point msg-window (point-max))))))

;; Make the Messages buffer stick to the end.
(add-hook 'post-command-hook 'tail-f-msgs)
;;; Install Nix Packages
(defun qv/nix-install ()
  (interactive)
  (shell-command
   (format "export NIXPKGS_ALLOW_UNFREE=1 ; nix-env -iA nixos.%s &"
           (prog1 (completing-read
                   "Install Package: "
                   (--map (or (ignore-errors (substring it 27 -4)) it)
                          (cdr (split-string
                                (shell-command-to-string
                                 "nix search | awk '/^\\*/{print $2}'")
                                "\n")))))
           (split-window nil nil 'left))))

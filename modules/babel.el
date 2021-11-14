;;; Indenting Code Blocks
(defun qv/format-code-block-indentation ()
  (interactive)
  (let ((case-fold-search t)
        (original-pos (cons (line-number-at-pos) (current-column)))
        (original-indentation (current-indentation))
        (mode nil) (text nil) (beg nil) (end nil))
    
    ;; Find the start and end of the source block
    (beginning-of-line)
    (search-forward-regexp "^[ 	]*#\\+END_SRC")
    (previous-line) (end-of-line) (setq end (point))
    (search-backward-regexp "^[ 	]*#\\+BEGIN_SRC[ \n]")
    
    ;; Figure out the language of the source block
    (setq mode (car (read-from-string
                     (concat (replace-regexp-in-string
                              "#\\+BEGIN_SRC \\([^ ]+\\).*" "\\1"
                              (buffer-substring-no-properties
                               (point) (save-excursion (end-of-line) (point))))
                             "-mode"))))

    ;; Save and delete the contents of the source block
    (next-line) (beginning-of-line) (setq beg (point))
    (setq text (buffer-substring-no-properties beg end))
    (delete-region beg end)

    ;; Insert the contents into a temporary buffer, indent it, and copy it
    (with-temp-buffer
      (if (commandp mode)
          (eval (list mode))
        (emacs-lisp-mode))
      (insert text)
      (indent-region (buffer-end -1) (buffer-end 1))
      (setq text (buffer-substring-no-properties (buffer-end -1) (buffer-end 1))))

    ;; Return to the original buffer and insert the indented text
    (insert text)
    (goto-line (car original-pos))
    (move-to-column (+ (cdr original-pos)
                       (current-indentation)
                       (- original-indentation)))))

;;; Tangle File Keys
(setq-default qv/tangle-file "")
(defun qv/change-tangle-file ()
  (interactive)
  (setq-default qv/tangle-file
                (format "./modules/%s.el"
                        (read-string "Tangle File: (.el) ./modules/"))))

(defun qv/put-tangle-file ()
  (interactive)
  (replace-string "#+BEGIN_SRC emacs-lisp\n"
                  (format "#+BEGIN_SRC emacs-lisp :tangle %s\n" qv/tangle-file)))

(global-set-key (kbd "C-c <f3>") 'qv/put-tangle-file)
(global-set-key (kbd "C-c <f2>") 'qv/change-tangle-file)

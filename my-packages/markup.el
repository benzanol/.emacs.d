
;;; Markup Language
;;;; Parsing Markup Language
(setq qv/markup-format-options
      '(("b" 0 (:weight 'bold))
        ("i" 0 (:slant 'italic))
        ("u" 0 (:underline t))
        ("u=\\(#[0-9A-Fa-f]\\{6\\}\\)" 1 (:underline (car matches)))
        ("c=\\(#[0-9A-Fa-f]\\{6\\}\\)" 1 (:foreground (car matches)))
        ("h=\\(#[0-9A-Fa-f]\\{6\\}\\)" 1 (:background (car matches)))
        )
      )
"Keywords for markup formatting"

;; Issue: match-beginning and match-end returning nil, had to use hack to get an approximate beginning and end
(defun qv/markup-format-tree ()
  (let* ((option-regexps (mapconcat 'car qv/markup-format-options "\\|\\\\"))
         (regexp (format "\\(\\(?:\\\\%s\\)+\\){\\(.*?\\)}" option-regexps))
         options face-spec formats f matches start end)
    (while (search-forward-regexp regexp nil 'noerror)
      (setq options (split-string (match-string 1) "\\\\" 'omit-nulls)
            start (match-beginning 2)
            end (match-end 2)
            face-spec nil)
      (dolist (s options)
        (setq formats qv/markup-format-options)
        (while (and formats (not (string-match-p (format "\\`%s\\'" (caar formats)) s)))
          (pop formats))
        (when formats
          (setq f (car formats)
                matches nil)
          (dotimes (i (cadr f))
            (add-to-list 'matches (replace-regexp-in-string (car f) (format "\\%s" (1+ i)) s)
                         'append 'ignore))
          (setq face-spec (append face-spec (eval (cons 'list (caddr f)))))))
      (put-text-property (1- (point)) (1+ (search-backward "{")) 'face face-spec)
      )
    ))

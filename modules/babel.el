(qv/package babel)

;;; Configuration
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-babel-load-languages '(python . t))
(setq org-babel-python-command "python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

;; Automiatcally update images after running source code
(add-hook 'org-babel-after-execute-hook
          'org-display-inline-images)

;; Set to a number to set pixel width
(setq org-image-actual-width t)

;;; Indenting Code Blocks
(defun qv/format-code-block-indentation ()
  (interactive)
  (let ((info (org-babel-get-src-block-info))
        (elem (cadr (org-element-at-point)))
        (pos (cons (line-number-at-pos) (current-column)))
        text)
    (with-current-buffer (format " *org-src-fontification:%s*" (org-src-get-lang-mode (car info)))
      (indent-region (point-min) (point-max))
      (setq text (buffer-string)))
    (delete-region (save-excursion (goto-char (plist-get elem :begin)) (line-end-position))
                   (save-excursion (goto-char (plist-get elem :end)) (forward-line -2) (point)))
    (insert text)
    (goto-line (car pos))
    (forward-char (cdr pos))))

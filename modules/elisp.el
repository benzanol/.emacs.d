;;; Font Lock Syntax
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("setq \\([^ ]+\\)" (1 font-lock-variable-name-face))
   ("(\\(lambda\\) (" (1 font-lock-variable-name-face))
   ("(\\(interactive\\))" (1 font-lock-constant-face))))

;;; Prettify Symbols
(qv/hook emacs-lisp-mode-hook qv/elisp-pretty-symbols
  (setq prettify-symbols-alist
        '(("lambda" . "λ")))
  (prettify-symbols-mode))

(qv/keys emacs-lisp-mode-map
  "C-e" (if mark-active (eval-region (min (point) (mark)) (max (point) (mark))) (eval-last-sexp nil))
  "C-r" (lambda (arg) (interactive "P")
          (if arg (qv/expand-replace) (qv/eval-replace)))
  "C-M-i" nil)

;;;; Eval at Point
(defun qv/eval-replace ()
  (interactive)
  (let ((output (eval-last-sexp nil))
        (pos (point))
        original-contents)
    (backward-sexp)
    (setq original-contents (buffer-substring pos (point)))
    (delete-region (point) pos)
    (setq pos (point))
    (insert (replace-regexp-in-string "\n$" "" (pp output)))
    (message "")))

(defun qv/expand-replace ()
  (interactive)
  (let ((marker (set-marker (make-marker) (point))))
    (backward-sexp)
    (insert "(macroexpand-1 '")
    (goto-char (marker-position marker))
    (insert ")")
    (qv/eval-replace)))

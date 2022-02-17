;;; Make *scratch* elisp
(with-current-buffer "*scratch*"
  (emacs-lisp-mode))

;;; Outline Mode
(qv/require outline)
(qv/hook emacs-lisp-mode-hook qv/elisp-outline
  (outline-minor-mode)
  (setq-local outline-regexp ";;;+ "))

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

;;; Eval at Point
(qv/keys emacs-lisp-mode-map
  "C-e" (if mark-active (eval-region (min (point) (mark)) (max (point) (mark))) (eval-last-sexp nil))
  "C-r" (lambda (arg) (interactive "P")
          (if arg (qv/expand-replace) (qv/eval-replace)))
  "C-M-i" nil)

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

;;; Flycheck
(qv/package flycheck)
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

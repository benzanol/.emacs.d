(bz/require rainbow)
(bz/require outline)
(bz/require hideshow)

;;; Keys
(bz/keys emacs-lisp-mode-map
  :sparse t)

;;; Eldoc
(global-eldoc-mode 1)

;;; Make scratch buffer elisp
(with-current-buffer "*scratch*"
  (emacs-lisp-mode))

;;; Outline mode
(bz/hook emacs-lisp-mode-hook bz/elisp-setup
  (setq-local outline-regexp ";;;+ ")
  (outline-minor-mode))

;;; Evaluating

(+ 1 2)

(defun bz/eval-last-sexp (arg)
  (interactive "P")
  (let* ((print-length nil) (print-depth nil)
         (beg (save-excursion (backward-sexp) (point)))
         (expr (read (buffer-substring-no-properties beg (point))))
         insert replaced)

    (pcase arg
      ('replace (setq insert (pp (eval expr lexical-binding))))
      ('expand (setq insert (pp (eval `(macroexpand-1 ',expr) lexical-binding))))
      ((or '(4) 'insert) (setq beg (point) insert (format "%s" (eval expr lexical-binding))))
      (_ (bz/eval expr lexical-binding)))

    (when insert
      (setq replaced (or (get-text-property beg 'bz/replaced) (buffer-substring beg (point))))
      (delete-region beg (point))
      (insert (propertize (string-trim insert) 'bz/replaced replaced)))))

(defun bz/eval-last-sexp-undo ()
  (interactive)
  (let* ((replaced (or (get-text-property (point) 'bz/replaced)
                       (progn (backward-char) (get-text-property (point) 'bz/replaced))
                       (progn (forward-char) (error "No output at point")))))
    (delete-region (and (text-property-search-forward 'bz/replaced) (point))
                   (and (text-property-search-backward 'bz/replaced) (point)))
    (insert replaced)))

(defun bz/eval-replace (arg)
  (interactive "P")
  (pcase arg
    ('(4) (bz/eval-last-sexp 'expand))
    ('(16) (bz/eval-last-sexp-undo))
    (_ (bz/eval-last-sexp 'replace))))
;;; Font lock keywords

(font-lock-add-keywords
 'emacs-lisp-mode
 `(;; Escape characters
   ("\\\\[\"\\nstu]" (0 'font-lock-keyword-face prepend))
   ;; Snippet indicators
   (,bz/snippet-indicator-regexp (0 'font-lock-constant-face prepend))))

;; (s-join "" (--filter (not (equal it (ignore-errors (read (concat "\"\\" it "\"")))))
;;                      (split-string (concat alphabet (upcase alphabet)) "" t)))
;; "abdefnrstuvxACHMNSU"

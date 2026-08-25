;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-functions)
(require 'bz-keys)
(require 'bz-rainbow)

(require 'eldoc)
(require 'erefactor)
(require 'text-property-search)


;;; Keys

(defvar bz/y nil)
(bz/advise :around y-or-n-p bz/y-or-n-p-advice (fn &rest args)
  (message "%s!" last-command)
  (or bz/y (eq last-command #'bz/y) (apply fn args)))

(bz/keys emacs-lisp-mode-map
  :sparse t
  "C-c r" erefactor-rename-symbol-in-buffer
  "C-c C-r" (@ bz/erefactor-rename-y
               (let* ((bz/y t))
                 (call-interactively #'erefactor-rename-symbol-in-buffer)))
  "C-c C-d" xref-find-definitions
  )


;;; Eldoc

(global-eldoc-mode 1)


;;; Make scratch buffer elisp

(with-current-buffer "*scratch*"
  (emacs-lisp-mode))


;;; Setup hook

(defun bz/format-buffer-on-save ()
  (when (bound-and-true-p apheleia-mode)
    (bz/format-buffer)))

;; For erefactor highlight mode
(setq idle-update-delay 0.1)

(bz/hook emacs-lisp-mode-hook bz/elisp-setup
  (setq-local outline-regexp ";;;+")
  (outline-minor-mode)
  (erefactor-highlight-mode 1)

  ;; (add-hook 'before-save-hook #'bz/format-buffer-on-save nil t)
  )


;;; Evaluating

(defun bz/eval-last-sexp (arg)
  (interactive "P")
  (let* ((print-level nil) (print-length nil)
         (debug-on-error t)
         (beg (set-marker (make-marker) (save-excursion (backward-sexp) (point))))
         (expr (read (buffer-substring-no-properties beg (point))))
         insert replaced)

    ;; When evaluating just a defvar, evaluate it just as setq
    (and (listp expr) (memq (car expr) '(defvar defvar-local defcustom))
         (setq expr `(let ((value ,(caddr expr)))
                       (,(car expr) ,(cadr expr) value . ,(cdddr expr))
                       (setq-default ,(cadr expr) value))))
    (and (listp expr) (eq (car expr) 'defface)
         (setq expr `(progn ,expr (face-spec-set ',(cadr expr) ,(caddr expr)))))

    (pcase arg
      ('replace (setq insert (pp (eval expr lexical-binding))))
      ('expand (setq insert (pp (eval `(macroexpand-1 ',expr) lexical-binding))))
      ((or '(4) 'insert) (setq beg (set-marker (make-marker) (point))
                               insert (let ((res (eval expr lexical-binding)))
                                        (if (stringp res) res (cl-prin1-to-string res)))))
      ((or '(16) 'ignore) (bz/eval (prin1-to-string (list #'ignore expr)) lexical-binding))
      ;; ('(16) (eval-buffer))
      (_ (bz/eval (prin1-to-string expr) lexical-binding)))

    (when insert
      (with-current-buffer (marker-buffer beg)
        (setq replaced (or (get-text-property beg 'bz/replaced) (buffer-substring beg (point))))
        (delete-region beg (point))
        (insert (propertize (string-trim insert) 'bz/replaced replaced))))))

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

(bz/face font-lock-regexp-grouping-backslash :weight normal :fg orange)
(bz/face font-lock-regexp-grouping-construct :weight normal :fg orange)

(bz/face bz/outline-heading1 :fg gray2 :w bold)
(bz/face bz/outline-heading2 :fg gray1 :w bold)
(bz/face bz/outline-heading3 :fg outline3 :w bold)
(bz/face bz/outline-heading4 :fg outline4 :w bold)

(bz/face bz/elisp-symbol :fg orange :w bold)
(bz/face bz/elisp-et-type :fg cyan :w normal)
(bz/face bz/elisp-et-struct :fg yellow :w normal)
(bz/face bz/elisp-et-at :fg purple :w bold)
(bz/face bz/elisp-et-dollar :fg green :s italic :w normal)

(bz/face bz/elisp-test-macro :fg red :w bold)

(bz/font-lock-add-keywords
 'emacs-lisp-mode
 `(;; Escape characters
   ("\\\\[\"\\nstur]" (0 'font-lock-keyword-face prepend))
   ;; Snippet indicators (for snippet definitions)
   (,bz/snippet-indicator-regexp (0 'font-lock-constant-face prepend))
   ;; Outline headings
   ("^;;; \\(.*\\)" (1 'bz/outline-heading1 t))
   ("^;;;;\\(?:;;;\\)* \\(.*\\)" (1 'bz/outline-heading2 t))
   ("^;;;;;\\(?:;;;\\)* \\(.*\\)" (1 'bz/outline-heading3 t))
   ("^;;;;;;\\(?:;;;\\)* \\(.*\\)" (1 'bz/outline-heading4 t))

   ;; Highlight et types
   ("\\('\\|\\<\\)[A-Z]+[a-z0-9][a-z0-9A-Z:]+\\>" (0 'bz/elisp-et-type append))
   ;; Highlights et struct names
   ("\\(\\*[-a-z0-9:]+\\)" (0 'bz/elisp-et-struct))
   ;; Highlight et @ directives
   ("(\\(@[-a-z0-9]+\\)" (0 'bz/elisp-et-at))
   ("(\\($[-a-z0-9]+\\)" (0 'bz/elisp-et-dollar))

   ;; Highlight all caps symbols orange
   ("\\('\\|\\<\\)\\([A-Z:-]+\\)\\>" (0 'bz/elisp-symbol append))

   ;; Test macros
   ("(\\(et-assert\\(?:-[-a-zA-Z]+\\)?\\)[ \n]" (1 'bz/elisp-test-macro prepend))
   ("(\\(et-test\\(?:-[-a-zA-Z]+\\)?\\)[ \n]" (1 'bz/elisp-test-macro prepend))))


;; (s-join "" (--filter (not (equal it (ignore-errors (read (concat "\"\\" it "\"")))))
;;                      (split-string (concat alphabet (upcase alphabet)) "" t)))
;; "abdefnrstuvxACHMNSU"


;;; Provide

(provide 'bz-elisp)

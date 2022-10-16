(defvar qv/latex-enabled t)
(defun qv/toggle-latex (&optional arg)
  (interactive)
  (setq-local qv/latex-enabled
              (if (numberp arg)
                  (if (eq arg 0) qv/latex-enabled
                    (if (< arg 0) nil t))
                (not qv/latex-enabled)))
  (if qv/latex-enabled
      (org-latex-preview '(16))
    (org-latex-preview '(64))))

(qv/package org-fragtog)

(qv/keys org-mode-map
  "C-c C-l" nil
  "C-c C-l C-l" ((org-latex-preview '(16)) (org-fragtog-mode 0))
  "C-c C-l C-o" ((org-latex-preview '(64)) (org-fragtog-mode 0))
  "C-c C-l C-f" ((org-latex-preview '(16)) (org-fragtog-mode 1))
  "C-c C-l C-t" qv/toggle-latex
  "M-|" ((insert "\\(  \\)") (backward-char 3)))

(plist-put org-format-latex-options :scale 1.75)


(setq qv/latex-abbrev-alist
      '(("." . "rightarrow")
        ("," . "leftarrow")
        ("g a" . "alpha")
        ("g b" . "beta")
        ("g g" . "gamma")
        ("g s" . "sigma")
        ("g t" . "tau")
        ("g r" . "rho")
        ("B" . "mathbb{}")))

(define-key org-mode-map (kbd "M-\\") nil)
(dolist (a qv/latex-abbrev-alist)
  (define-key org-mode-map (kbd (concat "M-\\ " (car a)))
    `(lambda () (interactive)
       (insert (concat "\\" ,(cdr a)))
       . ,(when (string= "}" (substring (cdr a) -1))
            '((forward-char -1))
            ))))

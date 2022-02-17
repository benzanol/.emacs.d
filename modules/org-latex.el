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

(qv/hook org-mode-hook qv/setup-org-latex
  (org-fragtog-mode 1))

(qv/keys org-mode-map
  "C-c C-l" qv/toggle-latex
  "M-\\" ((insert "\\(  \\)") (backward-char 3)))

(plist-put org-format-latex-options :scale 1.2)

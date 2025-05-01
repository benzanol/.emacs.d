;;; Settings

(setq
 org-export-dispatch-use-expert-ui t ; Not to brag but Im kind of an expert

 org-export-preserve-breaks t
 org-export-with-title t
 org-export-with-author nil
 org-export-with-date nil
 org-export-with-toc nil
 org-export-with-section-numbers nil


 org-export-with-todo-keywords nil
 org-export-with-tasks t
 org-export-with-priority nil
 )


;;; Headers

(let ((article-entry (assoc "article" org-latex-classes)))
  (unless (boundp 'bz/original-latex-header)
    (setq bz/original-latex-header (cadr article-entry)))

  (setf (cadr article-entry)
        (concat bz/original-latex-header
                "\n\\usepackage[margin=1in]{geometry}"
                "\n\\setlength{\\parindent}{0pt}"
                ))
  )


;; Export only visible by default
;; (bz/advise :filter-args org-export--dispatch-ui bz/oedu-filter-args (args)
;;   (unless (memq 'bz/set-visible (car args))
;;     (setcar args (cons 'visible (cons 'bz/set-visible (car args)))))
;;   args)


;; (setq org-latex-pdf-process (car (helpful--original-value 'org-latex-pdf-process)))

;;; Remove newlines before align blocks
(let*  ((nextline "\\(\\\\begin{\\|%% nonewline\\)")
        (cmd (format "sed -zi 's/\\\\\\\\\\(\\n\\+%s\\)/\\1/g' %%b.tex" nextline)))
  (add-to-list 'org-latex-pdf-process cmd nil #'string=))

;; (setq org-latex-pdf-process (car (helpful--original-value 'org-latex-pdf-process)))


;;; Get rid of the tex files when done
(ignore-errors (mkdir "/tmp/ox-tex"))
(add-to-list 'org-latex-pdf-process "mv %b.tex /tmp/ox-tex" 'append #'string=)


;;; Smaller margins
;; (add-to-list 'org-latex-packages-alist '("" "fullpage"))
;; (add-to-list 'org-latex-packages-alist '("" "pgfplots"))

;; To disable indent, add latex header:
;; \\setlength{\\parindent}{0pt}

;; To make each section start a new page, add the following header:
;; \let\stdsection\section
;; \renewcommand\section{\newpage\stdsection}
;;; Box Filter
(add-hook
 'org-export-filter-special-block-functions
 (defun bz/org-latex-box-filter (text backend info)
   (let ((before "\\noindent\\fbox{\\parbox{\\textwidth}{")
         (after "}}\\\\ \\\\")
         str)
     (when (string-match "\\`\\\\begin{box}\\([^1-0]*\\)\n\\\\end{box}\n+\\'" text)
       (setq str (match-string 1 text)
             str (replace-regexp-in-string "\\\\\\\\\\(\n*\\)\\'" "\\1" str))
       (format"\n%s\n%s\n%s\n" before str after)))))

(add-hook
 'org-export-filter-special-block-functions
 (defun bz/org-latex-proof-filter (text backend info)
   (let ((before "\\noindent\\fbox{\\parbox{\\textwidth}{")
         (after "}}\\\\ \\\\")
         (then "\\\\ \n\\emph{Proof}")
         str)
     (when (string-match "\\`\\\\begin{proof}\\([^1-0]*\\)\n\\\\end{proof}\n+\\'" text)
       (setq str (match-string 1 text)
             str (replace-regexp-in-string "\\\\\\\\\\(\n*\\)\\'" "\\1" str))
       (format"\n%s%s\n%s\n%s\n" before then str after)))))

(add-hook
 'org-export-filter-special-block-functions
 (defun bz/org-latex-def-filter (text backend info)
   (let ((before "\\noindent\\fbox{\\parbox{\\textwidth}{")
         (after "}}\\\\ \\\\")
         str)
     (message text)
     (when (string-match "\\`\\\\begin{definition}\\([^1-0]*\\)\n\\\\end{definition}\n+\\'" text)
       (setq str (match-string 1 text)
             str (replace-regexp-in-string "\\\\\\\\\\(\n*\\)\\'" "\\1" str))
       (format"\n%s\n%s\n%s\n" before str after)))))

(add-hook
 'org-export-filter-special-block-functions
 (defun bz/org-latex-theorem-filter (text backend info)
   (let ((before "\\noindent\\fbox{\\parbox{\\textwidth}{")
         (after "}}\\\\")
         (then "\\\\ \n\\textbf{Theorem}")
         str)
     (when (string-match "\\`\\\\begin{theorem}\\([^1-0]*\\)\n\\\\end{theorem}\n+\\'" text)
       (setq str (match-string 1 text)
             str (replace-regexp-in-string "\\\\\\\\\\(\n*\\)\\'" "\\1" str))
       (format"\n%s%s\n%s\n%s\n" before then str after)))))

(flycheck-add-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)








(qv/face qv/jsdoc-param :fg orange :w bold :slant normal)
(qv/face qv/jsdoc-hints :fg gray2 :w bold :h 0.9)


(setq qv/jsdoc-already-displayed nil)
(qv/hook pre-command-hook qv/jsdoc-reset-displayed
  (setq qv/jsdoc-already-displayed nil))

(defun qv/jsdoc-display-types ()
  (interactive)

  (remove-overlays nil nil 'jsdoc t)

  (beginning-of-buffer)

  (while (search-forward-regexp "\\<\\(function *[^) \n]+\\|constructor\\) *(" nil t)
    (let ((function (point))
          (end (save-excursion (backward-char) (forward-sexp) (point)))
          params return ol)

      ;; Search for a documentation comment above the function
      (forward-line -1)
      (if (not (comment-beginning)) (forward-line 2)

        ;; Find the return
        (when (search-forward-regexp "@returns? {\\(.+\\)}" function t)
          (setq return (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

        ;; Find the params
        (comment-beginning)
        (while (search-forward-regexp "@param {\\(.+\\)} *\\([^ \n]+\\)" function t)
          (setq ol (make-overlay (match-beginning 2) (match-end 2) nil nil t))
          (overlay-put ol 'jsdoc t)
          (overlay-put ol 'evaporate t)
          (push (list (buffer-substring-no-properties (match-beginning 2) (match-end 2))
                      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                      ol)
                params))

        ;; Add the return to the function
        (goto-char end)
        (when return
          (setq ol (make-overlay (point) (point)))
          (overlay-put ol 'jsdoc t)
          (overlay-put ol 'evaporate t)
          (overlay-put ol 'after-string (propertize (concat " :" return) 'face 'qv/jsdoc-hints)))

        ;; Add params to the function
        (goto-char function)
        (while (search-forward-regexp "[^][(){}, \n]+" end t)
          (-when-let* ((b (match-beginning 0)) (e (point))
                       (assoc (assoc (buffer-substring b e) params))
                       (other (nth 2 assoc))
                       (text (propertize (concat " :" (cadr assoc)) 'face 'qv/jsdoc-hints))
                       (ol (make-overlay b e nil nil t)))
            (overlay-put ol 'jsdoc t)
            (overlay-put ol 'after-string text)
            (overlay-put ol 'evaporate t)

            ;; Editing one edits the other
            (overlay-put ol 'other other)
            (overlay-put ol 'modification-hooks '(qv/jsdoc-modification-hook))
            (overlay-put ol 'insert-behind-hooks '(qv/jsdoc-modification-hook))
            (overlay-put ol 'insert-in-front-hooks '(qv/jsdoc-modification-hook))
            (overlay-put other 'other ol)
            (overlay-put other 'modification-hooks '(qv/jsdoc-modification-hook))
            (overlay-put other 'insert-behind-hooks '(qv/jsdoc-modification-hook))
            (overlay-put other 'insert-in-front-hooks '(qv/jsdoc-modification-hook))))))))

(defun qv/jsdoc-modification-hook (ol after &rest args)
  (when after
    (let* ((other (overlay-get ol 'other))
           (start (overlay-start other)))

      (when (memq (char-after (1- (overlay-end ol))) '(?, ?\s ?\n ?\)))
        (move-overlay ol (overlay-start ol) (1- (overlay-end ol))))

      (save-excursion
        (goto-char start)
        (delete-region (point) (overlay-end other))
        (insert (buffer-substring (overlay-start ol) (overlay-end ol)))
        (move-overlay other start (point))))))

(font-lock-add-keywords
 'js-mode
 '(;; Overlay types inside function signature
   ("@param\\|@returns?"
    0
    (prog1 'qv/jsdoc-param
      (unless qv/jsdoc-already-displayed
        (setq qv/jsdoc-already-displayed t)
        (save-excursion (qv/jsdoc-display-types))))
    prepend)))




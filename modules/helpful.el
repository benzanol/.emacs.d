(qv/package helpful)

(qv/keys *
  "C-h f" helpful-callable
  "C-h v" helpful-variable)

(qv/hook helpful-mode-hook qv/helpful-keybindings
  (qv/keys helpful-mode-map
    :sparse t
    :parent special-mode-map
    [remap revert-buffer] helpful-update
    "TAB" forward-button
    "<backtab>" backward-button))

;;; Make buffer variable pitch except for code
(advice-add 'helpful-update :after 'qv/helpful-code-overlay)
(defun qv/helpful-code-overlay ()
  (let ((inhibit-read-only t))
    (variable-pitch-mode 1)
    (save-excursion
      (beginning-of-buffer)

      (when (search-forward-regexp "^Signature$" nil t)
        (forward-line 1)
        (add-face-text-property (point) (line-end-position) 'fixed-pitch t))

      (when-let ((beg (search-forward-regexp "^References$" nil t))
                 (end (search-forward-regexp "^Find all references" nil t)))
        (goto-char beg) (forward-line 2)
        (setq beg (point))
        (goto-char end) (forward-line -1)
        (add-face-text-property beg (point) 'fixed-pitch t))

      
      (when-let ((beg (search-forward-regexp "^\\(Alias \\)?Source Code\n" nil t))
                 (end (search-forward-regexp "^Symbol Properties$\\|\\'" nil t)))
        (forward-line -1)
        (add-face-text-property beg (1- (point)) 'fixed-pitch t)
        (lisp-indent-region beg (1- (point))))

      (when (search-forward-regexp "^Symbol Properties\n" nil t)
        (add-face-text-property (point) (point-max) 'fixed-pitch t)))))


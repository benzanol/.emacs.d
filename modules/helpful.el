(bz/package helpful)

(bz/hook helpful-mode-hook bz/helpful-setup
  (bz/keys helpful-mode-map
    :sparse t
    :parent special-mode-map
    [remap bz/replace-char] helpful-update
    "TAB" forward-button
    "<backtab>" backward-button))

;;; Make buffer vaiable pitch except for code
(bz/advise :after helpful-update bz/helpful-code-overlay ()
  (flyspell-mode 0)
  (let ((inhibit-read-only t))
    (variable-pitch-mode 1)
    (save-excursion
      (beginning-of-buffer)

      (when (search-forward-regexp "^Signature$" nil t)
        (forward-line 1)
        (add-face-text-property (point) (line-end-position) 'fixed-pitch t))

      (while (search-forward-regexp "^Value$\\|^Original Value$" nil t)
        (forward-line 1)
        (overlay-put (make-overlay (point) (- (search-forward "\n\n") 2)) 'face 'fixed-pitch))

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
        (ignore-errors (lisp-indent-region beg (1- (point)))))

      (when (search-forward-regexp "^Symbol Properties\n" nil t)
        (add-face-text-property (point) (point-max) 'fixed-pitch t)))))

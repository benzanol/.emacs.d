(qv/package helpful)

(qv/keys *
  "C-h f" helpful-callable
  "C-h v" helpful-variable)

(qv/hook helpful-mode-hook qv/helpful-keybindings
  (qv/define-keys helpful-mode-map
    :sparse t
    :parent special-mode-map
    [remap revert-buffer] helpful-update))

(advice-add 'helpful-update :after 'qv/helpful-code-overlay)
(defun qv/helpful-code-overlay ()
  (save-excursion
    (beginning-of-buffer)

    (ignore-errors
      (search-forward-regexp "^Signature$")
      (next-line) (beginning-of-line)
      (overlay-put
       (make-overlay (point) (line-end-position))
       'face 'fixed-pitch))

    (ignore-errors
      (search-forward-regexp "^References$")
      (next-line 2) (beginning-of-line)
      (overlay-put
       (make-overlay (point) (search-forward-regexp "^$"))
       'face 'fixed-pitch))

    (ignore-errors
      (search-forward-regexp "^Source Code$")
      (next-line) (beginning-of-line)
      (overlay-put
       (make-overlay
        (point) (progn (search-forward-regexp "^Symbol Properties$\\|\\'")
                       (previous-line 2) (end-of-line) (point)))
       'face 'fixed-pitch))

    (ignore-errors
      (next-line 3) (beginning-of-line)
      (overlay-put (make-overlay (point) (point-max))
                   'face 'fixed-pitch))))

(bz/package eglot)

(defun bz/eglot-click ()
  (interactive)
  (if (frame-visible-p (lsp-ui-doc--get-frame)) (lsp-ui-doc-focus-frame)

    (if-let ((diag (bz/flymake-diagnostic-at-point)))
        (bz/eglot-show-hover-info (propertize (flymake--diag-text diag) 'face 'error))
      (eglot-hover-eldoc-function #'bz/eglot-show-hover-info))))

(defun bz/flymake-diagnostic-at-point ()
  (->> (overlays-at (point))
       (--map (overlay-get it 'flymake-diagnostic))
       (--first it)))


(defun bz/eglot-show-hover-info (text &rest info)
  (if (null text) (message "Nothing!")
    (lsp-ui-doc--display nil text)
    (add-hook 'pre-command-hook #'bz/eglot-remove-posframe)))

(defun bz/eglot-remove-posframe ()
  (remove-hook 'pre-command-hook #'bz/eglot-remove-posframe)
  (unless (eq this-command #'bz/eglot-click)
    (setq bz/eglot-info-showing nil)
    (lsp-ui-doc-hide)))


(bz/keys eglot-mode-map
  [remap bz/q] bz/eglot-click
  "C-c C-a" eglot-code-actions
  "C-c C-j" flymake-goto-next-error
  "C-c C-k" flymake-goto-prev-error
  )

(bz/keys lsp-ui-doc-frame-mode-map
  "q" (@ bz/lsp-ui-doc-exit (select-frame (frame-parent)) (lsp-ui-doc-hide))
  )

(bz/face flymake-error :u "red")
(bz/face flymake-warning :u "orange")
(bz/face eglot-highlight-symbol-face :w bold :s italic)

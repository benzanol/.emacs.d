(bz/hook flycheck-after-syntax-check-hook bz/flycheck-update-lsp-dired :remove
  (let ((dired (bz/activity-get :dired)))
    (when (buffer-live-p dired)
      (with-current-buffer dired
        (when lsp-dired-mode (dired-revert))))))

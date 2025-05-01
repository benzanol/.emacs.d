(bz/package undo-tree)

;; Enable in all buffers
(bz/hook buffer-list-update-hook bz/undo-tree-mode
  (with-current-buffer (car (buffer-list))
    (unless (derived-mode-p 'vterm-mode 'dired-mode)
      (undo-tree-mode 1))))

(bz/hook find-file-hook undo-tree-mode)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history"))
      undo-tree-auto-save-history nil)

;; (bz/hook undo-tree-mode-hook bz/undo-tree-load-history
;;   :remove
;;   (when undo-tree-mode
;;     (undo-tree-load-history nil 'noerror)))


(bz/keys undo-tree-map
  :sparse t
  "C-x C-u" bz/undo-tree-visualize
  [remap undo] undo-tree-undo
  [remap redo] undo-tree-redo
  "C-_" nil
  )


(defun bz/undo-tree-move-branch (delta)
  (let ((count (undo-tree-num-branches))
        (current (undo-tree-node-branch (undo-tree-current buffer-undo-tree))))
    (when (<= count 1) (error "Not at undo branch point!"))
    (undo-tree-switch-branch (mod (+ current delta) count))))

(defun bz/undo-tree-visualize ()
  (interactive)
  (let* ((buf (current-buffer))
         (layout (current-window-configuration))
         (hook `(lambda () (run-with-timer 0 nil #'set-window-configuration ',layout))))
    (undo-tree-visualize)
    (delete-other-windows)
    (select-window (prog1 (split-window-right) (switch-to-buffer buf)))
    (add-hook 'kill-buffer-hook hook nil 'local)))


(bz/advise :around switch-to-buffer-other-window
           bz/switch-to-buffer-other-window (func buffer-or-name &optional norecord)
  (let ((existing (get-buffer-window buffer-or-name)))
    (if (not existing) (funcall func buffer-or-name norecord)
      (select-window existing)
      (current-buffer))))



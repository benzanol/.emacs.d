(qv/package undo-tree)

;; Only enable in buffers associated with a file
(qv/hook buffer-list-update-hook qv/undo-tree-mode
  (with-current-buffer (car (buffer-list))
    (when (and (buffer-file-name) (not undo-tree-mode))
      (undo-tree-mode 1))))

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history"))
      undo-tree-auto-save-history t)

(qv/keys undo-tree-map
  :sparse t
  "u" undo-tree-undo
  "U" undo-tree-redo)

;; Large files won't save when undo tree is active, so disable it temporarily
(defun qv/with-undo-tree-disabled (func &rest args)
  "Around advice to run a function with undo tree disabled"
  (undo-tree-mode 0)
  (apply func args)
  (undo-tree-mode 1))

;; This makes undo tree not remember past version
;; (advice-add 'save-buffer :around 'qv/with-undo-tree-disabled)

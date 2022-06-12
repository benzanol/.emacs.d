(qv/package magit)
(qv/package dash)

(qv/hook magit-mode-hook nil
  (undo-tree-mode 0)
  (variable-pitch-mode 1))

;; When commiting, jump right to commit message
(setq magit-commit-show-diff nil)

;; Don't ask to save before refresh
(remove-hook 'magit-pre-refresh-hook 'magit-maybe-save-repository-buffers)
(defun magit-maybe-save-repository-buffers () nil)

;;; Faces
(qv/face magit-section-highlight :bg bg2)
(qv/face magit-diff-context-highlight fixed-pitch :bg bg2)

(qv/face magit-section-heading :h 1.2 :fg fg :w bold)
(qv/face magit-hash fixed-pitch)
(qv/face magit-branch-current fixed-pitch :h 0.95 :fg blue :w bold)
(qv/face magit-branch-local fixed-pitch :h 0.95 :fg blue :w bold)
(qv/face magit-section-child-count fixed-pitch :fg gray2)

(qv/face qv/magit-diff fixed-pitch)

(dolist (f (--filter (ignore-errors (equal (substring (symbol-name it) 0 11) "magit-diff-"))
                     (face-list)))
  (set-face-attribute
   f nil :inherit
   (let ((attr (face-attribute f :inherit)))
     (funcall (if (listp attr) 'cons 'list) 'qv/magit-diff attr))))

(qv/face magit-diff-file-heading nil :w normal :s italic)
(qv/face magit-diff-file-heading-highlight magit-section-highlight)

;;; Keybindings
(qv/keys magit-status-mode-map
  "j" nil "k" nil "h" nil "l" nil
  "J" nil "K" nil "H" nil "L" nil
  "g" nil
  "M-w" nil
  "r" magit-refresh
  "R" magit-refresh-all
  "G" magit-checkout
  "SPC" (@ qv/magit-section-toggle
           (ignore-errors (next-line) (goto-char (1- (line-beginning-position)))
                          (beginning-of-line))
           (call-interactively 'magit-section-toggle)))

;; ghp_TT13qgRNGS1VsEQRjsv4qxYcxZqgnB3txb

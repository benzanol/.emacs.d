(bz/package magit)
(bz/package dash)

;; Config
;; git config --global user.name "Adam Tillou"
;; git config --global user.email "adam.tillou@gmail.com"

(setq magit-display-buffer-function (lambda (b) (display-buffer-same-window b nil)))

(bz/hook magit-mode-hook bz/magit-variable-pitch
  (undo-tree-mode 0)
  (unless (eq major-mode 'magit-log-mode)
    (variable-pitch-mode 1)))

;; When commiting, jump right to commit message
(setq magit-commit-show-diff nil)

;; Don't ask to save before refresh
(remove-hook 'magit-pre-refresh-hook 'magit-maybe-save-repository-buffers)
(defun magit-maybe-save-repository-buffers () nil)

;;; Faces
(bz/face magit-section-highlight :bg bg2)
(bz/face magit-diff-context-highlight fixed-pitch :bg bg2)

(bz/face magit-section-heading :h 1.2 :fg fg :w bold)
(bz/face magit-hash fixed-pitch)
(bz/face magit-branch-current fixed-pitch :h 0.95 :fg blue :w bold)
(bz/face magit-branch-local fixed-pitch :h 0.95 :fg blue :w bold)
(bz/face magit-section-child-count fixed-pitch :fg gray2)

(bz/face bz/magit-diff fixed-pitch)

(dolist (f (--filter (ignore-errors (equal (substring (symbol-name it) 0 11) "magit-diff-"))
                     (face-list)))
  (set-face-attribute
   f nil :inherit
   (let ((attr (face-attribute f :inherit)))
     (funcall (if (listp attr) 'cons 'list) 'bz/magit-diff attr))))

(bz/face magit-diff-file-heading nil :w normal :s italic)
(bz/face magit-diff-file-heading-highlight magit-section-highlight)

(load-file "~/.githubtoken.el")
(bz/key * "C-x C-p C-p" (insert (getenv "GITHUB_TOKEN")))

;;; Keybindings
(bz/keys magit-diff-mode-map
  :sparse t
  :parent magit-status-mode-map)

(bz/keys magit-status-mode-map
  "j" nil "k" nil "h" nil "l" nil
  "J" nil "K" nil "H" nil "L" nil
  "g" nil
  "M-w" nil
  "M-1" nil "M-2" nil "M-3" nil "M-4" nil

  "f" magit-find-file
  "r" magit-refresh
  "R" magit-refresh-all
  "G" magit-checkout
  "d" magit-discard
  "SPC" (@ bz/magit-section-toggle
           (ignore-errors (next-line) (goto-char (1- (line-beginning-position)))
                          (beginning-of-line))
           (call-interactively 'magit-section-toggle)))

;;; Open in posframe
(bz/advise :remove magit-commit-create bz/exwm-posframe)

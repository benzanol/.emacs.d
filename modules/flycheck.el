(bz/package flycheck)

(bz/keys flycheck-mode-map
  "C-c C-e" bz/flycheck-posframe-show
  "C-c C-p" flycheck-explain-error-at-point
  "C-c C-j" flycheck-next-error
  "C-c C-k" flycheck-previous-error)

(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

;; Disable hints
(setq flycheck-error-list-minimum-level 'warning)
(setq flycheck-navigation-minimum-level 'warning)
(bz/advise :around flycheck-error-level-make-indicator
           bz/flycheck-error-level-advise (old level side &optional cont)
  (unless (eq level 'info)
    (funcall old level side cont)))


;;; When to update
(setq-default flycheck-check-syntax-automatically
              '(idle-buffer-switch mode-enabled idle-change new-line))

(bz/hook flycheck-mode-hook bz/flycheck-mode-setup
  (if flycheck-mode
      (add-hook 'after-save-hook 'flycheck-buffer nil t)
    (remove-hook 'after-save-hook 'flycheck-buffer t)))

(bz/hook (typescript-mode-hook js-mode-hook) bz/flycheck-disable-idle-check :remove
  (setq-local flycheck-check-syntax-automatically '(idle-buffer-switch mode-enabled)))

;; (setq flycheck-idle-change-delay 10)




;;; Flycheck posframe
(bz/package flycheck-posframe)

(bz/face flycheck-posframe-background-face :bg bg3)
(bz/face flycheck-posframe-border-face :fg "gray50")
(bz/face flycheck-posframe-error-face nil)
(bz/face flycheck-posframe-warning-face nil)
(setq flycheck-posframe-border-width 1)

;; Manually show the posframe
(defun bz/flycheck-posframe-show ()
  (interactive)
  (flycheck-posframe-show-posframe (flycheck-overlay-errors-at (point)))
  (add-hook 'pre-command-hook #'bz/hide-flycheck-posframe))

;; Hide the posframe immediately before the next command
(defun bz/hide-flycheck-posframe ()
  (remove-hook 'pre-command-hook #'bz/hide-flycheck-posframe)
  (posframe-hide flycheck-posframe-buffer))

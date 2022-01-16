(load-file "~/.emacs.d/modules/base.el")

(setq initial-scratch-message (concat initial-scratch-message "(qv/load-window-manager)"))

(qv/require faces)
(qv/require settings)
(qv/require keybindings)
(qv/require recenter)
(qv/require undotree)
(qv/require rectangle)

(defun qv/load-window-manager ()
  (interactive)
  (qv/require exwm)
  (qv/require system)
  (qv/require minibuffer)
  (qv/require ivy)
  (qv/require activities)
  (qv/require dired)
  (qv/require terminal)
  (server-start))

(qv/hook emacs-lisp-mode-hook nil
  (qv/require elisp)
  (qv/require rainbow)
  (qv/require outline))

(qv/hook custom-mode-hook nil
  (qv/require custom))

(eval-after-load 'dired '(qv/require dired))
(eval-after-load 'helpful '(qv/require helpful))
(eval-after-load 'magit '(qv/require magit))

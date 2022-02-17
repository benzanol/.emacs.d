(load-file "~/.emacs.d/modules/base.el")

(qv/require faces)
(qv/require settings)
(qv/require keybindings)
(qv/require recenter)
(qv/require undotree)
(qv/require rectangle)

(qv/package doom-modeline)
(doom-modeline-mode)

(defun qv/load-window-manager ()
  (interactive)
  (qv/require exwm)
  (qv/require system)
  (qv/require minibuffer)
  (qv/require activities)
  (qv/require dired)
  (qv/require terminal)
  (qv/require multicursors)
  (server-start))

(qv/hook emacs-lisp-mode-hook nil
  (qv/require elisp)
  (qv/require rainbow)
  (qv/require outline))

(qv/hook custom-mode-hook nil
  (qv/require custom))

(qv/after org)
(qv/after dired)
(qv/after helpful)
(qv/after magit)
(qv/after custom)
(qv/after eww)
(qv/after rect rectangle)

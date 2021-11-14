(load-file "~/.emacs.d/modules/base.el")

(qv/module settings)
(qv/module keybindings)
(qv/module faces)
(qv/module recenter)
(qv/module undotree)

(when (display-graphic-p)
  (qv/module gui))

(defun qv/load-window-manager ()
  (interactive)
  (qv/module exwm)
  (qv/module system)
  (qv/module minibuffer)
  (qv/module activities))


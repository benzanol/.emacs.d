(load-file "~/.emacs.d/modules/base.el")

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
  (qv/require activities)
  (server-start))

(qv/hook emacs-lisp-mode-hook nil
  (qv/require elisp)
  (qv/require rainbow)
  (qv/require outline))

(qv/hook custom-mode-map nil
  (qv/require custom))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit helpful emms start-menu company eldoc-overlay doom-modeline multiple-cursors rainbow-delimiters multi-term embark marginalia exwm dired-subtree all-the-icons-dired all-the-icons orderless prescient vertico undo-tree dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-button ((t (:inherit mode-line :box (:line-width 1 :color "#505860")))))
 '(variable-pitch ((t (:family "Attractive")))))

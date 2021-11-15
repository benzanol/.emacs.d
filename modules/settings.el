;;; Shell Mode
(qv/hook shell-mode-hook qv/shell-mode-setup
  (buffer-face-set 'fixed-pitch))

;;; Disable C-z
(global-unset-key (kbd "C-z"))

;;; Disable Cliking Minibuffer
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;;; Y or N
(advice-add 'yes-or-no-p :override 'y-or-n-p)

;;; Remove GUI Elements
(setq-default inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;;; Show Time
(setq display-time-format "%h %d  %H:%M |")
(display-time-mode 1)

;;; Theme Directory
(setq custom-theme-directory "~/.emacs.d/themes")

;;; Wrapping lines
(setq-default truncate-lines t)

;;; Also Quit Minibuffer
(advice-add 'keyboard-quit :before 'minibuffer-keyboard-quit)

;;; Move to Trash
(setq-default delete-by-moving-to-trash t)

;;; Auto Saves
(setq-default auto-save-default t)
(setq-default make-backup-files nil)
(defun make-auto-save-file-name ()
  (concat (expand-file-name "~/.emacs.d/auto-save-list/")
          (replace-regexp-in-string "/" "!" buffer-file-name)))
(setq create-lockfiles nil)

;;; Indents
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Line Numbers
(global-display-line-numbers-mode t)

;;; Relative Window Size
(setq window-combination-resize t)

;;; Recursive Minibuffer
(setq enable-recursive-minibuffers nil)

;;; Stretch Cursor
(setq-default x-stretch-cursor nil)

;;; Fringe Width
(set-fringe-mode 10)

;;; Display Buffer
(setq display-buffer-base-action '(display-buffer-same-window ()))
;;(setq display-buffer-base-action '(display-buffer-below-selected ()))

(setq display-buffer-alist
      '(("*Calendar*" . (display-buffer-at-bottom ()))))

;;; Visual Line Mode
(visual-line-mode 1)

;;; Case Insensitive Completions
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      bookmark-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;; Tramp
(setq tramp-histfile-override (expand-file-name "~/.emacs.d/.cache/tramp/tramp_history")
      tramp-persistency-file-name (expand-file-name "~/.emacs.d/.cache/tramp/tramp"))

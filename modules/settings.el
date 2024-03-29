;;; Prevent Clutter in init.el
(setq custom-file "/dev/null")

;;; Disable C-z
(qv/key * "C-z" nil)

;;; Disable Clicking Minibuffer
(qv/key minibuffer-inactive-mode-map [mouse-1] ignore)

;;; Y or N
(advice-add 'yes-or-no-p :override 'y-or-n-p)

;;; Don't ask for permission to use disabled commands
(setq disabled-command-function nil)

;;; Remove GUI Elements
(setq-default inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;;; Show Time
;;(setq display-time-format "%h %d  %H:%M |")
;;(display-time-mode 1)

;;; Theme Directory
(setq custom-theme-directory "~/.emacs.d/themes")

;;; Wrapping lines
(setq-default truncate-lines t)
(setq-default word-wrap t)


;;; Move to Trash
(setq-default delete-by-moving-to-trash t)

;;; Auto Saves
(setq-default auto-save-default t)
(setq-default make-backup-files nil)
(defun make-auto-save-file-name ()
  (concat (expand-file-name "~/.emacs.d/auto-save-list/")
          (replace-regexp-in-string "/" "!" buffer-file-name)))
(setq create-lockfiles nil)

;;; Moving Lines
(setq-default line-move-visual t)
(setq-default line-move-ignore-invisible t)

;;; Indents
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Line Numbers
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-grow-only t)

;;; Relative Window Size
(setq window-combination-resize t)

;;; Recursive Minibuffer
(setq enable-recursive-minibuffers nil)

;;; Stretch Cursor
(setq-default x-stretch-cursor nil)

;;; Fringe Width
;;(set-fringe-mode 10)

;;; Display Buffer
(setq display-buffer-base-action '(display-buffer-same-window ()))
;;(setq display-buffer-base-action '(display-buffer-below-selected ()))

(setq display-buffer-alist
      '(("*Calendar*" . (display-buffer-at-bottom ()))))

;;; Visual Line Mode
(visual-line-mode 1)

;;; Revert buffer
(advice-add 'revert-buffer :around
            (defun qv/revert-buffer-advice (revert &rest args)
              (if (cdr args) (apply revert args)
                (funcall revert (car args) t))))

;;; Case Insensitive Completions
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      bookmark-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;; Blink matching parens
(setq blink-matching-paren t)

;;; Profiler
(qv/keys *
  "C-x C-p" nil
  "C-x C-p C-s" (profiler-start 'cpu)
  "C-x C-p C-q" profiler-stop
  "C-x C-p C-r" profiler-report)

;;; Tramp
(setq tramp-histfile-override (expand-file-name "~/.emacs.d/.cache/tramp/tramp_history")
      tramp-persistency-file-name (expand-file-name "~/.emacs.d/.cache/tramp/tramp"))

;;; Disable browsers
(require 'seq)
(defun qv/disable-browsers ()
  (interactive)
  (dotimes (i 20)
    (let* ((script (expand-file-name "~/.bin/disable-browsers.sh"))
           (name (format "%s" (random)))
           (file (expand-file-name (format "/tmp/%s" name))))
      (shell-command-to-string (format "cp '%s' '%s'" script file))
      (shell-command-to-string (format "chmod +x '%s'" file))
      (qv/run-in-background file)
      (shell-command-to-string (format "rm -rf '%s'" file)))))
;;(qv/disable-browsers)



;;; Variables

(setq-default
 lexical-binding t

 tab-width 4
 indent-tabs-mode nil

 echo-keystrokes 0.001
 eldoc-idle-delay 0

 line-move-visual t
 line-move-ignore-invisible t

 sentence-end-double-space nil
 word-wrap t
 truncate-lines t
 delete-by-moving-to-trash t
 ring-bell-function #'ignore
 blink-matching-paren nil
 x-stretch-cursor nil
 window-combination-resize t
 enable-recursive-minibuffers nil

 custom-theme-directory "~/.emacs.d/themes" ; Don't clutter .emacs.d/
 custom-file "/tmp/custom.el" ; Prevent clutter in init.el
 disabled-command-function nil ; Don't ask for permission to use disabled commands
 register-preview-delay nil ; Don't show a preview when going to/setting marks
 warning-minimum-level :emergency ; Disable warning buffer popping up randomly
 )

;;; Move mouse to upper left on keypress
;; (mouse-avoidance-mode 'banish)

;;; Y or N
(bz/advise :override yes-or-no-p y-or-n-p)

;;; Remove GUI Elements
(setq-default inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;;; Auto Saves
(setq-default auto-save-default t)
(setq-default make-backup-files nil create-lockfiles nil)
(bz/advise :override make-auto-save-file-name bz/auto-save-file-name ()
  (concat (expand-file-name "~/.emacs.d/auto-save-list/")
          (replace-regexp-in-string "/" "!" buffer-file-name)))

;;; Line Numbers

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-grow-only t)

;;; Display Buffer
(defun bz/display-buffer-last-window (buf alist)
  (if (window-live-p bz/last-window)
      (select-window bz/last-window)
    (other-window 1))
  (display-buffer-same-window buf alist))

(setq display-buffer-base-action '(bz/display-buffer-last-window ()))

(setq display-buffer-alist
      '(("*Calendar*" . (display-buffer-at-bottom ()))
        ("*Backtrace*" . (display-buffer-at-bottom ()))
        ("\\*.*\\*" . (display-buffer-same-window ()))
        (".*\\.pdf" . (display-buffer-same-window ()))
        ;; ("magit:" . (display-buffer-same-window ()))
        ))

;;; Revert buffer without confirming
(bz/advise :around revert-buffer bz/revert-buffer-advice (revert &rest args)
  (if (cdr args) (apply revert args)
    (funcall revert (car args) t)))

;;; Case Insensitive Completions
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      bookmark-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;; Message Advice
(bz/advise :around message bz/message-advice (func format &rest args)
  (unless (string= format "Mark set")
    (apply func format args)))

;;; Avoid redundant marks
(defun bz/markers-too-close (m1 m2)
  (if-let* ((line1 (line-number-at-pos m1))
            (line2 (line-number-at-pos m2))
            (distance (abs (- line1 line2))))
      (<= distance 3)))

(bz/advise :around push-mark bz/avoid-redundant-marks (f &rest args)
  (setq mark-ring (seq-filter (lambda (m) (not (bz/markers-too-close m (mark)))) mark-ring))
  (apply f args))

;;; Keep Column
(setq-default bz/last-pos nil)
(bz/hook post-command-hook bz/update-cursor-column
  (if (not truncate-lines) (setq goal-column nil)
    (when bz/last-pos
      (when (or (not (eolp))
                (and (eq (car bz/last-pos) (line-number-at-pos))
                     (not (eq (cdr bz/last-pos) (current-column)))))
        (setq-local goal-column (current-column))))

    ;; (when track-eol (end-of-line))
    ;; (set-face-attribute 'cursor nil :background
    ;;                     (if track-eol (bz/color blue) "white"))

    (setq-local bz/last-pos (cons (line-number-at-pos) (current-column)))))
;;; Don't confirm new buffer on shell commands
(setq async-shell-command-buffer 'new-buffer)

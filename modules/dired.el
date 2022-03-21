;;; Launching
(defun qv/dired (&optional arg)
  (interactive "p")
  (let ((default-directory (if arg default-directory (expand-file-name "~"))))
    (call-interactively 'dired)))

(qv/keys * "C-x d" qv/dired)

;;; All the Icons
(qv/package all-the-icons)
(qv/package all-the-icons-dired)

(setq all-the-icons-scale-factor 1.0)
(setq all-the-icons-fileicon-scale-factor 1.0)

(qv/face all-the-icons-dired-dir-face dired-directory :fg nil)

;;; Subtree
(qv/package dired-subtree)

(setq dired-subtree-line-prefix "    ")

(qv/keys dired-mode-map
  "l" dired-subtree-insert
  "h" dired-subtree-remove
  "SPC" dired-subtree-toggle
  "o" (@ qv/dired-subtree-open-all
         (save-excursion
           (end-of-buffer)
           (while (> (line-number-at-pos nil) 1)
             (dired-subtree-insert) (previous-line 1))))
  "O" (@ qv/dired-subtree-close-all
         (save-excursion
           (end-of-buffer)
           (while (> (line-number-at-pos nil) 1)
             (dired-subtree-remove) (previous-line 1)))))

;;; Hidden Files
(setq dired-listing-switches "-lvA  --group-directories-first")
(defvar qv/dired-showing-hidden t)
(defun qv/dired-show-hidden (&optional arg)
  "If arg is nil or unspecified, toggle showing hidden.
    If arg is zero or negative, hide hidden files.
    Otherwise, show hidden files"
  (interactive)
  (setq qv/dired-showing-hidden
        (if (numberp arg)
            (if (> arg 0) t nil)
          (not qv/dired-showing-hidden)))
  ;; Yes, there should be two spaces between the argument groups,
  ;; because that noticably increases the speed for some reason
  (if qv/dired-showing-hidden
      (setq dired-listing-switches "-lvA  --group-directories-first")
    (setq dired-listing-switches "-lv  --group-directories-first"))
  (dired-revert)
  (qv/dired-reload))

(qv/key dired-mode-map "." qv/dired-show-hidden)

;;; Changing Directory
(defun qv/dired-open ()
  (interactive)
  (let ((filename (dired-get-filename)))
    (if (file-directory-p filename)
        (progn (kill-buffer (current-buffer)) (dired filename))
      (if (window-live-p qv/last-window)
          (select-window qv/last-window)
        (other-window 1))
      (find-file filename))))

(setq qv/current-window (selected-window)
      qv/last-window (selected-window))
(qv/hook window-state-change-hook qv/remember-last-window
  (unless (or (minibufferp nil) (eq qv/current-window (selected-window)))
    (setq qv/last-window qv/current-window
          qv/current-window (selected-window))))

(defun qv/dired-up ()
  (interactive)
  (let ((up-dir (expand-file-name (format "%s/.." dired-directory))))
    (kill-buffer (current-buffer))
    (dired up-dir)))

(qv/keys dired-mode-map
  "RET" qv/dired-open
  "L" qv/dired-open
  "H" qv/dired-up)

;;; Mappings
(qv/keys dired-mode-map
  "j" dired-next-line
  "k" dired-previous-line
  "J" qv/down4
  "K" qv/up4
  "d" dired-do-delete
  "/" isearch-forward
  "?" isearch-backward
  "n" isearch-repeat-forward
  "N" isearch-repeat-backward
  "p" emms-play-dired
  "g" nil)

(defun qv/dired-toggle-mark (&optional arg)
  "Toggle whether the current file is marked.
      If arg is negative or zero, disable the mark.
      If arg is positive, enable the mark."
  (interactive)
  (dired-move-to-filename)
  (let ((mark
         (if arg (if (and (numberp arg) (<= arg 0)) nil t)
           (if (eq (plist-get (text-properties-at (point)) 'face) 'dired-marked) nil t))))
    (if mark-active
        (dolist (i (number-sequence (line-number-at-pos (min (point) (mark)))
                                    (line-number-at-pos (max (point) (mark)))))
          (deactivate-mark) (goto-line i) (qv/dired-toggle-mark (if mark 1 0)))
      (if mark (dired-mark 1) (dired-unmark 1))
      (previous-line) (dired-move-to-filename))))

(qv/keys dired-mode-map
  "m" (qv/dired-toggle-mark 1)
  "u" (qv/dired-toggle-mark 0)
  "M" ((dired-unmark-all-marks) (dired-toggle-marks))
  "U" dired-unmark-all-marks
  "t" qv/dired-toggle-mark
  "T" dired-toggle-marks
  "r" ((revert-buffer) (qv/dired-reload))
  "V" set-mark-command
  "a" nil
  "a f" dired-create-empty-file
  "a d" dired-create-directory)

;;; Startup
(qv/hook dired-mode-hook qv/dired-setup
  (setq dired-listing-switches "-lvA  --group-directories-first")
  (setq dired-marker-char ?!)
  (push (list (concat "^[" (char-to-string dired-marker-char) "]")
              '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
        dired-font-lock-keywords)

  (display-line-numbers-mode 0)
  (setq dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-mode)
  (setq-local tab-width 1
              scroll-margin 0
              maximum-scroll-margin 0.0
              scroll-step 2
              window-size-fixed 'width
              line-spacing 0.1)

  (variable-pitch-mode 1)
  (qv/face dired-directory :fg blue)
  (qv/face dired-header dired-directory :h 1.1 :w bold :u t)
  (qv/face dired-marked :fg yellow)
  (qv/face dired-mark dired-marked)
  (qv/face dired-symlink :fg purple)
  (dotimes (i 5)
    (qv/face ,(intern (format "dired-subtree-depth-%s-face" (1+ i))) :bg nil))

  (rename-buffer
   (format "Dired: %s"
           (replace-regexp-in-string
            (concat "^" (regexp-quote (expand-file-name "~"))) "~"
            dired-directory)))

  (all-the-icons-dired-mode))

;;; Reloading
(defun qv/dired-reload (&optional arg pred third)
  (interactive)
  (all-the-icons-dired--refresh))

(add-hook 'dired-mode-hook 'qv/dired-reload)
(add-hook 'dired-subtree-after-remove-hook 'qv/dired-reload)
(add-hook 'dired-subtree-after-insert-hook 'qv/dired-reload)

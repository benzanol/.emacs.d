;;; Settings
(setq dired-listing-switches "-lvA  --group-directories-first")

;; Mark files with an exclamation point, and highlight them
(setq dired-marker-char ?!)
(push (list (concat "^[" (char-to-string dired-marker-char) "]")
            '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
      dired-font-lock-keywords)

;; Hide file details, but not link locations
(setq dired-hide-details-hide-symlink-targets nil)

;; Faces
(qv/face dired-directory :fg blue)
(qv/face dired-header dired-directory :h 1.1 :w bold :u t)
(qv/face dired-marked :fg yellow :w normal :s italic)
(qv/face dired-mark dired-marked)
(qv/face dired-symlink :fg orange :w normal)

;;; Keybindings
(qv/package f)

(qv/keys dired-mode-map
  :sparse t
  "RET" qv/dired-open
  "<S-return>" dired-find-file
  "H" (@ qv/dired-up (qv/dired-open (f-parent default-directory)))
  "r" revert-buffer
  "!" dired-do-shell-command
  "d" dired-do-delete
  "p" emms-play-dired
  "." qv/dired-show-hidden

  "s" qv/dired-do-move-files
  "y" qv/dired-do-copy-files
  "R" dired-do-rename
  "D" dired-do-delete

  "m" (qv/dired-toggle-mark 1)
  "M" ((dired-unmark-all-marks) (dired-toggle-marks))
  "u" (qv/dired-toggle-mark 0)
  "U" dired-unmark-all-marks
  "t" qv/dired-toggle-mark
  "T" dired-toggle-marks

  "a f" (let ((default-directory (dired-current-directory)))
          (call-interactively 'dired-create-empty-file)
          (dired-revert))
  "a d" ((call-interactively 'dired-create-directory)
         (dired-revert))

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

;;; Setup
(qv/hook dired-mode-hook qv/dired-setup
  (setq-local tab-width 1
              truncate-lines t
              window-size-fixed nil ; 'width
              line-spacing 0.1)

  (run-with-timer 0 nil 'qv/dired-truncate-title)
  (dired-hide-details-mode 1)
  (display-line-numbers-mode 0)
  (variable-pitch-mode 1)
  (all-the-icons-dired-mode)

  ;; By default, the buffer name shows up as error face, weirdly even if the window
  ;; isn't focused, in which case it should use the inactive face
  (when doom-modeline-mode
    (setq-local mode-line-format '("%e" (:eval (doom-modeline-format--main)))))

  (rename-buffer
   (format "Dired: %s"
           (replace-regexp-in-string
            (concat "^" (regexp-quote (expand-file-name "~"))) "~"
            dired-directory))))

(advice-add 'dired-revert :after 'qv/dired-truncate-title)

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

;; Make the mode line not red
(qv/hook dired-subtree-after-insert-hook qv/dired-subtree-after-insert
  (when doom-modeline-mode
    (setq-local mode-line-format '("%e" (:eval (doom-modeline-format--main))))))

;; Don't highlight inner depths
(qv/face dired-subtree-depth-1-face :bg nil)
(qv/face dired-subtree-depth-2-face :bg nil)
(qv/face dired-subtree-depth-3-face :bg nil)
(qv/face dired-subtree-depth-4-face :bg nil)
(qv/face dired-subtree-depth-5-face :bg nil)

;;; Special functions
;;;; Truncate title
(defun qv/dired-truncate-title (&rest args)
  (let ((inhibit-read-only t)
        prev)
    (unless (derived-mode-p 'dired-mode)
      (error "Not a dired buffer"))

    (save-excursion
      (beginning-of-buffer)

      (when (search-forward (expand-file-name "~") (line-end-position) t)
        (insert "~")
        (put-text-property (point-min) (1- (point)) 'invisible t))

      (while (search-forward-regexp "/\\.?[^./]" (line-end-position) t)
        (when prev (put-text-property prev (match-beginning 0) 'invisible t))
        (setq prev (point))))))

;;;; Hidden Files
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

  ;; Forcibly create a new buffer (will remove subtrees)
  (qv/dired-open default-directory))

;;;; Changing Directory
(defun qv/dired-open (&optional file)
  (interactive)
  (let ((filename (or file (dired-get-filename))))
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

;;;; Read file from dired
(defun qv/dired-read-file (key &optional dir msg)
  "Read a file by navigating to it in dired then pressing KEY."
  (message "%sNavigate to the file you wish to select, then press `%s`"
           (or msg "")
           (key-description key))

  (let* ((map (make-sparse-keymap)))
    (define-key map key 'exit-recursive-edit)

    (let ((minor-mode-map-alist
           (cons (cons t map) minor-mode-map-alist)))

      (recursive-edit)
      (if dir
          (dired-current-directory)
        (dired-get-filename)))))

;; Don't show mark popup
(advice-add 'dired-mark-read-file-name :override
            (lambda (prompt &rest args) (read-file-name prompt)))

;; Move files by moving to the target directory
(defun qv/dired-do-move-files ()
  (interactive)
  (let ((read-file-name-function
         (lambda (prompt &rest args)
           (qv/dired-read-file "p" 'dir "Move file to new directory: "))))
    (dired-do-rename)
    (revert-buffer)))

;; Copy files by moving to the target directory
(defun qv/dired-do-copy-files ()
  (interactive)
  (let ((read-file-name-function
         (lambda (prompt &rest args)
           (qv/dired-read-file "p" 'dir "Move file to new directory: "))))
    (dired-do-copy)
    (revert-buffer)))

;;;; Toggle mark
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

;;;; Mark files in region
(advice-add 'dired-get-marked-files :before 'qv/dired-add-region-files)

(defun qv/dired-add-region-files (&rest args)
  "Add files in the region to the list of marked files"
  (when mark-active (qv/dired-toggle-mark 1)))


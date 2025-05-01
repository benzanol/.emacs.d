;; -*- lexical-binding:t -*-

(bz/package dired)

(bz/require tabline)


;;; Settings
(setq dired-listing-switches "-lvA  --group-directories-first")

(setq dired-recursive-deletes 'always)

;; Mark files with an exclamation point, and highlight them
(setq dired-marker-char ?!)
(push (list (concat "^[" (char-to-string dired-marker-char) "]")
            '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
      dired-font-lock-keywords)

;; Hide file details, but not link locations
(setq dired-hide-details-hide-symlink-targets nil)

;; Show colors
(setq all-the-icons-dired-monochrome nil)

;; Refresh after delete
(bz/advise :after dired-do-delete revert-buffer)

;; Faces
(bz/face dired-directory :fg blue)
(bz/face dired-header dired-directory :h 1.1 :w bold :u t)
(bz/face dired-marked :fg yellow :w normal :s italic)
(bz/face dired-mark dired-marked)
(bz/face dired-symlink :fg orange :w normal)

;; Completing read for shell command
(bz/advise :override dired-read-shell-command bz/dired-read-shell-command (prompt arg files)
  (format "%s &" (completing-read (format prompt (dired-mark-prompt arg files))
                                  (bz/get-executables))))


;;; Keybindings
(bz/package f)

(bz/keys dired-mode-map
  :sparse t
  [remap bz/down] dired-next-line
  [remap bz/up] dired-previous-line
  [remap bz/left] dired-subtree-remove
  [remap bz/right] dired-subtree-insert

  "RET" bz/dired-open
  "<S-return>" dired-find-file
  "e" (@ bz/dired-up (bz/dired-open (f-parent default-directory)))
  "!" (@ bz/dired-shell-command-on-file
         (let ((default-directory (dired-current-directory)))
           (call-interactively #'dired-do-shell-command)))
  "@" (@ bz/dired-shell-command
         (let* ((default-directory (dired-current-directory))
                (dirname (file-name-nondirectory (directory-file-name default-directory))))
           (async-shell-command (read-string (format "Shell Command in %s: " dirname)))))

  "r" revert-buffer
  "P" emms-play-dired
  "." bz/dired-show-hidden

  "s" (bz/dired-clipboard 'move)
  "S" dired-do-rename
  "y" (bz/dired-clipboard 'copy)
  "Y" dired-do-copy
  "x" (bz/dired-clipboard 'symlink)
  "X" dired-do-symlink
  "p" bz/dired-paste
  "d" bz/dired-delete

  "m" (bz/dired-toggle-mark 1)
  "M" ((dired-unmark-all-marks) (dired-toggle-marks))
  "u" (bz/dired-toggle-mark 0)
  "U" dired-unmark-all-marks
  "t" bz/dired-toggle-mark
  "T" dired-toggle-marks

  "a f" (let ((default-directory (dired-current-directory)))
          (call-interactively 'dired-create-empty-file)
          (dired-revert))
  "a d" ((call-interactively 'dired-create-directory)
         (dired-revert))

  "SPC" dired-subtree-toggle
  "o" (@ bz/dired-subtree-open-all
         (save-excursion
           (end-of-buffer)
           (while (> (line-number-at-pos nil) 1)
             (dired-subtree-insert) (previous-line 1))))
  "O" (@ bz/dired-subtree-close-all
         (save-excursion
           (end-of-buffer)
           (while (> (line-number-at-pos nil) 1)
             (dired-subtree-remove) (previous-line 1)))))

;;; Setup


(setq bz/dired-chronological-directories
      '("~/Downloads" "~/Downloads/save" "~/Downloads/webpages" "~/Media/Images"))

(bz/hook dired-mode-hook bz/dired-setup
  (setq-local truncate-lines t
              window-size-fixed nil ; 'width
              line-spacing 0.1)

  (let ((sorting-by-date (string-match-p dired-sort-by-date-regexp dired-actual-switches))
        (should-sort-by-date (member (replace-regexp-in-string "/\\'" "" (expand-file-name dired-directory))
                                     (mapcar #'expand-file-name bz/dired-chronological-directories))))
    (when (xor sorting-by-date should-sort-by-date)
      (run-with-timer 0 nil #'dired-sort-toggle)))

  (let* ((buf (current-buffer))
         (fn (lambda () (with-current-buffer buf (bz/dired-truncate-title)))))
    (run-with-timer 0 nil fn))
  (dired-hide-details-mode 1)
  (display-line-numbers-mode 0)
  (variable-pitch-mode 1)
  (all-the-icons-dired-mode)
  (auto-revert-mode 1)

  ;; By default, the buffer name shows up as error face, weirdly even if the window
  ;; isn't focused, in which case it should use the inactive face
  (when doom-modeline-mode
    (setq-local mode-line-format '("%e" (:eval (doom-modeline-format--main)))))

  (rename-buffer
   (format "Dired: %s"
           (replace-regexp-in-string
            (concat "^" (regexp-quote (expand-file-name "~"))) "~"
            dired-directory))))

;;; Launching
(defun bz/dired (&optional arg)
  (interactive "p")
  (let ((default-directory (if arg default-directory (expand-file-name "~"))))
    (call-interactively 'dired)))

;; (bz/keys * "C-x d" bz/dired)

;;; All the Icons
(bz/package all-the-icons)
(bz/package all-the-icons-dired)

(setq all-the-icons-scale-factor 1.0)
(setq all-the-icons-fileicon-scale-factor 1.0)

(bz/face all-the-icons-dired-dir-face dired-directory :fg nil)

;;; Subtree
(bz/package dired-subtree)

;; Can't be 4 spaces, because otherwise weird indent guides show up on 2nd space
(setq dired-subtree-line-prefix "\t\t\t\t")

;; Make the mode line not red
(bz/hook dired-subtree-after-insert-hook bz/dired-subtree-after-insert
  (when doom-modeline-mode
    (setq-local mode-line-format '("%e" (:eval (doom-modeline-format--main))))))

;; Don't highlight inner depths
(bz/face dired-subtree-depth-1-face :bg nil)
(bz/face dired-subtree-depth-2-face :bg nil)
(bz/face dired-subtree-depth-3-face :bg nil)
(bz/face dired-subtree-depth-4-face :bg nil)
(bz/face dired-subtree-depth-5-face :bg nil)
(bz/face dired-subtree-depth-6-face :bg nil)

;;; Openwith
(bz/package openwith)
(openwith-mode 1)

(setq openwith-associations
      `((,(openwith-make-extension-regexp
           '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm" "ogg" "mkv"))
         "vlc" (file))
        ;; (,(openwith-make-extension-regexp
        ;;    '("xbm" "pbm" "pgm" "ppm" "pnm" "png" "gif" "bmp" "tif" "jpeg" "jpg"))
        ;; "gthumb" (file))
        (,(openwith-make-extension-regexp
           '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
         "libreoffice" (file))
        (,(openwith-make-extension-regexp
           '("pdf"))
         "evince" (file))))

(bz/advise :around files--ask-user-about-large-file
           bz/openwith-ask-about-large-file-advice (func size op-type filename offer-raw)
  (if (--any (string-match-p (car it) filename) openwith-associations)
      nil ; Return nil to open the file
    (funcall func size op-type filename offer-raw)))

(defun files--ask-user-about-large-file (size op-type filename offer-raw)
  "Query the user about what to do with large files.
Files are \"large\" if file SIZE is larger than `large-file-warning-threshold'.

OP-TYPE specifies the file operation being performed on FILENAME.

If OFFER-RAW is true, give user the additional option to open the
file literally."
  (let ((prompt (format "File %s is large (%s), really %s?"
                        (file-name-nondirectory filename)
                        (funcall byte-count-to-string-function size) op-type)))
    (if (not offer-raw)
        (if (y-or-n-p prompt) nil 'abort)
      (let ((choice
             (car
              (read-multiple-choice
               prompt '((?y "yes")
                        (?n "no")
                        (?l "literally"))
               (files--ask-user-about-large-file-help-text
                op-type (funcall byte-count-to-string-function size))))))
        (cond ((eq choice ?y) nil)
              ((eq choice ?l) 'raw)
              (t 'abort))))))


;;; Special functions
;;;; Move to trash

(defun bz/dired-delete (&rest files)
  (interactive (dired-get-marked-files))

  (unless files (error "No files specified"))

  (when (y-or-n-p
         (format "Are you sure you want to delete %s"
                 (if (cdr files) (format "%s files?" (length files))
                   (file-name-nondirectory (car files)))))

    (dolist (file files)
      (cond ((file-symlink-p file) (start-process "rm" nil "rm" file))
            ((file-exists-p file) (move-file-to-trash file))
            (t (message "File %s does not exist!" file)))))

  (when (derived-mode-p 'dired-mode) (revert-buffer)))


;;;; Copy/paste files from dired

;; Has the format (action files...)
;; Action can be move, copy, or symlink
(setq bz/dired-clipboard nil)

(defun bz/dired-clipboard (action)
  (interactive (list (intern (completing-read "Action: " '(move copy symlink)))))
  (setq bz/dired-clipboard (cons action (dired-get-marked-files)))

  (message "%s %s %s"
           (pcase action ('copy "Copying") ('move "Moving") ('symlink "Symlinking"))
           (length (cdr bz/dired-clipboard))
           (if (cddr bz/dired-clipboard) "files" "file")))

(defun bz/dired-paste (dir action &rest files)
  (interactive (cons (dired-current-directory)
                     (or bz/dired-clipboard (error "No clipboard!"))))

  (unless action (error "No action specified"))
  (unless dir (error "No directory specified"))

  (dolist (file files)
    (let ((name (file-name-nondirectory file)))

      (when (and
             (if (file-exists-p file) t (message "File %s does not exist" name) nil)
             (or (not (file-exists-p (format "%s/%s" dir name)))
                 (y-or-n-p (format "File `%s` already exists here. Overwrite it? " name))))

        (pcase action
          ('move (start-process "mv" nil "mv" file dir))
          ('copy (start-process "cp" nil "cp" "-rf" file dir))
          ('symlink (start-process "ln" nil "ln" "-s" file dir))))))

  (when (derived-mode-p 'dired-mode) (revert-buffer)))


;;;; Truncate title
(bz/advise :after dired-revert bz/dired-truncate-title (&rest args)
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
(defvar bz/dired-showing-hidden t)
(defun bz/dired-show-hidden (&optional arg)
  "If arg is nil or unspecified, toggle showing hidden.
    If arg is zero or negative, hide hidden files.
    Otherwise, show hidden files"
  (interactive)
  (setq bz/dired-showing-hidden
        (if (numberp arg)
            (if (> arg 0) t nil)
          (not bz/dired-showing-hidden)))
  ;; Yes, there should be two spaces between the argument groups,
  ;; because that noticably increases the speed for some reason
  (if bz/dired-showing-hidden
      (setq dired-listing-switches "-lvA  --group-directories-first")
    (setq dired-listing-switches "-lv  --group-directories-first"))

  ;; Forcibly create a new buffer (will remove subtrees)
  (bz/dired-open default-directory))

;;;; Changing Directory
(defun bz/dired-open (&optional file)
  (interactive)
  (unless file (setq file (dired-get-filename)))
  (if (not (file-directory-p file))
      (display-buffer (find-file-noselect file))
    (kill-buffer (current-buffer))
    (dired file)))

;;;; Toggle mark
(defun bz/dired-toggle-mark (&optional arg)
  "Toggle whether the current file is marked.
If arg is negative or zero, disable the mark. If arg is positive,
enable the mark. If the region is active, toggle all the marks in
the region."

  (interactive)
  (dired-move-to-filename)
  (let ((mark
         (if arg (if (and (numberp arg) (<= arg 0)) nil t)
           (if (eq (plist-get (text-properties-at (point)) 'face) 'dired-marked) nil t))))
    (if mark-active
        (dolist (i (number-sequence (line-number-at-pos (min (point) (mark)))
                                    (line-number-at-pos (max (point) (mark)))))
          (deactivate-mark) (goto-line i) (bz/dired-toggle-mark (if mark 1 0)))
      (if mark (dired-mark 1) (dired-unmark 1))
      (previous-line) (dired-move-to-filename))))

;;;; Mark files in region
(bz/advise :before dired-get-marked-files bz/dired-add-region-files (&rest args)
  "Add files in the region to the list of marked files."
  (when mark-active (bz/dired-toggle-mark 1)))

;;;; Include file name when renaming
(bz/advise :override dired-mark-read-file-name bz/* (prompt dir op-symbol arg files &optional default)
  (dired-mark-pop-up
   nil op-symbol files
   #'read-file-name
   (format prompt (dired-mark-prompt arg files)) dir default nil
   ;; Include the default name as the intial input
   (file-name-nondirectory default)))

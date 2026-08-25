;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-keys)

(require 'hideshow)
(require 'outline)


(defun bz/outline-after-move-line ()
  (ignore-errors
    (when (outline-invisible-p (1- (point)))
      (beginning-of-visual-line)
      (end-of-line))))

(bz/keys *
  "C-x C-o" (@ bz/outline (if outline-minor-mode (bz/outline-hide) (outline-minor-mode))))

(bz/hook outline-minor-mode-hook bz/outline-mode-setup
  ;; Minor mode hook runs on enter AND exit
  (when outline-minor-mode

    (hs-minor-mode 0)
    (bz/outline-hide)))


;;; Outline keybindings

(bz/keys outline-minor-mode-map
  :sparse t
  [remap bz/up] (@ bz/outline-up n
                   (bz/up n)
                   (bz/outline-after-move-line)
                   (setf (cadr bz/vertical-motion-info) (point)))
  [remap bz/down] (@ bz/outline-down n
                     (bz/down n)
                     (bz/outline-after-move-line)
                     (setf (cadr bz/vertical-motion-info) (point)))

  [remap bz/fold-toggle] outline-toggle-children

  [remap bz/fold-show] outline-show-all
  [remap bz/fold-hide]
  (@ bz/outline-hide arg
     (save-excursion (goto-char (point-min))
                     (outline-hide-sublevels (if arg 100 1))))

  [remap bz/fold-toggle-all] (if (--find (eq (overlay-get it 'invisible) 'outline) (overlays-in (point-min) (point-max)))
                                 (outline-show-all) (bz/outline-hide))

  [remap bz/fold-level]
  (@ bz/outline-fold-level
     (outline-show-entry) (outline-show-children)))


;;; Switch between hideshow
;;; Add a custom character for outline text

(set-display-table-slot
 standard-display-table
 'selective-display (string-to-vector " ➾"))


;;; Automatically move off of overlay

;; Redundant because of 'stay on same line' functions
;; (bz/hook post-command-hook bz/move-off-outline-overlays
;;   (when (or outline-minor-mode (eq major-mode 'org-mode))
;;     (when (or (outline-invisible-p)
;;               (save-excursion (goto-char (1- (point)))
;;                               (outline-invisible-p)))
;;       (beginning-of-visual-line)
;;       (end-of-line))))


;;; Provide

(provide 'bz-outline)

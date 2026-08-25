;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-keys)


(setq-default scroll-margin 0)
(setq-default maximum-scroll-margin 0.25)
(setq scroll-conservatively 0)

(defvar bz/recenter-cursor-arg nil)

(define-minor-mode bz/recenter-cursor-mode
  "Recenter the cursor after every command."
  :init-value t)

(bz/hook post-command-hook bz/recenter
  (ignore-errors
    (when (and bz/recenter-cursor-mode
               (eq (window-buffer) (current-buffer)))
      (if (eq this-command #'pixel-scroll-precision)
          nil
        ;; (let* ((start (posn-x-y (posn-at-point (window-start))))
        ;;        (left (car start))
        ;;        (top (cdr start))
        ;;        (btm (+ top (window-pixel-height)))
        ;;        (target (posn-point (posn-at-x-y left (/ (+ top btm) 2)))))
        ;;   (if target (goto-char target)
        ;;     (let ((beg (line-number-at-pos (window-start)))
        ;;           (end (line-number-at-pos (window-end))))
        ;;       (goto-char (point-min))
        ;;       (forward-line (1- (/ (+ beg end) 2))))))

        ;; When profiling bz/recenter, half of the memory usage came
        ;; from jit-lock (fontify) being called inside of recenter
        (let* ((jit-lock-mode nil))
          (recenter bz/recenter-cursor-arg))))))

(bz/hook (minibuffer-setup-hook calendar-mode-hook) bz/disable-recenter-cursor
  (interactive)
  (bz/recenter-cursor-mode 0))


(provide 'bz-recenter)

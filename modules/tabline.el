(bz/package tab-line)
(bz/package cl-seq)

;;; Custom Name
(setq tab-line-tab-name-function 'bz/tab-line-name)
(defun bz/tab-line-name (buffer &optional buffers)
  (format " %s %s  "
          (string (+ 9312 (1- (seq-position (buffer-local-value 'bz/tab-line-tabs buffer) buffer))))
          (replace-regexp-in-string "<.*>$" "" (buffer-name (get-buffer buffer)))))

;;; Which tabs to show
(defvar-local bz/tab-line-tabs nil
  "List of buffers, starting with the symbol `tab`.")

(setq tab-line-tabs-function 'bz/tab-line-function)
(defun bz/tab-line-function ()
  (setcdr bz/tab-line-tabs
          (-uniq
           (--filter (and (buffer-live-p it)
                          (buffer-local-value 'tab-line-mode it)
                          (eq bz/tab-line-tabs (buffer-local-value 'bz/tab-line-tabs it)))
                     (cdr bz/tab-line-tabs)))))

;;; Faces
(bz/face tab-line variable-pitch :h 1.0 :s normal :fg gray1 :bg bg2 :iv nil)
(bz/face tab-line-tab tab-line :fg nil :bg nil :b (:line-width 2 :color ,(bz/color gray3)))
(bz/face tab-line-tab-current tab-line-tab :fg nil :bg nil :w bold)
(bz/face tab-line-tab-inactive tab-line-tab :fg gray1 :bg nil)
(bz/face tab-line-tab-modified nil :s italic)
(bz/face tab-line-tab-special nil :s italic)

(setq tab-line-close-button-show nil)
(setq tab-line-new-button-show nil)

;;; Keybindings
(bz/keys bz/tab-line-mode-map
  :sparse t
  "C-M-a" tab-line-switch-to-prev-tab
  "C-M-d" tab-line-switch-to-next-tab
  "C-M-w" bz/kill-current-buffer
  "C-M-q" (bz/tab-line-move 'left)
  "C-M-e" (bz/tab-line-move 'right))

(dotimes (i 10)
  (define-key bz/tab-line-mode-map
    (kbd (format "<normal> m %s" i))
    `(lambda () (interactive) (switch-to-buffer (nth ,i bz/tab-line-tabs)))))

(bz/key esc-map "C-s" nil)
(bz/keys bz/normal-map
  "C-M-s" (tab-line-mode 'toggle))

(push (cons 'tab-line-mode bz/tab-line-mode-map) minor-mode-map-alist)

;;; Commands
;;;; Close Tab
(defun bz/tab-line-close (&optional b)
  (interactive)
  (with-current-buffer (or b (current-buffer))
    (when-let ((tabs bz/tab-line-tabs)
               (idx (seq-position tabs (current-buffer))))
      (setcdr tabs (remove (current-buffer) (cdr tabs)))
      (setq-local bz/tab-line-tabs nil)
      (tab-line-mode 0)
      (unless (equal (list 'tabs) tabs)
        (switch-to-buffer (or (nth idx tabs) (nth (1- idx) tabs)))))))

;;;; New Tab
(bz/hook tab-line-mode-hook bz/tab-line-check
  (if tab-line-mode
      (when (null bz/tab-line-tabs)
        (tab-line-mode 0)
        (bz/tab-line-new))
    ;; Make it so if you disable then enable, it will go with the current workspace
    (setq bz/tab-line-tabs nil)))

;; (global-tab-line-mode 1)

(bz/hook find-file-hook bz/tab-line-new)

(defvar-local bz/dired-tabs nil)
(defun bz/tab-line-new (&optional arg)
  (interactive)
  (if (and arg (listp arg))
      (progn (setq-local bz/tab-line-tabs arg)
             (setcdr (last arg) (list (current-buffer)))
             (tab-line-mode))

    (if (eq arg 'new)
        (progn (setq-local bz/tab-line-tabs (list 'tabs (current-buffer)))
               (tab-line-mode))

      (if tab-line-mode
          (let ((new (if (bufferp arg) arg
                       (get-buffer (read-buffer "New Tab: " nil nil))))
                (tabs bz/tab-line-tabs))
            (unless (memq new tabs)
              (setcdr (last tabs) (list new))
              (switch-to-buffer new)
              (setq-local bz/tab-line-tabs tabs)
              (tab-line-mode)))
        (let* (;; (f (lambda (b) (with-current-buffer (if (consp b) (car b) b)
               ;;                  (or tab-line-mode bz/dired-tabs))))
               ;; (b (when (seq-filter f (buffer-list))
               ;;      (read-buffer "Join tab group: " nil nil f)))
               ;; (tabs (if (or (null b) (string= b "")) (list 'tabs)
               ;;         (with-current-buffer b (or bz/tab-line-tabs bz/dired-tabs))))
               (tabs (or (bz/activity-get :tabs)
                         (progn (bz/activity-set :tabs (list 'tabs))
                                (bz/activity-get :tabs)))))
          (setq-local bz/tab-line-tabs tabs)
          (setcdr (last tabs) (list (current-buffer)))
          (tab-line-mode))))))

;;;; Move Tabs
(defun bz/tab-line-move (&optional direction)
  "If direction is nil or 'right, move right, otherwise move left."
  (interactive "P")
  (when (> (length bz/tab-line-tabs) 2)
    (when-let* ((tabs bz/tab-line-tabs)
                (i (seq-position tabs (current-buffer)))
                (g (+ i (if (or (null direction) (eq direction 'right)) 1 -1))))
      (cond ((eq g 0) (setcdr tabs (append (cddr tabs) (list (current-buffer)))))
            ((eq g (length tabs))
             (setcdr tabs (cons (current-buffer) (remove (current-buffer) (cdr tabs)))))
            (t (setf (nth i bz/tab-line-tabs) (nth g bz/tab-line-tabs))
               (setf (nth g bz/tab-line-tabs) (current-buffer))))
      ;; Update the tab display
      (set-window-parameter nil 'tab-line-cache nil))))

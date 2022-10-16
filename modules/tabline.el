(qv/package tab-line)
(qv/package cl-seq)

;;; Custom Name
(setq tab-line-tab-name-function 'qv/tab-line-name)
(defun qv/tab-line-name (buffer &optional buffers)
  (let ((b (get-buffer buffer)))
    (format "  %s  " (buffer-name b))))

;;; Custom Function
(defvar-local qv/tab-line-tabs nil
  "List of buffers, starting with the symbol `tab`.")

(setq tab-line-tabs-function 'qv/tab-line-function)
(defun qv/tab-line-function ()
  (setcdr qv/tab-line-tabs
          (cl-remove-duplicates
           (seq-filter 'buffer-live-p (cdr qv/tab-line-tabs)))))

;;;; Close Tab
(setq tab-line-close-tab-function 'qv/tab-line-close)
(defun qv/tab-line-close (&optional b)
  (interactive)
  (with-current-buffer (or b (current-buffer))
    (when-let ((tabs qv/tab-line-tabs))
      (setcdr tabs (remove (current-buffer) (cdr tabs)))
      (setq-local qv/tab-line-tabs nil)
      (tab-line-mode 0)
      (unless (equal (list 'tabs) tabs)
        (switch-to-buffer (cadr tabs))))))

;;;; New Tab
(qv/hook tab-line-mode-hook qv/tab-line-check
  (when (and (null qv/tab-line-tabs) tab-line-mode)
    (tab-line-mode 0)
    (qv/tab-line-new)))

(defun qv/tab-line-new (&optional arg)
  (interactive)
  (if (and arg (listp arg))
      (progn (setq-local qv/tab-line-tabs arg)
             (setcdr (last arg) (list (current-buffer)))
             (tab-line-mode))

    (if (eq arg 'new)
        (progn (setq-local qv/tab-line-tabs (list 'tabs (current-buffer)))
               (tab-line-mode))
      
      (if tab-line-mode
          (let ((new (if (bufferp arg) arg
                       (get-buffer (read-buffer "New Tab: " nil nil))))
                (tabs qv/tab-line-tabs))
            (unless (memq new tabs)
              (setcdr (last tabs) (list new))
              (switch-to-buffer new)
              (setq-local qv/tab-line-tabs tabs)
              (tab-line-mode)))
        (let* ((f (lambda (b) (with-current-buffer (if (consp b) (car b) b) tab-line-mode)))
               (b (when (seq-filter f (buffer-list))
                    (read-buffer "Join tab group: " nil nil f)))
               (tabs (if (or (null b) (string= b "")) (list 'tabs)
                       (with-current-buffer b qv/tab-line-tabs))))
          (setq-local qv/tab-line-tabs tabs)
          (setcdr (last tabs) (list (current-buffer)))
          (tab-line-mode))))))

;;;; Move Tabs
(defun qv/tab-line-move (&optional direction)
  "If direction is nil or 'right, move right, otherwise move left."
  (interactive "P")
  (when (> (length qv/tab-line-tabs) 2)
    (when-let* ((tabs qv/tab-line-tabs)
                (i (seq-position tabs (current-buffer)))
                (g (+ i (if (or (null direction) (eq direction 'right)) 1 -1))))
      (cond ((eq g 0) (setcdr tabs (append (cddr tabs) (list (current-buffer)))))
            ((eq g (length tabs))
             (setcdr tabs (cons (current-buffer) (remove (current-buffer) (cdr tabs)))))
            (t (setf (nth i qv/tab-line-tabs) (nth g qv/tab-line-tabs))
               (setf (nth g qv/tab-line-tabs) (current-buffer))))
      ;; Update the tab display
      (set-window-parameter nil 'tab-line-cache nil))))

;;; Appearance
(qv/face tab-line variable-pitch :h 1.0 :s normal :fg gray1 :bg bg2 :iv nil)
(qv/face tab-line-tab tab-line :fg nil :bg nil :b (:line-width 2 :color ,(qv/color gray3)))
(qv/face tab-line-tab-current tab-line-tab :fg nil :bg nil :w bold :s italic)
(qv/face tab-line-tab-inactive tab-line-tab :fg gray1 :bg nil)

(setq tab-line-close-button-show nil)
(setq tab-line-new-button-show nil)

;;; Keybindings
(qv/keys qv/tab-line-mode-map
  :sparse t
  "M-C-a" tab-line-switch-to-prev-tab
  "M-C-d" tab-line-switch-to-next-tab
  "M-C-w" tab-line-close-tab
  "M-C-s" qv/tab-line-new
  "M-C-q" (qv/tab-line-move 'left)
  "M-C-e" (qv/tab-line-move 'right))

(qv/keys *
  "M-C-s" qv/tab-line-new)

(push (cons 'tab-line-mode qv/tab-line-mode-map) minor-mode-map-alist)


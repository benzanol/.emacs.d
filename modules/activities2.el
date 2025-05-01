;; Activity
;; :layout - A window layout

(setq qv/activities nil) ; Alist of plists
(setq qv/current-activity 'default) ; Symbol

(defun qv/activity-new (name)
  (interactive (read-string "Name: "))

  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*scratch*"))

  (let ((symbol (intern (downcase name)))
        (act (list :layout (current-window-configuration))))
    ()
    ))


;;; Serialize window configurations
(defun qv/serialize-window-configuration (config)
  (save-window-excursion
    (set-window-configuration config)
    (qv/serialize-window-tree (car (window-tree)))))

(defun qv/serialize-window-tree (tree)
  (if (windowp tree)
      (with-selected-window tree
        (list 'buffer (qv/serialize-buffer) (window-width) (window-height)))
    (cons (if (car tree) 'vertical 'horizontal)
          (mapcar #'qv/serialize-window-tree (cddr tree)))))

(defun qv/deserialize-window-configuration (layout)
  (save-window-excursion
    (qv/load-window-layout layout)
    (current-window-configuration)))

(defun qv/load-window-layout (layout)
  (delete-other-windows)

  (dolist (window (qv/load-window-layout-1 layout))
    (with-selected-window (car window)
      (let ((window-size-fixed))
        (window-resize (selected-window) (- (cadr window) (window-width)) t)
        (window-resize (selected-window) (- (caddr window) (window-height)))))))

(defun qv/load-window-layout-1 (layout)
  (pcase layout
    (`(buffer ,serialization ,width ,height)
     (qv/deserialize-buffer serialization)
     (list (list (selected-window) width height)))
    (`(,direction . ,layouts)
     (let* ((split-func (if (eq direction 'horizontal) #'split-window-right #'split-window-below))
            (windows (reverse (--map (funcall split-func) (cdr layouts))))
            (all-wins (qv/load-window-layout-1 (car layouts)))) ; First, load the first layout
       (dotimes (idx (length (cdr layouts)))
         (select-window (nth idx windows))
         (setq all-wins (append all-wins (qv/load-window-layout-1 (nth idx (cdr layouts))))))
       all-wins))))


;;; Serialize buffers
(defun qv/serialize-buffer ()
  (cond (buffer-file-name (list 'file buffer-file-name))
        (dired-directory (list 'file dired-directory))
        ((eq major-mode 'vterm-mode) (list 'vterm (buffer-name) (qv/vterm-get-pwd)))
        ((member (buffer-name) '("*scratch*" "*Messages*")) (list 'buffer (buffer-name)))))

(defun qv/deserialize-buffer (serialization)
  (pcase serialization
    (`(file ,filename) (find-file filename))
    (`(vterm ,name ,dir) (let ((default-directory dir)) (multi-vterm)) (rename-buffer name))
    (`(buffer ,buffer) (switch-to-buffer buffer))
    (_ (switch-to-buffer (get-buffer-create "*scratch*")))))


;;; Getting the pwd from a vterm buffer (so hacky)
(setq qv/activity-vterm-pwd nil)
(defun qv/vterm-receive-pwd (pwd)
  (setq qv/activity-vterm-pwd pwd)
  (exit-recursive-edit))

(push '("qv/vterm-receive-pwd" qv/vterm-receive-pwd) vterm-eval-cmds)

(defun qv/vterm-get-pwd ()
  (vterm-send-string "printf \"\\e]51;E qv/vterm-receive-pwd %s \\e\\\\\" $PWD")
  (vterm-send-string "")

  (recursive-edit)
  qv/activity-vterm-pwd)

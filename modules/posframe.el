(qv/package posframe)

(setq qv/posframe nil)

(defun qv/posframe-show (buf)
  (when (frame-live-p qv/posframe)
    (delete-frame qv/posframe))

  (setq qv/posframe
        (posframe-show
         (get-buffer-create buf)
         :poshandler #'posframe-poshandler-frame-center
         :width 120 :height 25
         :border-width 3 :border-color (qv/color black)
         :respect-mode-line t)))

(defun qv/posframe-select ()
  (interactive)
  (select-frame qv/posframe)

  (when display-line-numbers-mode
    (display-line-numbers-mode 1))

  (setq cursor-type 'box))


(defun qv/posframe-exit ()
  (interactive)
  (when posframe--frame
    (delete-frame posframe--frame)))

(defun qv/posframe-eval ()
  (interactive)
  (let ((expr (buffer-substring (point) (save-excursion (backward-sexp) (point)))))
    (delete-frame posframe--frame)
    (qv/eval expr)))


;;; Display buffer

(defun qv/posframe-display-buffer (buffer alist)
  (qv/posframe-show buffer)
  (qv/posframe-select)
  ;; (window--display-buffer buffer (selected-window) 'frame alist)
  )


;;; Point Poshandler

(defun qv/point-poshandler (params)
  (-let (((&plist :posframe-width pos-width :posframe-height pos-height
                  :parent-frame-width frame-width :parent-frame-height frame-height)
          params)
         ((x . y) (window-absolute-pixel-position)))

    (setq x (+ x 10))
    (when (> (+ x pos-width) frame-width)
      (setq x (- frame-width pos-width)))

    (cons (- x 10) (+ y (line-pixel-height)))))


;;; Keymap

(qv/keys qv/posframe-map
  :sparse t
  "<normal> q" qv/posframe-exit
  "<C-return>" qv/posframe-eval)

(push (cons '(frame-live-p posframe--frame) qv/posframe-map) minor-mode-map-alist)

(setq minor-mode-map-alist (--filter (not (equal (car it) '(frame-live-p posframe--frame)))
                                     minor-mode-map-alist
                                     )
      )

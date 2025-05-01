f_c(z) = z^2 + c
f_c(f_c(f_c(f_c(0))))


(defun mandelbrot-value (re im)
  (let* ((norm (+ (* re re) (* im im)))
         (min (/ norm 1000.0)) (max (* norm 1000.0))
         (zre re) (zim im)
         (n 0) (ret nil) (znorm nil))
    (while (and (< n 35) (null ret))
      (setq zre (+ (- (* zre zre) (* zim zim)) re)
            zim (+ (* 2 zre zim) im)
            znorm (+ (* zre zre) (* zim zim))
            ret (cond ((< znorm min) 0) ( (> znorm max) n))
            n (1+ n)))
    (or ret 0)))


(bz/keys mandelbrot-mode-map
  :sparse t)

(defvar mandelbrot-default-viewport (list -2 0.5 -1.1 1.1))

(define-derived-mode mandelbrot-mode nil "Mandelbrot Mode"
  "Display an interactive mandelbrot set."

  )

(defun mandelbrot-insert (&optional scale viewport)
  (interactive)
  (setq scale (or scale 1))
  (setq viewport (or viewport mandelbrot-default-viewport))
  (pcase-let ((`(,xmin ,xmax ,ymin ,ymax) viewport))
    (let ((inhibit-read-only t) (inhibit-modification-hooks t)
          (dx (* scale (/ (- xmax xmin) (* 0.9 (window-width)))))
          (dy (* scale (/ (- ymax ymin) (* 0.9 (window-height)))))
          (is (lambda (x y) (if (eq (mandelbrot-value x y) 0) 1 0)))
          (cs " ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█")
          tl tr bl br)
      (delete-region (point-min) (point-max))

      (dolist (y (number-sequence ymax ymin (- dy)))
        (dolist (x (number-sequence xmin xmax dx))
          (setq tl (funcall is x y) tr (funcall is (+ x dx) y)
                bl (funcall is x (+ y dy)) br (funcall is (+ x dx) (+ y dy)))
          (insert (aref cs (+ tl (* tr 2) (* bl 4) (* br 8)))))
        (insert "\n"))

      (put-text-property (point-min) (point-max) 'face (list :height scale))
      (beginning-of-buffer))))

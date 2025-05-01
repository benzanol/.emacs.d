(bz/face eqn-face :family "FreeSerif" :slant italic)
(defvar eqn-string-pixel-width-function
  (if (boundp #'string-pixel-width) #'string-pixel-width
    (require 'shr) #'shr-string-pixel-width))

(defun eqn-string-width (string &optional face size)
  (let ((faces (list (when size (list :scale size)) face)))
    (funcall eqn-string-pixel-width-function (propertize string 'face faces))))

(defun eqn--move-atoms (atoms dx dy)
  (unless (eq dx 0) (dolist (atom atoms) (setcar (car atom) (+ dx (caar atom)))))
  (unless (eq dy 0) (dolist (atom atoms) (setcdr (car atom) (+ dy (cdar atom))))))

;; An ATOM is ((X . Y) PROPS...)
;; Returns ((TOP . BTM) . (WIDTH . ATOMS))
(defun eqn--elems-to-layout (elems size x y)
  (let* ((top 0) (btm 0)
         (layouts (--map (let ((lay (eqn--elem-to-layout it size x y)))
                           (setq x (+ x (cadr lay))
                                 top (max top (caar lay))
                                 btm (max btm (cdar lay)))
                           lay)
                         elems)))
    (dolist (layout layouts) (eqn--move-atoms (cddr layout) 0 (- top (caar layout))))
    (cons (cons top btm) (cons x (--reduce-from (nconc (cddr it) acc) nil layouts)))))

(defun eqn--elem-to-layout (elem size x y)
  (pcase elem
    (`(symbol ,str)
     (let* ((h size) (halfh (* h 0.5))
            (w (eqn-string-width str 'eqn-face h))
            (atom (list (cons x y) 'symbol str size)))
       (cons (cons halfh halfh) (cons w (list atom)))))

    (`(frac ,nelems ,delems)
     (pcase-let* ((`((,nt . ,nb) . (,nw . ,nats)) (eqn--elems-to-layout nelems size x y))
                  (`((,dt . ,db) . (,dw . ,dats)) (eqn--elems-to-layout delems size x (+ y nt nb))))
       (eqn--move-atoms (if (< nw dw) nats dats) (abs (* 0.5 (- nw dw))) 0)
       (cons (cons (+ nt nb) (+ dt db)) (cons (max nw dw) (nconc nats dats)))))

    (`(sup . ,elems)
     (pcase-let ((`((,top . ,btm) . (,wid . ats))
                  (eqn--elems-to-layout nelems (* 0.6 size) x y)))
       (cons (cons (+ top btm) 0) (cons wid ats))))
    (`(sub . ,elems)
     (pcase-let ((`((,top . ,btm) . (,wid . ats))
                  (eqn--elems-to-layout nelems (* 0.6 size) x y)))
       (cons (cons 0 (+ top btm)) (cons wid ats))))))

(defun eqn--atom-to-svg (atom)
  (pcase-let ((`((,x . ,y) . ,ps) atom))
    (pcase ps
      (`(symbol ,text ,size)
           (format "<text x='%s' y='%s' font-family='%s' font-size='%20'>%s</text>"
                   x y (face-attribute 'eqn-face :family) size text))
      (_ (error "Invalid atom %s" ps))
      )
  )
)

(defun eqn--elems-to-svg (elems)
  (pcase-let ((`((,top . ,btn) . (,wid . ,ats)) (eqn--elems-to-layout nelems 20 0 0)))
    (format "<svg width='%s' height='%s'>\n%s\n</svg>"
            wid (+ top btm) (s-join (--map #'eqn--atom-to-svg ats) "\n"))))


(defun bz/insert-svg (svg-content)
  "Display SVG content stored in the variable `svg-content` in the current buffer."
  (interactive "sCode: ")
  (let* ((buffer (get-buffer-create "*SVG Display*"))
         (image-data (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" svg-content))
         (image (create-image image-data 'svg t)))
    (insert-image image)))

(defun bz/insert-svg-100 (content)
  (interactive "sCode: ")
  (let ((top "<svg width='100' height='100'>")
        (btm "</svg>"))
    (bz/insert-svg (concat top "\n" content "\n" btm))))

(setq elems '((frac ((symbol "1") (symbol "11")) ((symbol "mmm")))))
(eqn--elems-to-svg elems)


(eqn-string-width "m" eqn-face)

(bz/insert-svg-100 "")

"<svg width='100' height='100' xmlns='http://www.w3.org/2000/svg'>\n                <rect width='100' height='100' fill='white'/>\n                <text x='10' y='50' font-family='FreeSerif' font-size='20' fill='black'>Hi+1</text>\n              </svg>"

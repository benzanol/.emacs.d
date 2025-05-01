(defun apply=> (implication &rest facts)
  (let (vars)

    (pcase implication
      (`(=> ,reqs ,results)
       (unless (and (vectorp reqs) (vectorp results)) (error "Not arrays"))
       (dolist (req (append req nil))
         (pcase req
           (`(In ,var ,set) req)
           )
         )
       )
      )))


(defun pf/verify-proof (known &rest steps)
  (let (vars)
    (dolist (step steps)
      (pcase step
        (`(LetIn ,var))
        )
      )
    ))



(setq pf/pred-alist ())
(setq pf/set-alist ())


(defmacro pf/defset (set-reqs set elem-reqs elem)
  `(let* ((set-reqs  (append ',set-reqs  nil)) (set ',set)
          (elem-reqs (append ',elem-reqs nil)) (elem ',elem)
          (set-name (if (listp set) (car set) set))
          (param-names (when (listp set) (cdr set))))

     (setf (alist-get set-name pf/set-alist)
           (list :params param-names :param-reqs set-reqs
                 :elem elem :elem-reqs elem-reqs))))

(defmacro pf/defpred (pred-reqs pred truth-reqs)
  `(let ((pred ',pred) (pred-reqs (append ',pred-reqs nil))
         (truth-reqs (append ',truth-reqs nil)))
     (setf (alist-get (car pred) pf/pred-alist)
           (list :params (cdr pred) :param-reqs pred-reqs
                 :truth-reqs truth-reqs))))

;; Fundamental predicates: =>, or


;; Definition of or
(def or)
;; left-or: (=> [a] [(or a b)])
;; right-or: (=> [b] [(or a b)])


;; Definition of double implication
(DefCond
  [(In a Cond) (In b Cond)]
  (<=> a b)
  [(=> a b)
   (=> b a)])

(DefSet
  []
  MetricSpace
  [(In M Set)
   (In d (Func (M M) Real))
   (=> [(In x M)] [(= (d x x) 0)])
   (=> [(In x M) (In y M)] [(= (d x y) (d y x))])
   (=> [(In x M) (In y M) (In z M)]
       [(<= (d x z) (+ (d x y) (d y z)))])]
  (M d))



;; Extensionality (definition of set equality)
(=> [(In X Set)
     (In Y Set)]
    [(<=> [(= X Y)]
          [(=> (In z)
               (<=> [(in z X)] [(in z Y)]))])])

;; Pairing
(=> [(In X Set) (In Y Set)]
    [(In Z Set)
     (=> [(In z)]
         [(<=> [(In z Z)] [(or (= z x) (= z y))])]
         )
     ]
    )


;; Axiom of choice
(func [(S set)]
      [(x S)])


;; Proof of continuity at a point p
(DefSet
  [(In (M d) Metric-space)
   (In (N r) Metric-space)
   (In p M)]
  (ContinuousFunctionAt M N p)
  [(In f (Function M N))
   (=> [(In epsilon Real) (> epsilon 0)]
       [(In delta Real)
        (> delta 0)
        (=> [(In x M) (< (d p x) delta)]
            [(< (r (f p) (f x)) epsilon)])])]
  f)

(Def [(In (M d) Metric-space)
      (In (N r) Metric-space)]
     (ContinuousFunction M N)
     [(In f (Function M N))
      (=> [(In p M)]
          [(in f (ContinuousFunctionAt M N p))])]
     f)

(qv/package clojure-mode)

(qv/package ob-clojure)
(qv/package cider)

(setq org-babel-clojure-backend 'cider)

(qv/hook clojure-mode-hook qv/clojure-setup
  (rainbow-delimiters-mode 1))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (unless org-babel-clojure-backend
    (user-error "You need to customize org-babel-clojure-backend"))
  (let* ((expanded (org-babel-expand-body:clojure body params))
	     (result-params (cdr (assq :result-params params)))
	     result)
    (setq result
	      (cond
	       ((eq org-babel-clojure-backend 'inf-clojure)
	        (ob-clojure-eval-with-inf-clojure expanded params))
	       ((eq org-babel-clojure-backend 'cider)
	        (ob-clojure-eval-with-cider expanded params))
	       ((eq org-babel-clojure-backend 'slime)
	        (ob-clojure-eval-with-slime expanded params))))
    (setq myvar2 result)
    (if (atom result) (format "%s" result)
      (mapconcat (lambda (s) (format "%s" (if (listp s) (car s) s))) result "\n"))))

;;; Vertico posframe
(bz/package vertico-posframe)
(vertico-posframe-mode 1)

;; (setq vertico-posframe-border-width 10)
;; (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center)
;; (setq vertico-posframe-width 120)
;; (setq vertico-posframe-height 24)

;; Put the posframe in the same spot as the normal minibuffer
(setq vertico-posframe-border-width 10)
(setq vertico-posframe-poshandler 'bz/posframe-bottom-poshandler)
(setq vertico-posframe-width (frame-width))
(setq vertico-posframe-height 18)

(defun bz/posframe-bottom-poshandler (info)
  (cons 0
        (- (plist-get info :parent-frame-height)
           (plist-get info :posframe-height)
           ;; (plist-get info :mode-line-height)
           ;; (plist-get info :minibuffer-height)
           )))

;; (setq marginalia-align 'left)

(bz/face vertico-posframe :bg bg2)
;; (bz/face bz/minibuffer :h 1.0)

;; (bz/hook post-command-hook bz/colorize-minibuffer
;;   (with-current-buffer " *Minibuf-1*"
;;     (setq-local line-spacing 0.2)
;;     (buffer-face-set 'bz/minibuffer)))

;; (defun marginalia-annotate-variable (cand)
;;   "Annotate variable CAND with its documentation string."
;;   (when-let (sym (intern-soft cand))
;;     (marginalia--fields
;;      ((marginalia--variable-value sym) :truncate 0.2)
;;      ((documentation-property sym 'variable-documentation)
;;       :truncate 1.0 :face 'marginalia-documentation))))

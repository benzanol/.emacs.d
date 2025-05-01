;; Options properties:
;; :name - Symbol representing the option name
;; :path - A list of symbols
;; :children - An alist of sub-options
;;
;; User options:
;; :enable - Hook for enabling the option
;; :disable - Hook for disabling the option
;; :mode - List of minor mode functions to be called with 1 on enable and 0 on disable
;; :var - Set a variable
;; :hook - List of (hook functions...) where functions are added to hook when the option is enabled
;; :advice - List of (where function advice) to advise functions when the option is enabled

;; Alist of options, where each option is a plist
(setq cfg-options nil)

;; Alist of (buffer/nil path...) to a list of changes
(setq cfg--changes)


(defun cfg-define-option (path plist)
  (setf (alist-get path cfg-options) plist))

(defmacro cfg (name &rest body)
  "Name is a symbol for a root path, or a "
  (declare (indent 1))
  (let* ((path (if (listp name) name (list name)))
         (plist (list :name (car (last path)) :path path :children nil))
         current keyword exprs)
    (while (setq current (pop body))

      (if (not (keywordp current))
          (if (null keyword) (error "No keyword at: %s" current)
            (setq exprs (nconc exprs (list current))))

        (pcase keyword
          ('nil) ; Do nothing
          (:enable  (cfg--plist-append plist :enable  `(lambda () ,@exprs)))
          (:disable (cfg--plist-append plist :disable `(lambda () ,@exprs)))
          (:hook
           (unless (--all-p (and (listp it) (car it) (symbolp (car it))) exprs)
             (error "Hook must be a list of (hook functions...), found %s" value))
           (apply #'cfg--plist-append plist :disable exprs))
          (:var
           
           )
          )))))

(defun cfg--plist-append (plist key &rest args)
  (plist-put plist key (append list (plist-get plist key)))
  )


(cfg (lsp typescript)
  :hook (org-open-at-point-functions)

  :var (lsp-typescript-preferences-quote-style "double")
  )


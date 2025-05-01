(bz/require fci)


;; (bz/hook (typescript-mode-hook js-jsx-mode-hook js-mode-hook) prettier-mode)

;; (dolist (mode '(typescript-mode js-jsx-mode js-mode))
;;   (setf (alist-get mode bz/indent-command-alist) #'prettier-prettify)
;;   )

(bz/hook prettier-mode-hook bz/prettier-setup
  (setq-local fci-rule-column 100)
  (fci-mode 1))

;; Fuck this
(bz/advise :override prettier--show-error bz/prettier-show-error (string &rest objects)
  (run-with-timer
   0 nil
   (lambda (str)
     (message "%s %s" (bz/add-face "Prettier Error:" 'error) str))
   string))

(bz/advise :override prettier-show-error bz/prettier-show-error)

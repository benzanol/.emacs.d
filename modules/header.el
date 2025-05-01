;; 
(setq frame-title-format '((:eval (qv/header-line-format))))

(setq-default header-line-format '(:eval (qv/header-line-format))
              mode-line-format nil)

(qv/face header-line mode-line)
(setq qv/modeline-buffer-color "#334455")

(defun qv/header-line-format ()
  (concat
   (propertize "" 'face (list :foreground qv/modeline-buffer-color))
   (propertize
    (if vc-mode
        (format " %s%s " (buffer-name)
                (replace-regexp-in-string "Git:" "  " vc-mode))
      (format " %s " (buffer-name)))
    'face (list :background qv/modeline-buffer-color))
   (propertize "" 'face (list :foreground qv/modeline-buffer-color))))

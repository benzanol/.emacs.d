;;; Doom modeline
(bz/package doom-modeline)

(doom-modeline-mode)
(setq doom-modeline-height 30)
(setf (alist-get 'exwm-mode all-the-icons-mode-icon-alist)
      '(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1 :face all-the-icons-purple))

(setq doom-modeline-buffer-encoding nil)

;; (setq global-mode-string '("%e" (:eval (propertize (format-time-string "%H:%M:%S %b %d ") 'face 'italic))))
(setq global-mode-string nil)

;;; Other stuff
(add-minor-mode 'bz/test "TEST")
(setq-local bz/test nil)
(doom-modeline-def-segment bz/test
  (when (flycheck-running-p)
    " "))

doom-modeline-mode-alist

(doom-modeline-def-modeline 'my-simple-line
  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  '(bz/test misc-info minor-modes input-method buffer-encoding major-mode process vcs))
(doom-modeline-set-modeline 'my-simple-line 'default)


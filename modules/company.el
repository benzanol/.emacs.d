(qv/package company)

(setq company-show-numbers t
      company-require-match nil
      company-tooltip-limit 10
      company-tooltip-margin 1
      company-idle-delay 0.0
      company-tooltip-minimum-width 30
      company-tooltip-maximum-width 50
      company-minimum-prefix-length 1
      company-tooltip-width-grow-only t
      company-auto-complete nil
      company-minimum-prefix-length 1000)

(defun qv/company-toggle-autocomplete ()
  (interactive)
  (setq company-minimum-prefix-length
        (if (eq company-minimum-prefix-length 1000) 1 1000)))

(add-hook 'insert-keymode-hook 'company-abort)

(qv/face company-tooltip :bg "#383B48")
(qv/face company-tooltip-common)
(qv/face company-tooltip-selection highlight)
(qv/face company-preview nil :fg gray2 :bg nil)
(qv/face company-preview-common company-preview)
(qv/face company-preview-search company-preview)
(qv/face company-scrollbar-fg :bg gray2)
(qv/face company-scrollbar-bg company-tooltip)

(qv/keys modeal-insert-map "<C-tab>" company-complete)

(qv/keys company-active-map
  :sparse t
  "<tab>" company-complete-selection
  "<backtab>" company-complete-common
  "C-g" company-abort
  "C-j" company-next-page
  "C-k" company-previous-page
  "C-M-j" company-select-last
  "C-M-k" company-select-first
  "C-S-j" (dotimes (i 4) (company-select-next))
  "C-S-k" (dotimes (i 4) (company-select-previous))
  "M-j" company-select-next
  "M-k" company-select-previous
  "M-J" (dotimes (i 4) (company-select-next))
  "M-K" (dotimes (i 4) (company-select-previous)))

(dotimes (i 10)
  (define-key company-active-map (kbd (format "M-%s" (% (1+ i) 10)))
    (eval `(lambda () (interactive) (company--complete-nth ,i)))))

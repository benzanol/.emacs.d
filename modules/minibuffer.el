;;; Vertico
(qv/package vertico)

(vertico-mode 1)

(setq vertico-count 15)

(qv/keys minibuffer-local-map
  :sparse t
  "C-j" next-history-element
  "C-k" previous-history-element
  "<return>" exit-minibuffer
  "<C-return>" exit-minibuffer
  "C-g" keyboard-escape-quit
  "<tab>" minibuffer-complete)

(setcdr vertico-map nil)
(qv/keys vertico-map
  :sparse t
  :parent minibuffer-local-map
  [remap qv/down] vertico-next
  [remap qv/up] vertico-previous
  [remap qv/down4] (dotimes (i 4) (vertico-next))
  [remap qv/up4] (dotimes (i 4) (vertico-previous))
  [remap exit-minibuffer] vertico-exit
  [remap minibuffer-complete] vertico-insert)

;;; Prescient
(qv/package prescient)

(prescient-persist-mode)
(setq prescient-sort-full-matches-first t
      prescient-save-file "~/.emacs.d/.cache/prescient/prescient-save-el")

;;; Orderless
(qv/package orderless)
(setq completion-styles '(orderless))


;;; Marginalia
(qv/package marginalia)
(marginalia-mode 1)

(setq qv/marginalia-variable-faces
      '(marginalia-documentation))

(dolist (face (face-list))
  (when (string-match-p "\\`marginalia-" (format "%s" face))
    (set-face-attribute face nil :height 1.0)
    (unless (memq face qv/marginalia-variable-faces)
      (let ((inheritance (face-attribute face :inherit)))
        (set-face-attribute
         face nil :family "Iosevka" :height 72)))))

(defun marginalia--truncate (str width)
  "Truncate string STR to WIDTH."
  (let ((truncate-string-ellipsis #("..." 0 3 (face 'fixed-pitch))))
    (truncate-string-to-width
     (if-let (pos (string-match-p "\n" str))
         (substring str 0 pos)
       str)
     width 0 32 t)))

;;; Embark
(qv/package embark)

(qv/key minibuffer-local-map "C-a" embark-act)

(setq embark-prompter 'embark-completing-read-prompter)


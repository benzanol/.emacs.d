;; -*- lexical-binding: t; -*-

(require 'visual-fill-column)


(setq-default visual-fill-column-width 100)

(bz/keys *
  "C-|" ((visual-fill-column-mode)
         (variable-pitch-mode)))

(bz/hook visual-fill-column-mode-hook visual-line-mode)

(bz/keys visual-fill-column-mode-map
  ;; :sparse t)
  "C-<" (@ bz/vfc-grow
           (setq visual-fill-column-width
                 (max 10 (- (or visual-fill-column-width 100) 5)))
           (visual-fill-column-adjust))
  "C->" (@ bz/vfc-shrink
           (setq visual-fill-column-width (+ (or visual-fill-column-width 100) 5))
           (visual-fill-column-adjust))
  "C-|" (@ bz/vfc-center
           (setq visual-fill-column-center-text (not visual-fill-column-center-text))
           (visual-fill-column-adjust)))


;;; Provide

(provide 'bz-visual-column)
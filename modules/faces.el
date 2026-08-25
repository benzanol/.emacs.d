;;; Faces  -*- lexical-binding: t; -*-

(require 'bz-base)


(defvar bz/font-size 120)


;; High Transparency
;; (bz/set-colors bg "#00040A" bg2 "#00040A" bg3 "#00040A")


;;; Basic Faces

(when (display-graphic-p)
  (bz/face default :fg fg :bg bg :family "Iosevka Nerd Font" :weight normal :height ,bz/font-size)

  (bz/face variable-pitch :f "IBM Plex Sans Condensed")
  (setf (alist-get "IBM Plex Sans Condensed" face-font-rescale-alist nil nil #'equal) 1.10)
  (bz/face serif :f "Liberation Serif" :h 1.1)
  ;; (bz/face variable-pitch :f "Inter Regular" :h 1.05)

  (bz/face fixed-pitch :f "Iosevka Nerd Font")
  (bz/face fixed-pitch-serif fixed-pitch :f "Iosevka"))

;; Use DejaVu Sans Mono for all symbol characters. This is because
;; Iosevka renders many symbols as width=2 when they are normally
;; width=1, which messes up lots of ascii art. Emacs also often thinks
;; these characters are width 1 in `string-width'. Also, scale the
;; font down to 0.9 so that the width of characters is 8 pixels
;; (matching Iosevka.)
(setq use-default-font-for-symbols nil)
(setf (alist-get "DejaVu Sans Mono" face-font-rescale-alist nil nil #'equal) 0.9)
(set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend)
;; Display box drawing characters as iosevka, because the scaled-down
;; DejaVu Sans versions don't fill up the full height.
(set-fontset-font t '(#x2500 . #x259F) "Iosevka Nerd Font" nil 'prepend)
(set-fontset-font t ?❯ "Iosevka Nerd Font" nil 'prepend)

(set-fontset-font t (cons (- ?🅃 100) (+ ?🅃 100)) "Iosevka Nerd Font" nil 'prepend)

(dolist (range '((#xe000 . #xf8ff)      ; private use area
                 (#xf0000 . #xffffd)))  ; nerd font supplementary
  (set-char-table-range char-width-table range 2))


(bz/face region :bg gray3)
(bz/face highlight :bg gray3)
(bz/face show-paren-match :bg gray3 :w bold)

(bz/face shadow :fg gray2)
(bz/face link :fg blue)
(bz/face error :fg "#FF4444")
(bz/face cursor :bg "#BBBBCC")

(bz/face line-number fixed-pitch :fg gray2 :h 0.9)
(bz/face line-number-current-line line-number :fg gray1 :w bold)


;;; Layout Faces

(bz/face mode-line :fg fg :bg bg2 :box nil)
(bz/face mode-line-inactive :fg gray2 :bg bg2 :box nil)
(bz/face fringe :bg bg2 :fg gray3)

(bz/face header-line :fg fg :bg bg2 :box nil)

(bz/face separator-line :bg nil :strike-through ,(bz/color gray3))

(if (display-graphic-p)
    (bz/face vertical-border fringe :iv t)
  (bz/face vertical-border :fg gray2 :s italic))

(bz/face doom-modeline-buffer-modified error :bg unspecified :w bold)


(bz/face web-mode-html-tag-face :fg "#DF6644" :w bold :s normal)


(bz/face scroll-bar)


;;; Provide

(provide 'bz-faces)

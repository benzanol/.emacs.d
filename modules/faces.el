;;; Colors
(qv/set-colors
 fg     "#B0C0CC"
 bg     "#1B1F26"
 bg2    "#14161B"
 bg3    "#1F252E"
 gray1  "#98A8B0"
 gray2  "#7C8084"
 gray3  "#484B54"
 black  "#0E1216"
 red    "#D75F5F"
 yellow "#FFD760"
 orange "#FFA500"
 green  "#58D020"
 cyan   "#5FFFD7"
 blue   "#5FAFD7"
 purple "#AF87D7")

(unless (display-graphic-p)
  (qv/set-colors
   bg "#000000"
   bg2 "#000000"
   bg3 "#000000"))

;;; Basic Faces
(when (display-graphic-p)
  (qv/face default :fg fg :bg bg :f "Iosevka" :w normal :h 64)
  (qv/face variable-pitch :f "IBM Plex Sans Condensed" :h 1.05)
  (qv/face serif :f "Droid Serif")
  (qv/face fixed-pitch :f "Iosevka" :w normal :h 0.95)
  (qv/face fixed-pitch-serif fixed-pitch :f "Iosevka"))

(qv/face region :bg gray3)
(qv/face highlight :fg "#EECC44" :bg nil :w semibold :u "#EECC44" :e nil)
(qv/face shadow :fg gray2)
(qv/face link :fg blue)
(qv/face error :fg "#FF4444")

(qv/face line-number fixed-pitch :fg gray2 :h 0.95)
(qv/face line-number-current-line line-number :fg gray1 :w bold)

;;; Layout Faces
(qv/face mode-line :fg fg :bg bg2 :b nil :iv nil)
(qv/face mode-line-inactive :fg gray2 :bg bg2 :b nil :iv nil)
(qv/face fringe mode-line-inactive :bg nil)

(if (display-graphic-p)
    (qv/face vertical-border fringe :iv t)
  (qv/face vertical-border :fg gray2 :s italic))

(qv/face doom-modeline-buffer-modified (error bold))

;;; Font Lock Faces
;; Purple and Blue
(qv/face font-lock-comment-face :fg gray2 :w bold :s italic)
(qv/face font-lock-string-face :fg green :s normal)
(qv/face font-lock-type-face :fg blue)
(qv/face font-lock-keyword-face :fg purple :w bold)
(qv/face font-lock-function-name-face :fg red)
(qv/face font-lock-variable-name-face :fg fg :s italic)
(qv/face font-lock-constant-face :fg blue)
(qv/face font-lock-builtin-face :fg blue)
(qv/face font-lock-doc-face font-lock-comment-face)

;; Superman Colors
;; (qv/face1 'font-lock-comment-face nil qv/gray2-color nil
;;          :weight 'bold
;;          :slant 'italic)
;; (qv/face1 'font-lock-string-face nil qv/green-color nil
;;          :slant 'italic)
;; (qv/face1 'font-lock-type-face nil qv/yellow-color nil)
;; (qv/face1 'font-lock-keyword-face nil qv/yellow-color nil)
;; (qv/face1 'font-lock-function-name-face nil qv/red-color nil)
;; (qv/face1 'font-lock-variable-name-face nil qv/red-color nil)
;; (qv/face1 'font-lock-constant-face nil qv/blue-color nil)
;; (qv/face1 'font-lock-builtin-face nil qv/blue-color nil)


;;; Global Font Size
(defun qv/change-face-height (face increment)
  (set-face-attribute face nil :height
                      (+ (face-attribute face :height) increment)))

(qv/keys *
  "C-+" (qv/change-face-height 'default +16)
  "C-_" (qv/change-face-height 'default -16))

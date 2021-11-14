;;; Colors
(setq qv/fg     "#B0C0CC"
      qv/bg     "#1B1F26"
      qv/bg2    "#14161B"
      qv/bg3    "#1F252E"
      qv/gray1  "#98A8AC"
      qv/gray2  "#7C8084"
      qv/gray3  "#484B54"
      qv/black  "#0E1216"
      qv/red    "#D75F5F"
      qv/yellow "#FFD75F"
      qv/orange "#FFA500"
      qv/green  "#40E040"
      qv/cyan   "#5FFFD7"
      qv/blue   "#5FAFD7"
      qv/purple "#AF87D7")

(unless (display-graphic-p)
  (setq qv/bg "#000000"
        qv/bg2 "#222222"
        qv/bg3 "#000000"))

;;; Basic Faces
(qv/face default :fg ,qv/fg :bg ,qv/bg :f "Iosevka" :w normal :h 130)
(qv/face variable-pitch :f "Attractive" :w normal :h 1.05)
(qv/face fixed-pitch :f "Iosevka" :w normal :h 1.0)

(qv/face region :bg ,qv/gray3)
(qv/face highlight :fg "#EECC44" :w bold :u "#EECC44" :e nil)
(qv/face shadow :fg ,qv/gray2)
(qv/face link :fg ,qv/blue)
(qv/face error :fg "#FF4444")
(qv/face minibuffer-prompt :fg ,qv/blue :w bold)

(qv/face line-number fixed-pitch :fg ,qv/gray2 :h 0.9)
(qv/face line-number-current-line line-number :fg ,qv/gray1 :w bold)

;;; Layout Faces
(qv/face mode-line :fg ,qv/fg :bg ,qv/bg2 :b nil :iv nil)
(qv/face mode-line-inactive :fg ,qv/gray2 :bg ,qv/bg2 :b nil :iv nil)
(qv/face fringe mode-line-inactive)
(qv/face vertical-border mode-line-inactive :fg ,qv/bg2)

(qv/face doom-modeline-buffer-modified (error bold))

;;; Font Lock Faces
;; Purple and Blue
(qv/face font-lock-comment-face :fg ,qv/gray2 :w bold :s italic)
(qv/face font-lock-string-face :fg ,qv/green :s italic)
(qv/face font-lock-type-face :fg ,qv/blue)
(qv/face font-lock-keyword-face :fg ,qv/purple :w bold)
(qv/face font-lock-function-name-face :fg ,qv/red)
(qv/face font-lock-variable-name-face :fg ,qv/fg :s italic)
(qv/face font-lock-constant-face :fg ,qv/blue)
(qv/face font-lock-builtin-face :fg ,qv/blue)
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
(defun change-font-size (face increment)
  (set-face-attribute face nil :height
                      (+ (face-attribute face :height) increment)))

(qv/keys *
  "C-+" (change-font-size 'default 16)
  "C-_" (change-font-size 'default -16))

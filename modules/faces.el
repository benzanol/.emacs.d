;;; Colors

(setq bz/color-mode nil)
(setq bz/font-size 120)


(defun bz/dark-mode ()
  (interactive)
  (setq bz/color-mode 'dark)
  (ignore-errors (bz/set-opacity 0.85))
  (bz/face bz/org-text variable-pitch :h 1.0)

  (bz/set-colors
   fg        "#B0C0CC"
   bg        "#1B1F26"
   bg2       "#14161B"
   bg3       "#343846"
   bg4       "#272B34"
   gray1     "#98A8B0"
   gray2     "#848890"
   gray3     "#505464"
   gray4     "#484B5B"

   black     "#0E1216"
   red       "#D75F5F"
   yellow    "#FFD760"
   orange    "#FFA500"
   green     "#5dc400"
   cyan      "#5FFFD7"
   blue      "#5FAFD7"
   purple    "#AF87D7"
   highlight "#FF9955"))
(bz/dark-mode)

(defun bz/light-mode ()
  (interactive)
  (setq bz/color-mode 'light)
  (ignore-errors (bz/set-opacity 1.0))
  (bz/face bz/org-text variable-pitch :family "FreeSerif" :h 1.15)

  (bz/set-colors
   fg        "#223344"
   bg        "white"
   bg2       "#F0F1F2"
   bg3       "#DCE0E6"
   bg4       "#F3F3F5"
   gray1     "#484B5B"
   gray2     "#848890"
   gray3     "#98A8B0"
   gray4     "#C8D0E0"

   yellow    "gold"
   orange    "orange2"
   green     "#009010"
   blue      "#2F5FC7"
   red       "#C73F3F"
   purple    "#A067B7"))


(defun bz/toggle-color-mode ()
  (interactive)
  (if (eq bz/color-mode 'light) (bz/dark-mode) (bz/light-mode)))

;; High Transparency
;; (bz/set-colors bg "#00040A" bg2 "#00040A" bg3 "#00040A")


;;; Basic Faces

(when (display-graphic-p)
  (bz/face default :fg fg :bg bg :family "Iosevka Nerd Font" :weight normal :height ,bz/font-size)
  (bz/face variable-pitch :f "IBM Plex Sans Condensed" :h 1.05)
  (bz/face serif :f "Droid Serif")
  (bz/face fixed-pitch :f "Iosevka Nerd Font" :h 0.95)
  (bz/face fixed-pitch-serif fixed-pitch :f "Iosevka"))

(bz/face region :bg gray3)
(bz/face highlight :bg gray3)
(bz/face show-paren-match :bg gray3 :w bold)

(bz/face shadow :fg gray2)
(bz/face link :fg blue)
(bz/face error :fg "#FF4444")
(bz/face cursor :bg "#BBBBCC")

(bz/face line-number fixed-pitch :fg gray2 :h 0.95)
(bz/face line-number-current-line line-number :fg gray1 :w bold)

;;; Layout Faces
(bz/face mode-line :fg fg :bg bg2 :box (:color ,(bz/color gray3)))
(bz/face mode-line-inactive :fg gray2 :bg bg2 :box (:color ,(bz/color gray3)))
(bz/face fringe :bg bg2)

(bz/face header-line :bg bg2 :box (:color ,(bz/color gray3)))

(if (display-graphic-p)
    (bz/face vertical-border fringe :iv t)
  (bz/face vertical-border :fg gray2 :s italic))

(bz/face doom-modeline-buffer-modified error :w bold)

;;; Font Lock Faces
(bz/face font-lock-comment-face :fg gray2 :w bold :s italic)
(bz/face font-lock-string-face :fg green :s normal)
(bz/face font-lock-type-face :fg blue)
(bz/face font-lock-keyword-face :fg purple :w bold)
(bz/face font-lock-function-name-face :fg red)
(bz/face font-lock-variable-name-face :fg fg :s italic)
(bz/face font-lock-constant-face :fg blue)
(bz/face font-lock-builtin-face :fg blue)
(bz/face font-lock-doc-face font-lock-comment-face)


(bz/face web-mode-html-tag-face :fg "#FF6644" :w bold)



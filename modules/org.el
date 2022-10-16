(qv/package org)

;;; Configuration
(qv/hook org-mode-hook qv/org-mode-setup
  (buffer-face-set 'qv/org-text)
  (setq line-spacing 0.2)
  (display-line-numbers-mode 0))

(font-lock-add-keywords
 'org-mode
 '(;; Display dashes as bullets
   ("^ *\\([-]\\) "
    (0 (ignore (compose-region (match-beginning 1) (match-end 1) "•"))))
   ;; Make empty lines half height
   ("\\(^\n\\)" (1 '(:height 0.5)))
   ;; Make indentation noticable
   ("^\\( +\\)" (1 'fixed-pitch))
   ("^\\(\\**\\)\\(\\* \\)\\([^:\n]*[^ \n]:\\) "
    (3 'qv/org-small-header))))

(setq org-ellipsis " ➾")
(setq org-src-tab-acts-natively nil)
(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-pretty-entities nil)
(setq org-adapt-indentation nil)
(setq org-indirect-buffer-display 'current-window)

;;; Outline Faces
(qv/face qv/org-text variable-pitch)

(qv/face qv/org-header qv/org-text :h 1.0 :w bold :u nil)
(qv/face qv/org-small-header qv/org-text :h 1.0 :w bold :u t)

(qv/face org-document-title qv/org-header :fg fg :h 1.7 :u nil :s italic)
(qv/face org-document-info qv/org-header :fg fg :h 1.3 :u nil)

(qv/face org-level-1 qv/org-header :u t)
(qv/face org-level-2 qv/org-header :u t)
(qv/face org-level-3 qv/org-header :u t)
(qv/face org-level-4 qv/org-header :u t)
(qv/face org-level-5 qv/org-header :u t)

;;; Special Faces
(qv/face org-hide :h 1.0 :fg bg)
(qv/face org-special-keyword fixed-pitch :fg gray2 :w bold)
(qv/face org-meta-line fixed-pitch :fg gray2 :h 0.8)
(qv/face org-document-info-keyword fixed-pitch :fg gray2 :h 0.8)
(qv/face org-drawer org-meta-line :fg nil)
(qv/face org-table fixed-pitch :fg yellow)
(qv/face org-column org-table :bg nil)
(qv/face org-verbatim fixed-pitch :fg gray2 :h 0.9)
(qv/face org-code org-verbatim :bg bg2)
(qv/face org-block fixed-pitch :bg bg2 :x t :h 0.9)
(qv/face org-block-begin-line org-block :fg gray3)
(qv/face org-block-end-line org-block :fg gray3)
(qv/face org-checkbox fixed-pitch)
(qv/face org-ellipsis :fg gray2 :u nil)

;;; Keybindings
(setq-default qv/org-lang "emacs-lisp")
(make-local-variable 'qv/org-lang)
(qv/keys org-mode-map
  :sparse t
  "C-c C-c" org-ctrl-c-ctrl-c
  "C-t" org-todo

  "<normal> SPC" outline-toggle-children
  "<normal> S-SPC" org-cycle
  "<normal> <backtab>" org-global-cycle

  "<normal> RET" org-open-at-point
  "<insert> RET" (newline) ;; Prevent indentation

  "C-k" org-move-item-up
  "C-j" org-move-item-down

  "M-a" org-do-promote
  "M-d" org-do-demote
  "C-c C-h" ((org-toggle-heading) (qv/org-update-header-overlay))
  "C-o" org-insert-heading-after-current
  "C-p" ((org-insert-heading-after-current) (org-do-demote))

  "C-a" nil
  "C-a C-l" org-insert-link

  "C-c C-b" (insert (format "#+BEGIN_SRC %s\n#+END_SRC" qv/org-lang))
  "<normal> g =" qv/format-code-block-indentation

  "C-<" ((setq visual-fill-column-width
               (max 10 (- visual-fill-column-width 5)))
         (visual-fill-column-adjust))
  "C->" ((setq visual-fill-column-width (+ visual-fill-column-width 5))
         (visual-fill-column-adjust))
  "C-c C-\\" ((setq visual-fill-column-center-text (not visual-fill-column-center-text))
             (visual-fill-column-adjust)))

;;; Hiding Text
(defvar qv/org-showing-meta-text nil
  "If non-nil, hide meta lines in org mode buffers.")

(setq qv/org-hide-exclude-keywords
      '("begin_src"
        "end_src"))

(setq qv/org-show-value-keywords
      '("title"
        "author"
        "description"))

(defun qv/org-show-meta-text (&optional state)
  "If STATE is positive, show meta text
If STATE is negative, hide meta text.
If STATE is 0, do not make any change, but make sure
that the text is being displayed/hidden properly.
Otherwise, toggle meta text."
  (interactive)
  (setq-local qv/org-showing-meta-text
              (if (numberp state)
                  (if (eq state 0) qv/org-showing-meta-text
                    (if (< state 0) nil t))
                (not qv/org-showing-meta-text)))

  (remove-overlays nil nil 'qv/hide-meta-lines t)

  (unless qv/org-showing-meta-text
    (let ((original-position (point)))

      (beginning-of-buffer)
      (while (search-forward-regexp "^#\\+[a-zA-Z]" nil t)
        (beginning-of-line)
        (let* ((case-fold-search t)
               (exclude-regexp
                (concat "\\(#\\+"
                        (string-join qv/org-hide-exclude-keywords "[: \n]\\|#\\+")
                        "[: \n]\\)"))
               (end-regexp
                (concat "\\(#\\+"
                        (string-join qv/org-show-value-keywords ":? \\|#\\+")
                        ":? \\|\n\\)"))
               (beg (point))
               (end (save-excursion (search-forward-regexp end-regexp nil t) (point)))
               (line (buffer-substring-no-properties beg end)))
          (when (string= line (replace-regexp-in-string
                               exclude-regexp "" line))
            (let ((overlay (make-overlay beg end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'qv/hide-meta-lines t)))
          (end-of-line)))

      (goto-char original-position))))
(defvar qv/org-showing-drawers nil
  "If non-nil, hide drawers in org mode buffers.")
(setq-default qv/org-showing-drawers nil)

(defun qv/org-show-drawers (&optional state)
  "If STATE is positive, show drawers
If STATE is negative, hide drawers.
If STATE is 0, do not make any change, but make sure
that drawers are being displayed/hidden properly.
Otherwise, toggle drawers."
  (interactive)
  (setq-local qv/org-showing-drawers
              (if (numberp state)
                  (if (eq state 0) qv/org-showing-drawers
                    (if (< state 0) nil t))
                (not qv/org-showing-drawers)))

  (defvar-local qv/org-drawer-overlays nil
    "Store the overlays for drawers and meta text in the current buffer")
  (mapcar 'delete-overlay qv/org-drawer-overlays)
  (setq-local qv/org-drawer-overlays nil)

  (unless qv/org-showing-drawers
    (let ((original-position (point)))

      (beginning-of-buffer)
      (while (search-forward-regexp org-drawer-regexp nil t)
        (beginning-of-line)
        (when (ignore-error t (org-element-drawer-parser nil (list (point))))
          (let* ((props (cadr (org-element-drawer-parser nil (list (point)))))
                 (beg (plist-get props ':begin))
                 (end (plist-get props ':end))
                 (overlay (make-overlay (1- beg) (1- end))))
            (overlay-put overlay 'invisible t)
            (setq-local qv/org-drawer-overlays
                        (append qv/org-drawer-overlays (list overlay)))
            (goto-char (1- end))))
        (forward-char))
      (goto-char original-position))))

(add-hook 'org-mode-hook
          (lambda ()
            (qv/org-show-meta-text -1)
            (qv/org-show-drawers 1)
            (local-set-key (kbd "C-c C-h") 'qv/org-show-meta-text)
            (local-set-key (kbd "C-c C-S-h") 'qv/org-show-drawers)))

;;; Equation Overlays
(qv/hook org-mode-hook qv/org-equation-overlays
  "Search the buffer for equations surrounded by ``, and
italicize them using an overlay so as not to invalidate
other formatting."
  (interactive)

  (remove-overlays nil nil 'qv/equation t)

  (let ((original-position (point)))
    (beginning-of-buffer)
    (while (search-forward-regexp "〈.*?〉" nil t)
      (search-backward "〈")

      (let ((overlay (make-overlay (point) (search-forward "〉"))))
        (overlay-put overlay 'face '(:slant italic :height 1.05))
        (overlay-put overlay 'qv/equation t)))

    (beginning-of-buffer)
    (while (search-forward "√" nil t)
      (let ((overlay (make-overlay (1- (point)) (point))))
        (overlay-put overlay 'face '(:slant normal))
        (overlay-put overlay 'qv/equation t)
        (when (string= (buffer-substring (point) (1+ (point))) "{")
          (overlay-put overlay 'display '((raise 0.1))))))

    (goto-char original-position)))

;;; Visual Fill Column

(qv/package visual-fill-column)

(qv/hook org-mode-hook qv/visual-fill-column-setup
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text nil)
  (setq truncate-lines t)
  (setq word-wrap t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))


;;; Inserting Items
(setq qv/insert-symbols-alist
      '(("`" . "∙")
        ("." . "·")
        (";" . "°")
        ("-" . "−")
        ("~" . "≈")
        ("+" . "±")
        ("/" . "⁄")
        ("*" . "×")
        ("m r" . "√")
        ("m i" . "∞")
        ("g a" . "∡")
        ("g A" . "∢")
        ("g t" . "Δ")
        ("g =" . "∥")
        ("g +" . "⟂")
        ("s u" . "∪")
        ("s i" . "∩")
        ("s e" . "∈")
        ("l p" . "π")
        ("l t" . "θ")
        ("f 1 /" . "⅟")
        ("f 1 2" . "½")
        ("f 1 3" . "⅓")
        ("f 2 3" . "⅔")
        ("f 1 4" . "¼")
        ("f 3 4" . "¾")
        ("f 1 5" . "⅕")
        ("f 2 5" . "⅖")
        ("f 3 5" . "⅗")
        ("f 4 5" . "⅘")
        ("f 1 6" . "⅙")
        ("f 5 6" . "⅚")
        ("f 1 7" . "⅐")
        ("f 1 8" . "⅛")
        ("f 5 8" . "⅝")
        ("f 7 8" . "⅞")
        ("f 1 9" . "⅑")))

(defface qv/delimiter '((t :height 0.1))
  "Face for easily changing whether equation delimiters (`)
are visible (full height) or invisible (tiny height).")

(font-lock-add-keywords
 'org-mode
 '(("∡" (0 '(:height 1.05)))
   ("√" (0 `(:family ,(face-attribute 'fixed-pitch ':family))))
   ("[〈〉]" (0 'qv/delimiter))
   ("\\(⌈\\)\\(.+?\\)\\(⌉\\)"
    (1 'qv/delimiter) (2 '(:overline t)) (3 'qv/delimiter))
   ("√\\({\\)\\([^}\n]*\\)\\(}\\)"
    (1 '(:overline t :inherit qv/delimiter)) (2 '(:overline t))
    (3 '(:overline t :inherit qv/delimiter)))))


;;; Org Appear
(qv/package org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autosubmarkers t)
(setq org-appear-autolinks nil)
(setq org-appear-autoentities t)

;;; Pretty Symbols
(qv/hook org-mode-hook qv/org-prettify-symbols
  (qv/face org-checkbox fixed-pitch)
  (setq-local prettify-symbols-alist
              `(("[ ]" . ?○)
                ("[X]" . ?⊙)
                ("[-]" . ?⊙)
                ("\\[" . "⟨")
                ("\\]" . "⟩")
                ("\\(" . "⟨")
                ("\\)" . "⟩")
                ("\\\\" . "⏎")
                ("\\sqrt" . "√")
                . ,prettify-symbols-alist))
  (prettify-symbols-mode 1))

(qv/face org-todo fixed-pitch :h 1.0 :w bold :fg red)
(qv/face org-done fixed-pitch :h 1.0 :w bold :fg "LawnGreen")
(qv/face org-headline-done :fg nil :u t :s italic)
(qv/face org-checkbox-statistics-todo fixed-pitch :h 0.75 :w bold :fg gray2)
(qv/face org-checkbox-statistics-done fixed-pitch :h 0.75 :w bold :fg "LawnGreen")

;;; Hide ALL Leading Stars

;; Made redundant by overlays in org indent mode

;;(setq org-hide-leading-stars nil
;;      org-hide-all-leading-stars t)
;;
;;(advice-add
;; 'org-get-level-face :filter-args
;; (lambda (args)
;;   (if (and (eq (car args) 2) org-hide-all-leading-stars)
;;       (list 1) args)))
;;
;;;;; Hide property drawers
;;(qv/hook org-mode-hook qv/org-hide-all-drawers
;;  (save-excursion
;;    (while (search-forward-regexp "^:END:$" nil t)
;;      (when (not (overlays-at (1- (point))))
;;        (org-hide-drawer-toggle)))))
;;(advice-add 'org-global-cycle :after 'qv/org-hide-all-drawers)
;;
;;(defun qv/org-hide-next-drawer ()
;;  (save-excursion
;;    (when (and (search-forward ":PROPERTIES:" (line-end-position 2) t)
;;               (not (overlays-at (point))))
;;      (org-hide-drawer-toggle))))
;;(advice-add 'outline-toggle-children :after 'qv/org-hide-next-drawer)


;;; Org Indent Mode

;;(setq org-startup-indented nil)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-indent-mode-turns-on-hiding-stars nil)

(defun qv/org-indent-refresh ()
  (interactive)
  (org-indent-mode 1))

;;;; Add an overlay to the header stars
(qv/hook outline-view-change-hook qv/org-header-overlay-timer
  (run-with-timer 0 nil 'qv/org-update-header-overlay))

(defun qv/org-update-header-overlay (&rest args)
  (save-excursion
    ;; Go to the previous heading when called with hook, but not from org indent mode
    (unless args (forward-line 1) (outline-previous-visible-heading 1))

    (remove-overlays (line-beginning-position) (line-end-position) 'hide-stars t)
    (beginning-of-line)
    (when (and (not (looking-at "\\*+ *$"))
               (search-forward-regexp org-heading-regexp (line-end-position) t))
      (let ((o (make-overlay (match-beginning 1) (1+ (match-end 1)) nil t nil)))
        (overlay-put o 'hide-stars t)
        (overlay-put o 'invisible t)
        (overlay-put o 'intangible t)
        (overlay-put o 'modification-hooks
                     '((lambda (ov &rest args) (delete-overlay ov))))

        (overlay-put
         o 'after-string
         (propertize
          (concat
           (if (let ((cur (org-current-level)))
                 (save-excursion
                   (or (= 1 (forward-line 1))
                       (and (looking-at org-heading-regexp)
                            (<= (org-current-level) cur)))))
               #("⦿ " 0 1 (display ((height 0.7) (raise 0.15))))
             (if (outline-invisible-p (line-end-position))
                 #("› " 1 2 (display (height 1.3)))
               (propertize "ˬ" 'face '(:weight bold) 'display '((raise 0.3)))))
           (propertize " " 'display '(raise 0.3)))
          'face '(:inherit qv/org-header :underline nil)))))))

;;;; Indent contents of headings
(advice-add 'org-indent-set-line-properties :override 'qv/oislp-override)
(defun qv/oislp-override (level indentation &optional heading)
  (qv/org-update-header-overlay 'dont-go-back)
  (when (not heading) ; When it isn't a heading
    (if (save-excursion ; If inside of a drawer, indent doubly
          (let* ((p (line-end-position))
                 (s (search-backward-regexp org-drawer-regexp nil t))
                 (e (search-forward-regexp org-property-end-re nil t))
                 (h (search-backward-regexp org-heading-regexp s t)))
            (and s e (not h) (> e p))))
        (setq level (+ 2 level))
      (setq level (+ 1 level))))

  (let* ((line (aref org-indent--text-line-prefixes level)))
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
			             `(line-prefix ,line wrap-prefix ,line)))
  (forward-line))

;;;; Add indent guides
(cond ((display-graphic-p)
       (setq qv/org-indent-margin 15)
       (setq qv/org-indent-width 25)
       (setq qv/org-indent-before 2)
       (setq qv/org-indent-guide-char ?\s)
       (qv/face qv/org-indent-guide :bg gray3 :h 0.4))
      (t
       (setq qv/org-indent-margin 0)
       (setq qv/org-indent-width 3)
       (setq qv/org-indent-before 0)
       (setq qv/org-indent-guide-char ?\s)
       (qv/face qv/org-indent-guide :fg gray2 :h 1.0)))

(defun qv/org-indent-guide-string (level)
  (let ((align (round (+ qv/org-indent-margin (* qv/org-indent-width (1- level))))))
    (if (= level 1)
        (propertize " " 'display `(space :align-to (,align)))
      (concat (propertize " " 'display `(space :width (,qv/org-indent-before)))
              (propertize (string qv/org-indent-guide-char) 'face 'qv/org-indent-guide)
              (propertize " " 'display `(space :align-to (,align)))))))


(advice-add 'org-indent--compute-prefixes :override
            'qv/org-indent-guide--compute-prefixes)

(defun qv/org-indent-guide--compute-prefixes ()
  (let ((prefixes (make-vector org-indent--deepest-level nil)))
    (dotimes (i org-indent--deepest-level)
      (aset prefixes i
            (if (= i 0) ""
              (concat (aref prefixes (1- i))
                      (qv/org-indent-guide-string i)))))

    (setq org-indent--heading-line-prefixes prefixes)
    (setq org-indent--inlinetask-line-prefixes prefixes)
    (setq org-indent--text-line-prefixes prefixes)
    nil))

(defun org-indent-set-line-properties (level indentation &optional heading)
  (let* ((line (aref org-indent--text-line-prefixes level)))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
			             `(line-prefix ,line wrap-prefix ,line)))
  (forward-line))


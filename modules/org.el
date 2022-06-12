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
   ;; Hide leading stars
   ("^\\*+ " (0 'org-hide))))

(qv/face org-hide :h 0.1)

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
(qv/face qv/org-header qv/org-text :w medium :h 1.0)
(qv/face org-document-title qv/org-header :fg fg :h 2.2 :u nil :s italic)
(qv/face org-document-info qv/org-header :fg fg :h 1.25)
(qv/face org-level-1 qv/org-header :h 1.7 :w bold :u nil)
(qv/face org-level-2 qv/org-header :h 1.3 :w bold :s normal :u nil)
(qv/face org-level-3 qv/org-header :h 1.05 :w bold :u t)
(qv/face org-level-4 qv/org-header :h 1.0 :u t :s italic)

;;; Special Faces
(qv/face org-special-keyword nil :fg nil :w bold)
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
  "C-c C-b" (insert (format "#+BEGIN_SRC %s\n#+END_SRC" qv/org-lang))
  "C-c C-c" org-ctrl-c-ctrl-c
  "C-c C-t" org-todo
  "{" org-move-item-up
  "}" org-move-item-down
  "RET" (if insert-keymode (newline) (org-open-at-point))
  "SPC" outline-toggle-children
  "<backtab>" org-global-cycle
  "g =" qv/format-code-block-indentation)

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
  (setq visual-fill-column-width 30)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defvar qv/org-width 0.8)
(qv/hook window-state-change-hook qv/visual-column-update
  (walk-windows
   (lambda (win)
     (with-selected-window win
       (when visual-fill-column-mode
         (let ((win (window-total-width)))
           (setq visual-fill-column-width
                 (min (round (* qv/org-width win)) (- win 8) 100))))))))

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
(setq org-appear-autolinks t)
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

;;; Org Indent Mode
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 20)
(setq org-indent-boundary-char ?|)
(qv/face org-indent :fg bg :bg bg)
(add-hook 'org-mode 'org-indent-mode)

(qv/face qv/org-header qv/org-text :w bold :h 1.05, :u nil)
(qv/face org-document-title qv/org-header :fg fg :h 1.7 :u nil :s italic)
(qv/face org-document-info qv/org-header :fg fg :h 1.3 :u nil)
(qv/face org-level-1 qv/org-header :u t)
(qv/face org-level-2 qv/org-header :u t)
(qv/face org-level-3 qv/org-header :u t)
(qv/face org-level-4 qv/org-header :u t)


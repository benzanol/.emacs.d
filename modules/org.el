(bz/package org)
(bz/package org-indent)

(bz/require visual-column)
(bz/require org-latex)
(bz/require org-export)

;;; Configuration
(bz/hook org-mode-hook bz/org-mode-setup
  (buffer-face-set 'bz/org-text)
  (setq line-spacing 0.1)
  (setq tab-width 4)
  (display-line-numbers-mode 0)
  (setq-local forward-sexp-function 'bz/org-forward-sexp-function)
  )

(font-lock-add-keywords
 'org-mode
 '(;; Make empty lines half height
   ;; ("\\(^\n\\)" (1 '(:height 0.5)))
   ;; Make indentation noticable
   ("^\\( +\\)" (1 'fixed-pitch))
   ("^\\(\\**\\)\\(\\* \\)\\([^:\n]*[^ \n]:\\) "
    (3 'bz/org-small-header))
   ("^#\\+begin_definition \\(.+\\)" (1 'bz/org-definition))
   ))

(bz/face bz/org-definition :w bold)

(setq org-ellipsis " »")
(setq org-src-tab-acts-natively nil)
(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-pretty-entities nil)
(setq org-adapt-indentation nil)
(setq org-indirect-buffer-display 'current-window)


;;; Faces
;; (bz/face bz/org-text variable-pitch :family "IBM Plex Sans Condensed")
;; (bz/face bz/org-text variable-pitch :family "FreeSerif")

(bz/face bz/org-header bz/org-text :h 0.9 :w bold :u nil)
(bz/face bz/org-small-header bz/org-text :h 1.0 :w bold :u t)

(bz/face org-document-title bz/org-header :fg fg :h 1.7 :u nil :s italic)
(bz/face org-document-info bz/org-header :fg fg :h 1.3 :u nil)

(bz/face org-level-1 bz/org-header)
(bz/face org-level-2 bz/org-header)
(bz/face org-level-3 bz/org-header)
(bz/face org-level-4 bz/org-header)
(bz/face org-level-5 bz/org-header)

(bz/face org-hide :h 1.0 :fg bg)
(bz/face org-meta-line fixed-pitch :fg gray2 :h 0.8)
(bz/face org-special-keyword org-meta-line)
(bz/face org-document-info-keyword fixed-pitch :fg gray2 :h 0.8)
(bz/face org-drawer org-meta-line :fg nil :bg bg)
(bz/face org-table fixed-pitch :fg blue)
(bz/face org-column org-table :bg nil)
(bz/face org-verbatim fixed-pitch :fg gray2 :h 0.9)
(bz/face org-code org-verbatim :bg bg2)
(bz/face org-block fixed-pitch :x t :bg bg :h 0.9)
(bz/face org-block-begin-line org-block :fg gray3 :bg bg2 :x t)
(bz/face org-block-end-line org-block :fg gray3 :bg bg2 :x t)
(bz/face org-checkbox fixed-pitch)
(bz/face org-ellipsis :fg gray2 :u nil :w light)

(bz/face org-todo fixed-pitch :h 0.8 :w bold :fg red)
(bz/face org-done fixed-pitch :h 0.8 :w bold :fg green)
(bz/face org-headline-done :fg nil)
(bz/face org-checkbox-statistics-todo fixed-pitch :h 0.75 :w bold :fg gray2)
(bz/face org-checkbox-statistics-done fixed-pitch :h 0.75 :w bold :fg green)

;;; Keybindings
(setq-default bz/org-lang "emacs-lisp")
(make-local-variable 'bz/org-lang)

(bz/keys org-src-mode-map
  "<tab>" indent-for-tab-command)

(bz/keys org-mode-map
  :sparse t
  :parent outline-minor-mode-map
  [remap bz/up] bz/outline-up
  [remap bz/down] bz/outline-down

  [remap indent-for-tab-command]
  (@ bz/org-tab
     (cond ((and (looking-back "[_^]{") (looking-at-p "}"))
            (delete-backward-char 2)
            (let ((pos (point)))
              (bz/org-tab)
              (delete-region pos (1+ pos))))
           ((eq (face-at-point) 'org-table) (search-forward-regexp "| ?"))
           ((org-in-src-block-p) (bz/org-indent-code-block))
           ((search-forward-regexp "\\= *\\\\rightarrow " nil t))
           ((search-forward "{}" (point-at-eol) t) (backward-char))
           ((search-forward-regexp "\\= *\\(\\\\right.\\|\\\\?}\\)" nil t))
           ((forward-char))))

  "C-c C-c" org-ctrl-c-ctrl-c
  [remap bz/q] org-ctrl-c-ctrl-c

  "S-<return>" (let ((s (save-excursion
                          (beginning-of-line)
                          (and (looking-at " *- \\(?:\\[.\\] \\)?") (match-string 0)))))
                 (newline) (when s (insert s)))

  "C-f C-f" lens-create-new-file
  "C-f C-p" lens-auto-at-point
  "C-f C-a" lens-auto-mode
  "C-f C-r" lens-remove
  "C-f C-d" lens-remove-all

  "C-c C-t" org-todo
  "C-w" org-todo
  "C-c C-l" org-insert-link
  "C-c C-e" org-export-dispatch
  "C-c C-i" org-toggle-inline-images

  [remap bz/fold-toggle]
  (@ bz/org-toggle-hidden
     (if (or (org-at-drawer-p) (org-at-block-p)
             (save-excursion (move-beginning-of-line 1)
                             (looking-at "#\\+\\(begin\\|end\\)_")))
         (org-cycle)
       (outline-toggle-children)))
  [remap bz/fold-toggle-all] lens-fold-toggle-all

  [remap bz/click] org-open-at-point
  [remap newline] (newline) ;; Prevent indentation

  "C-c C-n" org-narrow-to-element
  "C-c C-w" widen

  "C-k" org-move-item-up
  "C-j" org-move-item-down

  [remap bz/shift-left]
  (@ bz/org-shift-left
     (if (org-at-heading-p) (org-do-promote)
       (bz/shift-left)))
  [remap bz/shift-right]
  (@ bz/org-shift-right
     (if (org-at-heading-p) (org-do-demote)
       (bz/shift-right)))

  "C-c C-h" org-toggle-heading
  "C-o" (@ bz/org-insert-heading
           (org-insert-heading-after-current)
           (bz/insert-keymode))
  "C-S-o" (@ bz/org-insert-subheading
             (org-insert-heading-after-current)
             (org-do-demote)
             (bz/insert-keymode))

  "C-c C-b" (@ bz/org-insert-source-block
               (insert (format "#+begin_src %s\n#+end_src" bz/org-lang)))
  "C-c C-S-b" (@ bz/org-set-language
                 (setq-local bz/org-lang (read-string "Set Language: ")))

  [remap bz/format-buffer]
  (@ bz/org-indent-code-block
     (org-edit-special)
     (indent-region (point-min) (point-max))
     (org-edit-src-exit))

  "C-t C-t" org-table-create
  "C-t C-r" org-table-insert-row
  "C-t C--" org-table-insert-hline
  "C-t C-j" org-table-move-row-down
  "C-t C-k" org-table-move-row-up
  "C-t C-c" org-table-insert-column
  "C-t C-d" org-table-delete-column
  "C-t C-h" org-table-move-column-left
  "C-t C-l" org-table-move-column-right
  "C-t d" ((org-table-insert-row)
           (insert (format-time-string "%Y-%m-%d"))
           (org-ctrl-c-ctrl-c))
  )


;;; Hiding Text
(defvar bz/org-showing-meta-text nil
  "If non-nil, hide meta lines in org mode buffers.")

(setq bz/org-hide-exclude-keywords
      '("begin_src"
        "end_src"))

(setq bz/org-show-value-keywords
      '("title"
        "author"
        "description"))

(defun bz/org-show-meta-text (&optional state)
  "If STATE is positive, show meta text
If STATE is negative, hide meta text.
If STATE is 0, do not make any change, but make sure
that the text is being displayed/hidden properly.
Otherwise, toggle meta text."
  (interactive)
  (setq-local bz/org-showing-meta-text
              (if (numberp state)
                  (if (eq state 0) bz/org-showing-meta-text
                    (if (< state 0) nil t))
                (not bz/org-showing-meta-text)))

  (remove-overlays nil nil 'bz/hide-meta-lines t)

  (unless bz/org-showing-meta-text
    (let ((original-position (point)))

      (beginning-of-buffer)
      (while (search-forward-regexp "^#\\+[a-zA-Z]" nil t)
        (beginning-of-line)
        (let* ((case-fold-search t)
               (exclude-regexp
                (concat "\\(#\\+"
                        (string-join bz/org-hide-exclude-keywords "[: \n]\\|#\\+")
                        "[: \n]\\)"))
               (end-regexp
                (concat "\\(#\\+"
                        (string-join bz/org-show-value-keywords ":? \\|#\\+")
                        ":? \\|\n\\)"))
               (beg (point))
               (end (save-excursion (search-forward-regexp end-regexp nil t) (point)))
               (line (buffer-substring-no-properties beg end)))
          (when (string= line (replace-regexp-in-string
                               exclude-regexp "" line))
            (let ((overlay (make-overlay beg end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'bz/hide-meta-lines t)))
          (end-of-line)))

      (goto-char original-position))))
(defvar bz/org-showing-drawers nil
  "If non-nil, hide drawers in org mode buffers.")
(setq-default bz/org-showing-drawers nil)

(defun bz/org-show-drawers (&optional state)
  "If STATE is positive, show drawers
If STATE is negative, hide drawers.
If STATE is 0, do not make any change, but make sure
that drawers are being displayed/hidden properly.
Otherwise, toggle drawers."
  (interactive)
  (setq-local bz/org-showing-drawers
              (if (numberp state)
                  (if (eq state 0) bz/org-showing-drawers
                    (if (< state 0) nil t))
                (not bz/org-showing-drawers)))

  (defvar-local bz/org-drawer-overlays nil
    "Store the overlays for drawers and meta text in the current buffer")
  (mapcar 'delete-overlay bz/org-drawer-overlays)
  (setq-local bz/org-drawer-overlays nil)

  (unless bz/org-showing-drawers
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
            (setq-local bz/org-drawer-overlays
                        (append bz/org-drawer-overlays (list overlay)))
            (goto-char (1- end))))
        (forward-char))
      (goto-char original-position))))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (bz/org-show-meta-text -1)
;;             (bz/org-show-drawers 1)
;;             (local-set-key (kbd "C-c C-h") 'bz/org-show-meta-text)
;;             (local-set-key (kbd "C-c C-S-h") 'bz/org-show-drawers)))


;;; Equation Overlays
(bz/hook org-mode-hook bz/org-equation-overlays
  "Search the buffer for equations surrounded by ``, and
italicize them using an overlay so as not to invalidate
other formatting."
  (interactive)

  (remove-overlays nil nil 'bz/equation t)

  (let ((original-position (point)))
    (beginning-of-buffer)
    (while (search-forward-regexp "〈.*?〉" nil t)
      (search-backward "〈")

      (let ((overlay (make-overlay (point) (search-forward "〉"))))
        (overlay-put overlay 'face '(:slant italic :height 1.05))
        (overlay-put overlay 'bz/equation t)))

    (beginning-of-buffer)
    (while (search-forward-regexp "√\\|\\\\sqrt" nil t)
      (let ((overlay (make-overlay (1- (point)) (point))))
        (overlay-put overlay 'face '(:slant normal))
        (overlay-put overlay 'bz/equation t)
        (when (string= (buffer-substring (point) (1+ (point))) "{")
          (overlay-put overlay 'display '((raise 0.1))))))

    (goto-char original-position)))


;;; Visual Fill Column

(bz/package visual-fill-column)

(bz/hook org-mode-hook bz/visual-fill-column-setup
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text nil)
  (setq truncate-lines t)
  (setq word-wrap t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))


;;; Org Appear
(bz/package org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autosubmarkers t)
(setq org-appear-autolinks t)
(setq org-appear-autoentities t)


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
;;; Forward sexp func

(defun bz/org-forward-sexp-function (arg)
  (let* ((next (or (scan-sexps (point) arg) (buffer-end arg)))
         (ol (--find (eq (car (overlay-get it 'display)) 'image)
                     (overlays-at next))))
    (goto-char (cond ((null ol) next)
                     ((< arg 0) (1- (overlay-start ol)))
                     ((overlay-end ol))))))


;;; Hide property drawers
;;(bz/hook org-mode-hook bz/org-hide-all-drawers
;;  (save-excursion
;;    (while (search-forward-regexp "^:END:$" nil t)
;;      (when (not (overlays-at (1- (point))))
;;        (org-hide-drawer-toggle)))))
;;(advice-add 'org-global-cycle :after 'bz/org-hide-all-drawers)
;;
;;(defun bz/org-hide-next-drawer ()
;;  (save-excursion
;;    (when (and (search-forward ":PROPERTIES:" (line-end-position 2) t)
;;               (not (overlays-at (point))))
;;      (org-hide-drawer-toggle))))
;;(advice-add 'outline-toggle-children :after 'bz/org-hide-next-drawer)



;;; Org Indent Mode

;;(setq org-startup-indented nil)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-indent-mode-turns-on-hiding-stars nil)

;; Fixes issues with ⦿ on nixos
(set-fontset-font t '(#x29BF . #x29BF) "DejaVu Sans")

(defun bz/org-indent-refresh ()
  (interactive)
  (org-indent-mode 1))

(cond ((display-graphic-p)
       (setq bz/org-indent-margin 15)
       (setq bz/org-indent-width 25)
       (setq bz/org-indent-before 2)
       (setq bz/org-indent-guide-char ?\s)
       (bz/face bz/org-indent-guide :bg gray3 :fg gray3))
      (t
       (setq bz/org-indent-margin 0)
       (setq bz/org-indent-width 3)
       (setq bz/org-indent-before 0)
       (setq bz/org-indent-guide-char ?\s)
       (bz/face bz/org-indent-guide :fg gray2 :h 1.0)))

(defun bz/org-indent-guide-string (level)
  (let ((align (round (+ bz/org-indent-margin (* bz/org-indent-width (1- level))))))
    (if (= level 1)
        (propertize " " 'display `(space :align-to (,align) :height (1)))
      (concat (propertize " " 'display `(space :width (,bz/org-indent-before) :height (1)))
              (propertize (string bz/org-indent-guide-char)
                          'display '(space :height (1) :width (1))
                          'face 'bz/org-indent-guide)
              (propertize " " 'display `(space :align-to (,align) :height (1)))))))

(bz/advise :override org-indent--compute-prefixes bz/org-indent--compute-prefixes ()
  (let ((n org-indent--deepest-level) prefixes)
    (dotimes (i (+ 2 n))
      (push (if (= i 0) "" (concat (car prefixes) (bz/org-indent-guide-string i)))
            prefixes))
    (setq prefixes (reverse prefixes))

    (setq org-indent--heading-line-prefixes (apply #'vector (-slice prefixes 0 n)))
    (setq org-indent--text-line-prefixes (apply #'vector (-slice prefixes 1 (1+ n))))
    (setq org-indent--inlinetask-line-prefixes org-indent--text-line-prefixes)
    (setq bz/org-indent--wrap-prefixes (apply #'vector (-slice prefixes 1 (+ 1 n))))
    ;; (setq bz/org-indent--wrap-prefixes (apply #'vector (-slice prefixes 2 (+ 2 n))))
    nil))


(defun bz/alter-other-text-property (from to prop setprop func &optional object)
  "Iterates over values of PROP, but changes the value of SETPROP.
This code is copied directly from alter-text-property with minimal modification."
  (let ((begin from) end val)
    (while (setq val (get-text-property begin prop object)
                 end (text-property-not-all begin to prop val object))
      (put-text-property begin end setprop (funcall func val) object)
      (setq begin end))
    (if (< begin to)
        (put-text-property begin to setprop (funcall func val) object))))

(bz/advise :override org-indent-set-line-properties
           bz/org-indent-set-line-properties (level indentation &optional heading)
  (let* ((line (aref (pcase heading
                       (`nil org-indent--text-line-prefixes)
                       (`inlinetask org-indent--inlinetask-line-prefixes)
                       (_ org-indent--heading-line-prefixes))
                     level))
         (wrap (aref bz/org-indent--wrap-prefixes level))
         (beg (line-beginning-position)) (end (line-beginning-position 2)))
    ;; Add properties down to the next line to indent empty lines.
    (bz/alter-other-text-property beg end 'bz/line-prefix 'line-prefix (lambda (pre) (concat line pre)))
    (bz/alter-other-text-property beg end 'bz/wrap-prefix 'wrap-prefix (lambda (pre) (concat wrap pre))))
  (forward-line))

;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-keys)
(require 'bz-visual-column)


;; (require 'bz-lens)
(require 'bz-outline)

;; (require 'lens)
(require 'org)
(require 'org-appear)
(require 'org-indent)
(require 'visual-fill-column)


;;; Configuration

(bz/hook org-mode-hook bz/org-mode-setup
  (buffer-face-set 'bz/org-text)
  (setq line-spacing 0.1)
  (setq tab-width 8)
  (display-line-numbers-mode 0)
  (setq-local forward-sexp-function #'bz/org-forward-sexp-function)
  (setq-local indent-line-function #'ignore))

(bz/font-lock-add-keywords
 'org-mode
 '(;; Make empty lines half height
   ;; ("\\(^\n\\)" (1 '(:height 0.5)))
   ;; Make indentation noticable
   ("^\\( +\\)" (1 'fixed-pitch))
   ("^\\(\\**\\)\\(\\* \\)\\([^:\n]*[^ \n]:\\) "
    (3 'bz/org-small-header))
   ("^#\\+begin_box\n\\([^9-0]*?\\)#\\+end_box"
    (1 'bz/org-block prepend))
   ("^#\\+begin_definition\\(.*\\)\n\\([^9-0]*?\\)#\\+end_definition"
    (1 'bz/org-definition-title)
    (2 'bz/org-block prepend))
   ("^#\\+begin_theorem\\(.*\\)\n\\([^9-0]*?\\)#\\+end_theorem"
    (1 'bz/org-theorem-title)
    (2 'bz/org-block prepend))
   ("^#\\+begin_proof\\(.*\\)\n\\([^9-0]*?\\)#\\+end_proof"
    (1 'bz/org-proof-title)
    (2 'bz/org-block prepend))
   )
 )

;; Add the text property to block starts instead of clobbering
(bz/advise :around add-text-properties
           bz/add-text-properties-advice (func start end properties &optional object)
  (if (member properties '((face org-block-begin-line) (face org-block-end-line)))
      (add-face-text-property start end 'org-block-begin-line t object)
    (funcall func start end properties object)))

(setq org-ellipsis " »")
(setq org-src-tab-acts-natively nil)
(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-pretty-entities nil)
(setq org-adapt-indentation nil)
(setq org-indirect-buffer-display 'current-window)


;;; Faces

(bz/face bz/org-text :light:inherit (serif) :dark:inherit (variable-pitch))

(bz/face bz/org-block :bg bg2 :x t :fg fg)
(bz/face bz/org-block-title bz/org-text :w bold :s italic :fg fg)
(bz/face bz/org-definition-title bz/org-block-title :fg "#2A509C")
(bz/face bz/org-theorem-title bz/org-block-title :fg fg)
(bz/face bz/org-proof-title bz/org-block-title :fg gray2)

(bz/face bz/org-header bz/org-text :w bold :u nil)
(bz/face bz/org-small-header bz/org-text :w bold :u t)

(bz/face org-document-title bz/org-header :fg fg :h 1.7 :u nil :s italic)
(bz/face org-document-info bz/org-header :fg fg :h 1.3 :u nil)

(bz/face org-level-1 bz/org-header)
(bz/face org-level-2 bz/org-header)
(bz/face org-level-3 bz/org-header)
(bz/face org-level-4 bz/org-header)
(bz/face org-level-5 bz/org-header)

(bz/face org-hide :h 1.0 :fg bg)
(bz/face org-footnote :fg green)
(bz/face org-meta-line fixed-pitch :fg gray2 :h 0.8)
(bz/face org-special-keyword org-meta-line)
(bz/face org-document-info-keyword fixed-pitch :fg gray2 :h 0.8)
(bz/face org-drawer org-meta-line :fg nil :bg bg)
(bz/face org-table fixed-pitch :fg blue)
(bz/face org-column org-table :bg nil)
(bz/face org-verbatim fixed-pitch :w bold :fg blue :h 0.9)
(bz/face org-code org-verbatim :bg bg2)
(bz/face org-checkbox fixed-pitch)
(bz/face org-ellipsis :fg gray2 :u nil :w light)

(bz/face org-block fixed-pitch :x t :bg bg2 :h 0.85 :fg gray1)
(bz/face org-block-begin-line org-block :fg gray3)
(bz/face org-block-end-line org-block :fg gray3)

(bz/face org-todo fixed-pitch :h 0.8 :w bold :fg red)
(bz/face org-done fixed-pitch :h 0.8 :w bold :fg green)
(bz/face org-headline-done :fg nil)
(bz/face org-checkbox-statistics-todo fixed-pitch :h 0.75 :w bold :fg gray2)
(bz/face org-checkbox-statistics-done fixed-pitch :h 0.75 :w bold :fg green)


;;; Keybindings

(setq-default bz/org-lang "typescript")
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
     (cond ((and (looking-back "[_^]{" (pos-bol)) (looking-at-p "}"))
            (delete-char -2)
            (let ((pos (point)))
              (bz/org-tab)
              (delete-region pos (1+ pos))))
           ((eq (face-at-point) 'org-table) (search-forward-regexp "| ?"))
           ((org-in-src-block-p) (bz/org-indent-code-block))
           ((search-forward-regexp "\\= *\\\\rightarrow " nil t))
           ((search-forward "{}" (pos-eol) t) (backward-char))
           ((search-forward-regexp "\\= *\\(\\\\right.\\|\\\\?}\\)" nil t))
           ((search-forward-regexp "\\= *|| *" nil t))
           ((forward-char))))

  "C-c C-c" org-ctrl-c-ctrl-c
  [remap bz/q] org-ctrl-c-ctrl-c
  "C-c C-f" font-lock-fontify-buffer

  "S-<return>" (let ((s (save-excursion
                          (beginning-of-line)
                          (and (looking-at " *- \\(?:\\[.\\] \\)?") (match-string 0)))))
                 (newline) (when s (insert s)))

  ;; "C-f C-f" lens-create-new-file
  ;; "C-f C-p" lens-auto-at-point
  ;; "C-f C-a" lens-auto-mode
  ;; "C-f C-r" lens-remove
  ;; "C-f C-d" lens-remove-all

  "C-c C-t" org-todo
  "C-w" org-todo
  "C-c C-l" org-insert-link
  "C-c C-e" org-export-dispatch
  "C-c C-i" org-toggle-inline-images

  [remap outline-toggle-children] bz/org-toggle-hidden
  [remap bz/fold-toggle]
  (@ bz/org-toggle-hidden
     (dolist (ol (overlays-at (pos-eol)))
       (when (overlay-get ol 'bz/search-invisible) (delete-overlay ol)))
     (if (or (org-at-drawer-p) (org-at-block-p)
             (save-excursion (move-beginning-of-line 1)
                             (looking-at "#\\+\\(begin\\|end\\)_")))
         (org-cycle)
       (outline-toggle-children)))
  ;; [remap bz/fold-toggle-all] lens-fold-toggle-all

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
           (bz/insert))
  "C-S-o" (@ bz/org-insert-subheading
             (org-insert-heading-after-current)
             (org-do-demote)
             (bz/insert))
  "C-c C-s" org-edit-special

  "C-c C-b" (@ bz/org-insert-source-block
               (bz/insert-snippet (format "#+begin_src %s\n<<>>\n#+end_src" bz/org-lang)))
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

(defvar bz/org-hide-exclude-keywords
  '("begin_src"
    "end_src"))

(defvar bz/org-show-value-keywords
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

      (goto-char (point-min))
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

(defvar-local bz/org-drawer-overlays nil
  "Store the overlays for drawers and meta text in the current buffer")
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

  (mapc #'delete-overlay bz/org-drawer-overlays)
  (setq-local bz/org-drawer-overlays nil)

  (unless bz/org-showing-drawers
    (let ((original-position (point)))

      (goto-char (point-min))
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

  (remove-overlays nil nil 'bz/equation t)

  (let ((original-position (point)))
    (goto-char (point-min))
    (while (search-forward-regexp "〈.*?〉" nil t)
      (search-backward "〈")

      (let ((overlay (make-overlay (point) (search-forward "〉"))))
        (overlay-put overlay 'face '(:slant italic :height 1.05))
        (overlay-put overlay 'bz/equation t)))

    (goto-char (point-min))
    (while (search-forward-regexp "√\\|\\\\sqrt" nil t)
      (let ((overlay (make-overlay (1- (point)) (point))))
        (overlay-put overlay 'face '(:slant normal))
        (overlay-put overlay 'bz/equation t)
        (when (string= (buffer-substring (point) (1+ (point))) "{")
          (overlay-put overlay 'display '((raise 0.1))))))

    (goto-char original-position)))


;;; Visual Fill Column

(bz/hook org-mode-hook bz/visual-fill-column-setup
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text nil)
  (setq truncate-lines t)
  (setq word-wrap t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))


;;; Org Appear

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


;;; Plantuml

(require 'ob-plantuml)

(setq org-plantuml-exec-mode 'plantuml)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (dot . t)
   (mermaid . t)))


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

(defvar bz/org-indent-margin 15)
(defvar bz/org-indent-width 20)
(defvar bz/org-indent-before 2)
(defvar bz/org-indent-guide-char ?\s)
(bz/face bz/org-indent-guide :bg gray3 :fg gray3)

(unless (display-graphic-p)
  (setq bz/org-indent-margin 0)
  (setq bz/org-indent-width 3)
  (setq bz/org-indent-before 0)
  (setq bz/org-indent-guide-char ?\s)
  (bz/face bz/org-indent-guide :fg gray2 :h 1.0))

(defun bz/org-indent-guide-string (level)
  (if (= level 1)
      (propertize " " 'display `(space :width (,bz/org-indent-width) :height (1)))
    (concat (propertize " " 'display `(space :width (,bz/org-indent-before) :height (1)))
            (propertize (string bz/org-indent-guide-char)
                        'display '(space :height (1) :width (1))
                        'face 'bz/org-indent-guide)
            (propertize " " 'display `(space :width (,bz/org-indent-width) :height (1))))))

(defvar bz/org-indent--wrap-prefixes nil)
(bz/advise :override org-indent--compute-prefixes bz/org-indent--compute-prefixes ()
  (let ((n org-indent--deepest-level) prefixes)
    (dotimes (i (+ 2 n))
      (push (if (= i 0) "" (concat (car prefixes) (bz/org-indent-guide-string i)))
            prefixes))
    (setq prefixes (reverse prefixes))

    (setq org-indent--heading-line-prefixes (apply #'vector (-slice prefixes 0 n)))
    (setq org-indent--text-line-prefixes (apply #'vector (-slice prefixes 1 (1+ n))))
    (setq org-indent--inlinetask-line-prefixes org-indent--text-line-prefixes)

    ;; (setq bz/org-indent--wrap-prefixes (apply #'vector (-slice prefixes 1 (+ 1 n))))
    (setq bz/org-indent--wrap-prefixes
          (apply #'vector (--map (concat it wrap-prefix)
                                 (-slice prefixes 1 (1+ n)))))
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
           bz/org-indent-set-line-properties (level _indentation &optional heading)
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


;;; Latex
;;;; Config

(add-to-list 'load-path "~/.emacs.d/my-packages/asynctex")
(require 'asynctex)


(setq asynctex-preview-scale 1.3)

;; Syntax highlighting for latex snippets
(setq org-highlight-latex-and-related '(native))

(bz/hook org-mode-hook asynctex-auto-mode)

(bz/keys org-mode-map
  "C-c C-l" nil
  "C-c C-l C-p" asynctex-org-queue-at-point
  "C-c C-l C-f" asynctex-auto-mode
  "C-c C-l C-l" (@ bz/asynctex-on
                   (asynctex-auto-mode 1)
                   (asynctex-org-queue-new))
  "C-c C-l C-o" (@ bz/asynctex-off
                   (asynctex-auto-mode 0)
                   (asynctex-remove-all))

  "M-\\" (@ bz/insert-latex-fragment
            (if (and (looking-back "\\$" 1) (looking-at-p "\\$"))
                (progn
                  (delete-char 1)
                  (delete-char -1)
                  (insert "\\(  \\)")
                  (backward-char 3))
              (insert "$$") (backward-char 1))
            (bz/insert)))

;; Fix the weird bug where the org-block face leaks outside of the fragment for single dollar sign fragments
(bz/advise :around org-src-font-lock-fontify-block bz/org-src-font-lock-fontify-block (func lang start end)
  ;; Don't remove all text properties
  (advice-add #'remove-text-properties :override #'ignore)
  (unwind-protect (funcall func lang start end)
    (advice-remove #'remove-text-properties #'ignore))

  (let ((is-bol nil)
        (cb (lambda (face)
              (if (listp face)
                  (remove 'org-block face)
                (and (not (eq face 'org-block)) face)))))

    (when (and (equal lang "latex")
               (save-excursion
                 (goto-char start)
                 (setq is-bol (bolp))
                 (or (setq is-bol (and (bolp) (looking-at-p "\\$[^$]")))
                     (looking-at-p "[^$]\\$[^$]"))))
      (unless is-bol (alter-text-property start (1+ start) 'face cb))
      (alter-text-property (1- end) end 'face cb))))


;;;; Snippets

(defun bz/snippet-math-pattern ()
  (interactive)
  (let* ((term (if mark-active (buffer-substring (point) (mark)) (read-string "Term: ")))
         (sep (string (read-char "Separator: ")))
         (min (read-string "Min: "))
         (min+1 (if (not (string-match-p "[0-9]+" min)) (format "%s+1" min)
                  (number-to-string (1+ (string-to-number min)))))
         (max (read-string "Max: "))
         (term0 (replace-regexp-in-string "!" min term))
         (term1 (replace-regexp-in-string "!" min+1 term))
         (termn (replace-regexp-in-string "!" max term)))

    (when mark-active (delete-region (point) (mark)))
    (insert (format "%s %s %s %s ... %s %s" term0 sep term1 sep sep termn))))
(bz/keys bz/selection-map
  "!" bz/snippet-math-pattern)

(setf (alist-get 'org-mode bz/snippet-mode-alist)
      '(("b s" "\\{ <<>>_j \\}_{j=1}^{\\infty}")
        ("!" bz/snippet-math-pattern)

        ("h t" "#+title: ")
        ("h i" "#+latex_header: \\setlength{\\parindent}{0pt}")
        ("h m" "#+latex_header: \\newcommand{\\Mod}[1]{\\ (\\mathrm{mod}\\ #1)}")
        ("h n" "#+latex_header: \\newcommand\\norm[1]{\\left\\lVert#1\\right\\rVert}")
        ("h g" "#+latex_header: \\usepackage[margin=1in]{geometry}")
        ("h s" "#+latex_header: \\AddToHook{cmd/section/before}{\\newpage}")
        ("h p" "#+latex: \\newpage")
        ("h C-i" "#+latex_header: \\newcommand{\\tab}[0]{|\\quad}")

        ("(" "\\left( <<>> \\right)")
        ("[" "\\left[ <<>> \\right]")
        (")" "\\left[ <<>> \\right)")
        ("]" "\\left( <<>> \\right]")
        ("{" "\\left\\{ <<>> \\right\\}")
        ("\\" "\\left< <<>> \\right>")
        ("|" "\\left| <<>> \\right|")
        ("1" "^{-1}")
        ("2" "^2")
        ("k" "^")
        ("j" "_")
        ("K" "^{<<>>}")
        ("J" "_{<<>>}")
        ("I" " \\in ")
        ("t" "\\text{<<>>}")
        ("T" "\\texttt{<<>>}")
        ("M" "\\Mod{<<>>}")
        ("S" "\\star")
        ("x" "\\cdot")
        ("X" "\\times")
        ("D" "\\displaystyle")
        ("$" "\\textdollar")
        ("Q" "\\square")

        ("=" "\\equiv")
        ("~" "\\widetilde{<<>>}")
        ("` `" "\\sim")
        ("` ~" "\\approx")
        ("` -" "\\simeq")
        ("` =" "\\cong")
        ("` p" "\\propto")

        ("F" "\\frac{<<>>}{}" )
        ("f 1" "\\frac{1}{<<>>}")
        ("f 2" "\\frac{1}{2}")

        ("," "\\leftarrow")
        ("." "\\rightarrow")
        ("/" "\\leftrightarrow")
        ("<" "\\Leftarrow")
        (">" "\\Rightarrow")
        ("/" "\\Leftrightarrow")
        ("b >" "\\Longrightarrow")
        ("b <" "\\Longleftarrow")
        ("b ?" "\\Longleftrightarrow")
        ("a k" "\\uparrow")
        ("a K" "\\Uparrow")
        ("a j" "\\downarrow")
        ("a J" "\\Downarrow")
        ("a v" "\\updownarrow")
        ("a V" "\\Updownarrow")

        ("O" "\\operatorname{<<>>}")
        ("o o" "\\operatorname{<<>>}")
        ("o i" "\\operatorname{im}")
        ("o g" "\\operatorname{graph}")
        ("o c" "\\operatorname{codim}")
        ;; Stats
        ("p p" "\\operatorname{Pr}\\left[ <<>> \\right]")
        ("p e" "\\operatorname{E}\\left[ <<>> \\right]")
        ("p v" "\\operatorname{Var}\\left[ <<>> \\right]")
        ("p c" "{<<>> \\choose }")
        ("p C" "\\operatorname{Cov}\\left[ <<>> \\right]")
        ("p t" "\\hat{\\theta}")

        ("s c" "\\circ")
        ("s s" "\\left\\{ <<>> \\right\\}")
        ("s m" "\\setminus")
        ("s ," "\\subseteq")
        ("s ." "\\supseteq")
        ("s u" "\\cup")
        ("s U" "\\bigcup_{<<>>}^{}")
        ("s i" "\\cap")
        ("s I" "\\bigcap_{<<>>}^{}")
        ("s e" "\\emptyset")
        ("s t" "\\triangle")
        ("s b" "\\bar{<<>>}")
        ("s o" "\\overline{<<>>}")
        ("s O" "\\bar{<<>>}")
        ("s n" "\\trianglelefteq") ; Normal
        ("s N" "\\trianglerighteq") ; Normal
        ("s +" "\\oplus") ; Direct sum
        ("s X" "\\otimes") ; Direct sum

        ("g a" "\\alpha") ("g A" "\\Alpha")
        ("g b" "\\beta") ("g B" "\\Beta")
        ("g g" "\\gamma") ("g G" "\\Gamma")
        ("g d" "\\delta") ("g D" "\\Delta")
        ("g e" "\\varepsilon") ("g E" "\\Epsilon")
        ("g l" "\\lambda") ("g L" "\\Lambda")
        ("g m" "\\mu") ("g M" "\\Mu")
        ("g n" "\\nu") ("g N" "\\Nu")
        ("g o" "\\omega") ("g O" "\\Omega")
        ("g p" "\\varphi") ("g P" "\\Phi")
        ("g r" "\\rho") ("g R" "\\Rho")
        ("g s" "\\sigma") ("g S" "\\Sigma")
        ("g t" "\\theta") ("g T" "\\Theta")
        ("g u" "\\tau") ("g U" "\\Tau")
        ("g z" "\\zeta") ("g Z" "\\Zeta")
        ("v p" "\\varphi")
        ("v e" "\\varepsilon")

        ("m r" "\\sqrt{<<>>}")
        ("m R" "\\sqrt[<<>>]{}")
        ("m f" "\\lfloor <<>> \\rfloor")
        ("m c" "\\lceil <<>> \\rceil")
        ("m s" "\\sum_{<<>>}^{}")
        ("m S" "\\sum_{<<>>}^{\\infty}")
        ("m p" "\\prod_{<<>>}^{}")
        ("m P" "\\prod_{<<>>}^{\\infty}")
        ("m i" "\\int_{<<>>}^{}")
        ("m I" "\\int_{<<>>}^{\\infty}")
        ("m l" "\\lim_{<<>> \\rightarrow }")
        ("m L" "\\lim_{<<>> \\rightarrow \\infty}")
        ("m g" "\\nabla")
        ("m n" "\\norm{<<>>}")
        ("m t" "\\transv")
        ("m w" "\\wedge")

        ;; Logic
        ("l ." "\\mapsto") ; Arrow with bar
        ("l e" "\\exists")
        ("l f" "\\forall")
        ("l n" "\\neg")
        ("l a" "\\land")
        ("l o" "\\lor")
        ("l c" "\\circ") ; Compose
        ("l k" "\\ker")

        ("r b" "#+begin_box\n<<>>\n#+end_box")
        ("r d" "#+begin_definition <<>>\n#+end_definition")
        ("r t" "#+begin_theorem <<>>\n#+end_theorem")
        ("r p" "#+begin_proof\n<<>>\n#+end_proof")
        ("r a" "\\begin{align*}\n<<>>\n\\end{align*}")
        ("r c" "\\begin{cases} <<>> \\end{cases}")
        ("r m" "\\begin{matrix} <<>> \\end{matrix}")
        ("r <" "\\langle <<>> \\rangle")

        ("b x" "\\mathbf{x}")
        ("b z" "\\mathbb{Z}")
        ("b -" "\\mathbb{Z}_-")
        ("b =" "\\mathbb{Z}_+")
        ("b 0" "\\mathbb{Z}_{\\ge 0}")

        ("b r" "\\mathbb{R}")
        ("b R" "\\mathbb{R}^n")
        ("b _" "\\mathbb{R}_-")
        ("b +" "\\mathbb{R}_+")
        ("b )" "\\mathbb{R}_{\\ge 0}")

        ("b n" "\\mathbb{N}")
        ("b q" "\\mathbb{Q}")
        ("b c" "\\mathbb{C}")
        ("b f" "\\mathbb{F}")
        ("b i" "\\infty")
        ("b l" "\\ell")
        ("b p" "\\partial")
        ("b a" "\\ast")

        ("f a" "\\mathcal{A}")
        ("f b" "\\mathcal{B}")
        ("f c" "\\mathcal{C}")
        ("f d" "\\mathcal{D}")
        ("f e" "\\mathcal{E}")
        ("f f" "\\mathcal{F}")
        ("f g" "\\mathcal{G}")
        ("f h" "\\mathcal{H}")
        ("f i" "\\mathcal{I}")
        ("f j" "\\mathcal{J}")
        ("f k" "\\mathcal{K}")
        ("f l" "\\mathcal{L}")
        ("f m" "\\mathcal{M}")
        ("f n" "\\mathcal{N}")
        ("f o" "\\mathcal{O}")
        ("f p" "\\mathcal{P}")
        ("f q" "\\mathcal{Q}")
        ("f r" "\\mathcal{R}")
        ("f s" "\\mathcal{S}")
        ("f t" "\\mathcal{T}")
        ("f u" "\\mathcal{U}")
        ("f v" "\\mathcal{V}")
        ("f w" "\\mathcal{W}")
        ("f x" "\\mathcal{X}")
        ("f y" "\\mathcal{Y}")
        ("f z" "\\mathcal{Z}")))


;;;; Prettify

(defvar bz/org-latex-font-lock-keywords nil)
(defmacro bz/org-make-font-lock-keywords (&rest replacements)
  `(progn
     (font-lock-remove-keywords 'org-mode bz/org-latex-font-lock-keywords)
     (setq bz/org-latex-font-lock-keywords
           (--map `(,(concat "\\(" (regexp-quote (car it)) "\\)"
                             "\\(?:\\>\\|[^a-zA-Z0-9]\\|$\\)")
                    (1 (ignore (compose-region (match-beginning 1) (match-end 1) ,(cadr it)))))
                  ,(list '\` replacements)))
     (font-lock-add-keywords 'org-mode bz/org-latex-font-lock-keywords)))


(bz/org-make-font-lock-keywords
 ("\\[" "⟦")
 ("\\]" "⟧")
 ("\\(" "⟨")
 ("\\)" "⟩")
 ("\\left[" "⟦")
 ("\\right]" "⟧")
 ("\\left(" "⟬")
 ("\\right)" "⟭")
 ("\\left\\{" "⦃")
 ("\\right\\}" "⦄")
 ("\\langle" "〈")
 ("\\rangle" "〉")
 ("\\mid" "│")

 ("\\text" "🅃")
 ("\\textbf" "🄱")
 ("\\widetilde" "～")

 ("\\sqrt" "√")
 ("\\cdot" "∙")
 ("\\times" "×")
 ("\\otimes" "")
 ("\\oplus" "⨁")
 ("\\otimes" "⨂")
 ("\\equiv" "≡")

 ("\\Rightarrow" "⇒")
 ("\\Leftarrow" "⇐")
 ("\\Leftrightarrow" "⇔")
 ("\\Uparrow" "⇑")
 ("\\Downarrow" "⇓")
 ("\\Updownarrow" "⇕")
 ("\\Longrightarrow" "⟹")
 ("\\Longleftarrow" "⟸")
 ("\\Longleftrightarrow" "⟺")

 ("\\rightarrow" "→")
 ("\\leftarrow" "←")
 ("\\uparrow" "↑")
 ("\\downarrow" "↓")
 ("\\leftrightarrow" "↔")
 ("\\updownarrow" "↕")
 ("\\mapsto" "⟼")

 ("\\lfloor" "⌊")
 ("\\rfloor" "⌋")
 ("\\rceil" "⌈")
 ("\\lceil" "⌉")
 ("\\left|" "⎢")
 ("\\right|" "⎥")

 ("\\sum" "Σ")
 ("\\prod" "Π")
 ("\\int" "∫")
 ("\\oint" "∮")

 ("\\neg" "¬")
 ("\\land" "∧")
 ("\\lor" "∨")
 ("\\circ" "∘") ; Compose
 ("\\in" "∈")
 ("\\notin" "∉")
 ("\\subseteq" "⊆")
 ("\\not\\subseteq" "⊄")
 ("\\supseteq" "⊇")
 ("\\not\\supseteq" "⊈")
 ("\\cap" "∩")
 ("\\cup" "∪")
 ("\\bigcap" "⋂")
 ("\\bigcup" "⋃")
 ("\\emptyset" "∅")
 ("\\setminus" "∖")
 ("\\triangle" "∆")
 ("\\trianglelefteq" "⊴")
 ("\\trianglerighteq" "⊵")
 ("\\exists" "∃")
 ("\\not\\exists" "∄")
 ("\\forall" "∀")
 ("\\infty" "∞")
 ("\\ne" "≠")
 ("\\ge" "≥")
 ("\\le" "≤")
 ("\\square" "□")

 ("\\mathbb{C}" "ℂ")
 ("\\mathbb{H}" "ℍ")
 ("\\mathbb{N}" "ℕ")
 ("\\mathbb{P}" "ℙ")
 ("\\mathbb{Q}" "ℚ")
 ("\\mathbb{R}" "ℝ")
 ("\\mathbb{Z}" "ℤ")
 ("\\mathbb{F}" "𝔽")
 ("\\partial" "∂")
 ("\\nabla" "∇")
 ("\\ast" "✱")
 ("\\transv" "⫛")
 ("\\wedge" "∧")

 ("\\ell" "ℓ")
 ("\\overline" "﹉")
 ("\\bar" "‾")
 ("\\operatorname" "⏣")

 ("\\cong" "≅")
 ("\\approx" "≈")
 ("\\sim" "~")
 ("\\simeq" "≃")
 ("\\propto" "∝")

 ;; ("\\frac" "𝕗")
 ;; ("\\frac{1}{2}" "½")
 ;; ("\\frac{1}{3}" "⅓")
 ;; ("\\frac{1}{4}" "¼")

 ("\\begin{matrix}" "▦")
 ("\\end{matrix}" "▥")

 ("\\Alpha"   "Α") ("\\alpha"   "α")
 ("\\Beta"    "Β") ("\\beta"    "β")
 ("\\Gamma"   "Γ") ("\\gamma"   "γ")
 ("\\Delta"   "Δ") ("\\delta"   "δ")
 ("\\Epsilon" "Ε") ("\\epsilon" "ϵ") ("\\varepsilon" "ε")
 ("\\Zeta"    "Ζ") ("\\zeta"    "ζ")
 ("\\Eta"     "Η") ("\\eta"     "η")
 ("\\Theta"   "Θ") ("\\theta"   "θ")
 ("\\Iota"    "Ι") ("\\iota"    "ι")
 ("\\Kappa"   "Κ") ("\\kappa"   "κ")
 ("\\Lambda"  "Λ") ("\\lambda"  "λ")
 ("\\Mu"      "Μ") ("\\mu"      "μ")
 ("\\Nu"      "Ν") ("\\nu"      "ν")
 ("\\Xi"      "Ξ") ("\\xi"      "ξ")
 ("\\Omikron" "Ο") ("\\omikron" "ο")
 ("\\Pi"      "Π") ("\\pi"      "π")
 ("\\Rho"     "Ρ") ("\\rho"     "ρ")
 ("\\Sigma"   "Σ") ("\\sigma"   "σ")
 ("\\Tau"     "Τ") ("\\tau"     "τ")
 ("\\Upsilon" "Υ") ("\\upsilon" "υ")
 ("\\Phi"     "Φ") ("\\phi"     "ϕ") ("\\varphi" "φ")
 ("\\Chi"     "Χ") ("\\chi"     "χ")
 ("\\Psi"     "Ψ") ("\\psi"     "ψ")
 ("\\Omega"   "Ω") ("\\omega"   "ω")

 ("\\mathcal{A}" "𝓐") ("\\mathcal{B}" "𝓑")
 ("\\mathcal{C}" "𝓒") ("\\mathcal{D}" "𝓓")
 ("\\mathcal{E}" "𝓔") ("\\mathcal{F}" "𝓕")
 ("\\mathcal{G}" "𝓖") ("\\mathcal{H}" "𝓗")
 ("\\mathcal{I}" "𝓘") ("\\mathcal{J}" "𝓙")
 ("\\mathcal{K}" "𝓚") ("\\mathcal{L}" "𝓛")
 ("\\mathcal{M}" "𝓜") ("\\mathcal{N}" "𝓝")
 ("\\mathcal{O}" "𝓞") ("\\mathcal{P}" "𝓟")
 ("\\mathcal{Q}" "𝓠") ("\\mathcal{R}" "𝓡")
 ("\\mathcal{S}" "𝓢") ("\\mathcal{T}" "𝓣")
 ("\\mathcal{U}" "𝓤") ("\\mathcal{V}" "𝓥")
 ("\\mathcal{W}" "𝓦") ("\\mathcal{X}" "𝓧")
 ("\\mathcal{Y}" "𝓨") ("\\mathcal{Z}" "𝓩"))


;;; Provide

(provide 'bz-org)

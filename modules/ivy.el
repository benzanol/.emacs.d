;;; Ivy
(qv/package ivy)
(ivy-mode 1)

(qv/face ivy-current-match highlight :fg nil :bg nil)
(qv/face ivy-minibuffer-match-face-1 :fg green :bg nil :w bold)
(qv/face ivy-minibuffer-match-face-2 :fg purple :bg nil :w bold)
(qv/face ivy-minibuffer-match-face-3 :fg red :bg nil :w bold)
(qv/face ivy-minibuffer-match-face-4 :fg blue :bg nil :w bold)

(qv/keys ivy-minibuffer-map
  :sparse t
  :parent minibuffer-local-map
  "TAB" ivy-insert-current
  "RET" ivy-done
  "<C-return>" ivy-immediate-done
  [remap qv/down] ivy-next-line
  [remap qv/up] ivy-previous-line
  [remap qv/down4] (ivy-next-line 4)
  [remap qv/up4] (ivy-previous-line 4)
  [remap beginning-of-buffer] ivy-beginning-of-buffer
  [remap end-of-buffer] ivy-end-of-buffer)

;;; Ivy prescient
(qv/package ivy-prescient)
(ivy-prescient-mode 1)
(setcdr (assoc t ivy-sort-functions-alist) 'ivy-prescient-sort-function)

;;; Custom formatting
(setq ivy-format-functions-alist
      '((counsel-fonts . ivy-format-function-default)
        (t . qv/ivy-format)))

(defun qv/ivy-format (cands)
  (ivy--format-function-generic
   `(lambda (str) (qv/ivy-format-cand str t))
   `(lambda (str) (qv/ivy-format-cand str))
   cands "\n"))

(defun qv/ivy-format-cand (str &optional current)
  (let ((title-face (if current '(:inherit (highlight variable-pitch) :height 1.05)
                      '(:inherit variable-pitch :height 1.05)))
        (same-size (eq ivy-height 10))
        pos1 pos2 faces new)
    (dotimes (i (length str))
      (setq faces (get-text-property i 'face str))
      (unless (listp faces) (setq faces (list faces)))
      (when (and (not pos1) (memq 'ivy-completions-annotations faces)) (setq pos1 i))
      (when (and (not pos2) (memq 'marginalia-documentation faces)) (setq pos2 i i (length str))))
    
    (when (and pos2 (null (remove ?\s (append (substring str pos2) nil)))) (setq pos2 nil))

    
    (setq new (concat " " (ivy--add-face (substring str 0 pos1) title-face)))

    (cond ((and (not same-size) (not pos1) (not pos2))
           (concat new #(" " 0 1 (display ((height 1.3) (raise -0.1))))))
          
          ((and (not same-size) pos1 (not pos2))
           (concat new #(" " 0 1 (display ((space :align-to 30) (height 1.3) (raise -0.1))))
                   (ivy--add-face (substring str pos1 pos2) '(:height 0.95))))

          (t (concat new
                     (if pos1 #(" " 0 1 (display (space :align-to 30))) "")
                     (if pos1 (ivy--add-face (substring str pos1 pos2) '(:height 0.95)) "")
                     "\n "
                     (propertize (if (and pos1 pos2) (substring str pos2) " ")
                                 'display '(raise 0.1)) "")))))

;;; Ivy posframe
(qv/package ivy-posframe)
(ivy-posframe-mode 1)

(setq ivy-posframe-style "frame-top-center")

(setq ivy-posframe-width 100)
(setq ivy-posframe-height 23)
(setq ivy-posframe-width 90)
(setq ivy-height 15)

(add-to-list 'ivy-height-alist '(execute-extended-command . 10))
(add-to-list 'ivy-height-alist '(helpful-callable . 10))
(add-to-list 'ivy-height-alist '(helpful-variable . 10))
(add-to-list 'ivy-height-alist '(counsel-fonts . 40))

;;; Counsel
(qv/package counsel)

(qv/key * "C-x C-f" counsel-find-file)

(qv/keys counsel-find-file-map
  :sparse t
  "DEL" counsel-up-directory
  "TAB" counsel-down-directory)

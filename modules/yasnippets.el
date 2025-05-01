(qv/package yasnippet)

(yas-global-mode 1)

;;; Keybindings and settings

(qv/keys yas-minor-mode-map
  :sparse t
  ;; "<tab>" (@ qv/yas-tab
  ;;              (or (yas-expand)
  ;;                  (if (company--active-p) (company-complete-selection)
  ;;                    (indent-for-tab-command))))
  )

(setq qvs/jsdoc-current-comment nil)

;; Disable company while snippeting
(qv/hook yas-before-expand-snippet-hook qv/disable-company-autocomplete
  (qv/company-autocomplete-mode 0))

(qv/hook yas-after-exit-snippet-hook qv/company-autocomplete-mode)

;;; Yas functions

(defun qvs/add-field (beg end &optional transform)
  (let* ((fields (yas--snippet-fields (car yas--active-snippets)))
         (new (yas--make-field
               (+ 1 (length fields))
               (set-marker (make-marker) beg)
               (set-marker (make-marker) end)
               nil)))
    (when transform (setf (yas--field-transform new) transform))
    (nconc fields (list new))))

(defun qvs/trim (before after)
  (when (and yas-moving-away-p (string= yas-text ""))
    (setq yas-inhibit-overlay-modification-protection t)
    (save-excursion
      (goto-char (yas--field-start (yas-current-field)))
      (delete-backward-char before)
      (delete-forward-char after))
    (setq yas-inhibit-overlay-modification-protection nil)))


;;; Jsdoc

(defun qvs/jsdoc-add-param-fields ()
  "After creating the arg string, remove the jsdoc comment as a
mirror, and add each individual variable as a field."
  (when yas-moving-away-p
    (setf (yas--field-mirrors (yas-current-field)) nil)
    (save-excursion
      (goto-char yas-snippet-beg)
      (while (search-forward "@param {}" yas-snippet-end t)
        (qvs/add-field (1- (point)) (1- (point)) '(qvs/trim 2 1))
        (end-of-line)
        (qvs/add-field (point) (point) '(qvs/trim 3 0))))))

(defun qvs/jsdoc-comment ()
  (if (string= yas-text "") ""
    (mapconcat (lambda (param) (format "\n * @param {} %s - " (string-trim param)))
               (split-string yas-text ",") "")))

(defun qvs/js-constructor-body ()
  (mapconcat (lambda (field) (format "this.%s = %s;" field field))
             (mapcar #'string-trim (split-string yas-text ",")) "\n"))


;; (defun qvs/jsdoc-params (params)
;;   "When exiting the args field, remove the type annotations"
;;   (if (not yas-moving-away-p) params
;;     (mapconcat (lambda (param) (car (split-string param ":")))
;;                (split-string params ",") ",")))
;; (defun qvs/jsdoc-comment (params)
;;   (unless yas-moving-away-p
;;     (setq qvs/jsdoc-current-comment
;;           (if (string= params "") ""
;;             (mapconcat (lambda (param)
;;                          (let ((split (mapcar #'string-trim (split-string param ":"))))
;;                            (if (eq 1 (length split))
;;                                (format "\n * @param %s - " (car split))
;;                              (format "\n * @param {%s} %s - " (cadr split) (car split)))))
;;                        (split-string params ",") ""))))
;;   qvs/jsdoc-current-comment)


;;; Mode keybindings

(defun qv/yas-insert (snippet)
  (qvk-insert-keymode)
  (yas-expand-snippet (yas-lookup-snippet snippet)))


;;;; Typescript

(qv/keys qv/js-snippet-map
  :sparse t
  "f f" (qv/yas-insert "function")
  "f F" (qv/yas-insert "export function")
  "f a" (qv/yas-insert "async function")
  "f A" (qv/yas-insert "export async function")
  "c" (qv/yas-insert "class")
  "v" (qv/yas-insert "const")
  "l f" (qv/yas-insert "for loop")
  "l e" (qv/yas-insert "for each")
  "p" (qv/yas-insert "console log"))

(qv/keys typescript-mode-map
  :keymode normal
  "'" ,qv/js-snippet-map)

(qv/keys js-jsx-mode-map
  :keymode normal
  "'" ,qv/js-snippet-map)


;;;; Html

(qv/keys mhtml-mode-map
  :sparse t
  :keymode normal
  "g =" qv/format-buffer
  "' '" (qv/yas-insert "tag")
  "' h" (qv/yas-insert "html")
  "' d" (qv/yas-insert "div")
  "' b" (insert "</br>")
)

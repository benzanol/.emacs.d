;; -*- lexical-binding: t; -*-
(setf (alist-get 'c-mode bz/indent-command-alist)
      nil)

(bz/hook c-mode-hook bz/c-mode-setup
  (setq-local
   imenu-generic-expression
   '(("Functions" "^\\([[:alpha:]_][[:alnum:]_:<>~]*\\)[    ]*([    ]*\\([^     (*][^)]*\\)?)[      ]*[^    ;(]" 1)
     ("Structs" "^.*[\n\s\t]struct[\n\s\t]+\\([a-zA-Z0-9_]+\\)[\n\s\t]*{" 1)))

  ;; 4 indentation
  (c-set-style "stroustrup"))

(defun bz/imenu-next (&optional n)
  (interactive)
  (let (mark)
    (with-selected-window (or (get-buffer-window imenu-list-buffer-name) (get-buffer-window lsp-ui-imenu-buffer-name))
      (forward-line n)
      (while (and (not (bobp)) (not (eobp))
                  (not (markerp (setq mark (cdr (imenu-list--find-entry))))))
        (forward-line n)))
    (when (markerp mark) (goto-char mark))))

(bz/keys c-mode-map
  :sparse t
  "C-j" bz/imenu-next
  "C-k" (@ bz/imenu-previous (bz/imenu-next -1))
  )
;; (defun bz/c-indent ()
;;   (goto-char (point-min))
;;   (while (not (eobp))
;;     (indent-for-tab-command)
;;     (forward-line 1))))

(defun bz/imenu-current-buffer ()
  (interactive)
  (setq bz/imenu-buffer (current-buffer)))

(setq bz/imenu-buffer nil)
(bz/advise :around imenu-list-update bz/imenu-list-update (func &rest args)
  (with-current-buffer (or bz/imenu-buffer (current-buffer))
    (apply func args)))


;; Use system clangd instead of cached one (doesn't work for some reason)
;; (setq lsp-clients-clangd-executable "clangd")
(setq lsp-clients-clangd-executable nil)

;; Set indent width
(setq c-basic-offset 4)


;;; Provide

(provide 'bz-c)
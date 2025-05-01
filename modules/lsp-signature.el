(bz/package subr-x)
(bz/package seq)

;;; Show one line of function signature

(defun bz/lsp-signature (&optional buffer)
  (ignore-errors
    (when (or (null buffer) (eq buffer (current-buffer)))
      (lsp-request-async
       "textDocument/signatureHelp"
       (lsp--text-document-position-params)
       (lambda (signature)
         (let ((message
                (if (lsp-signature-help? signature)
                    (lsp--signature->message signature)
                  (mapconcat #'lsp--signature->message signature "\n"))))
           (if (s-present? message) (bz/lsp-signature-show message)

             ;; If no signature, maybe do flycheck error instead
             ;; (when-let ((err (car (sort (flycheck-overlay-errors-at (point))
             ;;                            (lambda (e1 e2) (flycheck-error-level-< e2 e1))))))
             ;;   (message
             ;;    (propertize
             ;;     (bz/lsp-signature-truncate
             ;;      (flycheck-error-format-message-and-id err))
             ;;     'face (flycheck-error-level-fringe-face (flycheck-error-level err)))))
             )))

       :cancel-token :signature))))


;; Same as eldoc - when inside function parens, show the required arguments and types
(defun bz/lsp-signature-show (text)
  (eldoc-message (bz/fontify-code (bz/lsp-signature-truncate text) 'rust-mode)))

(defun bz/lsp-signature-truncate (text)
  (setq text (car (split-string text "\n")))
  (if (<= (length text) 150) text
    (substring text 0 150)))

(setq flycheck-display-errors-function nil)


;;; Fontify signature
(defun bz/fontify-code (code mode)
  "Colorize a string of code and return it as a fontified string."

  ;; Keep track of the bold text
  (let (beg end)
    (dotimes (i (length code))
      (when (eq (cadr (text-properties-at i code)) 'eldoc-highlight-function-argument)
        (setq beg (or beg i) end (1+ i))))

    (with-temp-buffer
      (insert code)
      (newline) ; Necessary
      (funcall mode)
      (font-lock-ensure)
      (setq code (buffer-substring (point-min) (1- (point-max)))))

    (when (and beg end)
      (add-face-text-property
       beg end 'eldoc-highlight-function-argument
       nil code))

    code))

(bz/face eldoc-highlight-function-argument :w bold)


;;; Automatically display signature
(define-minor-mode bz/lsp-signature-mode
  "Automatically display function signatures in minibuffer"
  :global t
  (if bz/lsp-signature-mode
      (add-hook 'post-command-hook 'bz/lsp-signature-maybe nil 'local)
    (remove-hook 'post-command-hook 'bz/lsp-signature-maybe 'local)))

(setq bz/lsp-signature-delay 0.5)

(setq-default bz/lsp-signature-cursor nil)

(defun bz/lsp-signature-maybe ()
  (if (not lsp-mode)
      (bz/lsp-signature-mode 0)

    (cancel-function-timers #'bz/lsp-signature)

    (unless (eq (point) bz/lsp-signature-cursor)
      (setq bz/lsp-signature-cursor (point))
      (run-with-timer bz/lsp-signature-delay nil #'bz/lsp-signature (current-buffer)))))

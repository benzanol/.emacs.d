;;; Show info in popup


(defun bz/lsp-popup ()
  (interactive)

  ;; The frame SHOULD be closed automatically without having to do
  ;; this, but it sometimes doesn't (believe me)
  ;; If the popup disappears right after its created, its because the
  ;; mouse is over it, not because of this
  (add-hook 'post-command-hook 'bz/remove-popup-postcmd)

  (lsp-ui-doc-show))

(defun bz/remove-popup-postcmd ()
  (unless (or (eq this-command 'bz/lsp-popup-show)
              lsp-ui-doc-frame-mode)
    (lsp-ui-doc--hide-frame)
    (remove-hook 'post-command-hook 'bz/remove-popup-postcmd)))


;;; Auto popup

(setq bz/lsp-auto-popup-delay 0.5)

(define-minor-mode bz/lsp-auto-popup-mode
  "Automatically display lsp info in a popup"
  nil nil nil
  (if bz/lsp-auto-popup-mode
      (add-hook 'post-command-hook 'bz/lsp-popup--delayed-display nil 'local)
    (remove-hook 'post-command-hook 'bz/lsp-popup--delayed-display 'local)))

;; Start a timer to automatically display info
(defun bz/lsp-popup--delayed-display ()
  (cancel-function-timers 'bz/lsp-popup--callback)
  (run-with-timer bz/lsp-auto-popup-delay nil #'bz/lsp-popup--callback (current-buffer)))

;; Function called by timer to maybe display info
(defun bz/lsp-popup--callback (buf)
  (when (eq (current-buffer) buf)
    (bz/lsp-popup)))


;;; Auto focus

;; Sometimes the popup goes away instantly no matter what. Sometimes
;; this is because the mouse is in the way, otherwise, this variable
;; makes it stay.
(define-minor-mode bz/lsp-popup-auto-focus
  "Auto focus the lsp popup."
  :global t :init-value nil)


;;; Hide ugly line
(bz/advise :override lsp-ui-doc--handle-hr-lines ignore)

;;; Complicated way of just doing lsp-ui-doc-show

;; (defun bz/lsp-popup ()
;;   (interactive)

;;   ;; The frame SHOULD be closed automatically without having to do
;;   ;; this, but it sometimes doesn't (believe me)
;;   ;; If the popup disappears right after its created, its because the
;;   ;; mouse is over it, not because of this
;;   (add-hook 'post-command-hook 'bz/remove-popup-postcmd)

;;   (lsp-request-async
;;    "textDocument/hover"
;;    (lsp--text-document-position-params)
;;    #'bz/lsp-popup--hover-callback))

;; ;; Callback after recieving the hover information
;; (lsp-defun bz/lsp-popup--hover-callback ((hover &as &Hover? :contents))
;;   (if (not hover)
;;       (lsp-request-async
;;        "textDocument/signatureHelp"
;;        (lsp--text-document-position-params)
;;        #'bz/lsp-popup--signature-callback
;;        :cancel-token :signature)
;;     (lsp-ui-doc--display
;;      nil (-some->> contents
;;            lsp-ui-doc--extract
;;            (replace-regexp-in-string "\r" "")
;;            (replace-regexp-in-string " " " ")))
;;     (when bz/lsp-popup-auto-focus (lsp-ui-doc-focus-frame))))

;; ;; Callback after recieving the signature information
;; (defun bz/lsp-popup--signature-callback (sig)
;;   (if (not sig) (message "No signature found")
;;     (lsp-ui-doc--display nil (lsp--signature->message sig))
;;     (when bz/lsp-popup-auto-focus (lsp-ui-doc-focus-frame))))

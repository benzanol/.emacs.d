;;; Show one line of function signature
;; Same as eldoc - when inside function parens, show the required arguments and types
(defun qv/lsp-signature-function (text)
  (when (and (with-current-buffer " *Echo Area 0*"
               (or (= 1 (point-max)) (get-text-property 1 'lsp-signature)))
             (stringp text))
    (let ((sig (car (split-string text "\n"))))
      (unless (string= sig "")
        (message "%s" (propertize sig 'lsp-signature t))))))

;; Only display the first line of the signature
;;(setq lsp-signature-function #'lsp-lv-message)
;;(setq lsp-signature-function #'message)
(setq lsp-signature-function #'qv/lsp-signature-function)

;; Automatically display signature
(define-minor-mode qv/lsp-signature-mode
  "Automatically display function signatures in minibuffer"
  nil nil nil
  (if qv/lsp-signature-mode
      (add-hook 'post-command-hook 'lsp-signature-activate nil 'local)
    (remove-hook 'post-command-hook 'lsp-signature-activate 'local)))


;;; Show info in popup
(defun qv/lsp-popup ()
  (interactive)

  (lsp-request-async
   "textDocument/hover"
   (lsp--text-document-position-params)
   #'qv/lsp-popup--hover-callback))

;; Callback after recieving the hover information
(lsp-defun qv/lsp-popup--hover-callback ((hover &as &Hover? :contents))
  (if hover
      (lsp-ui-doc--display
       nil (-some->> contents
             lsp-ui-doc--extract
             (replace-regexp-in-string "\r" "")
             (replace-regexp-in-string " " " ")))
    
    (lsp-request-async
     "textDocument/signatureHelp"
     (lsp--text-document-position-params)
     #'qv/lsp-popup--signature-callback
     :cancel-token :signature)))

;; Callback after recieving the signature information
(defun qv/lsp-popup--signature-callback (sig)
  (if sig (lsp-ui-doc--display nil (lsp--signature->message sig))
    (message "No signature found")))


;;; Auto display info
(define-minor-mode qv/lsp-popup-mode
  "Automatically display lsp info in a popup"
  nil nil nil
  (if qv/lsp-popup-mode
      (add-hook 'post-command-hook 'qv/lsp-popup--delayed-display nil 'local)
    (remove-hook 'post-command-hook 'qv/lsp-popup--delayed-display 'local)))

;; Keep track of the timer so it can be cancelled
(setq qv/lsp-popup-timer nil)

;; Keep track of the buffer which the function was called in
(setq qv/lsp-popup-buffer nil)

;; Start a timer to automatically display info
(defun qv/lsp-popup--delayed-display ()
  (when (timerp qv/lsp-popup-timer)
    (cancel-timer qv/lsp-popup-timer))

  (setq qv/lsp-popup-buffer (current-buffer))
  (setq qv/lsp-popup-timer (run-with-timer 0.5 nil #'qv/lsp-popup--callback)))

;; Function called by timer to maybe display info
(defun qv/lsp-popup--callback ()
  (when (and qv/lsp-popup-mode
             (eq (current-buffer) qv/lsp-popup-buffer))
    (qv/lsp-popup)))


;;; Flycheck posframe
(qv/package flycheck-posframe)

(qv/face flycheck-posframe-background-face :bg "#383B48")
(qv/face flycheck-posframe-border-face :fg "gray50")
(qv/face flycheck-posframe-error-face nil)
(qv/face flycheck-posframe-warning-face nil)
(setq flycheck-posframe-border-width 1)

;; Manually show the posframe
(defun qv/flycheck-posframe-show ()
  (interactive)
  (flycheck-posframe-show-posframe (flycheck-overlay-errors-at (point)))
  (add-hook 'pre-command-hook #'qv/hide-flycheck-posframe))

;; Hide the posframe immediately before the next command
(defun qv/hide-flycheck-posframe ()
  (remove-hook 'pre-command-hook #'qv/hide-flycheck-posframe)
  (posframe-hide flycheck-posframe-buffer))


;;; Settings
(qv/package lsp-mode)
(qv/package lsp-ui)

(qv/require company)


(qv/keys lsp-mode-map
  :sparse t
  "C-c C-d" lsp-find-definition
  "C-c C-r" lsp-find-references
  "C-c C-j" flycheck-next-error
  "C-c C-k" flycheck-previous-error
  "C-c C-l" flycheck-list-errors
  "C-c C-e" qv/flycheck-posframe-show
  "C-c C-c" (@ qv/lsp-popup-show
               (if (lsp-ui-doc--frame-visible-p)
                   (lsp-ui-doc-focus-frame)
                 (qv/lsp-popup))))


(qv/hook lsp-mode-hook qv/lsp-setup

  (lsp-headerline-breadcrumb-mode 1)

  ;; Display function signature (eldoc equivalent)
  (lsp-signature-mode 0)
  (setq lsp-signature-auto-activate nil)
  (qv/lsp-signature-mode 0)

  ;; Eldoc is unpredictable in conjunction with lsp
  (eldoc-mode 0)

  ;; Use the default major mode indentation function
  (setq lsp-enable-indentation nil)

  ;; Posframe showing information about a function
  (lsp-ui-doc-mode 0)
  ;;(setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-position 'at-point)

  ;; Enable company with immediate autocompleting
  (company-mode 1)
  (setq company-minimum-prefix-length 1)

  ;; Disable diagnostics in the modeline
  (lsp-modeline-diagnostics-mode 0))

(when lsp-mode (qv/lsp-setup))


;;; Languages
;;;; Python

;; Install python package 'python-lsp-server' for lsp support
;; Install python package 'flake8' for syntax and style checking
;; Install python package 'mypy' for type checking
(defun qv/python-lsp-setup ()
  (interactive)
  (qv/package flycheck-pycheckers)
  (setq flycheck-pycheckers-checkers '(flake8 mypy3))
  (flycheck-pycheckers-setup))


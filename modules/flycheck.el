;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'checkdoc)
(require 'flycheck)
(require 'flycheck-posframe)


;;; Config

(bz/keys flycheck-mode-map
  :sparse t
  "C-c C-p" flycheck-explain-error-at-point
  "C-c C-b" (@ bz/flycheck-reload
               (when (bound-and-true-p bingle-session) (bingle-reload-file))
               (flycheck-stop) (flycheck-buffer))
  "C-c C-j" flycheck-next-error
  "C-c C-k" flycheck-previous-error
  "C-j" flycheck-next-error
  "C-k" flycheck-previous-error

  ;; "C-c C-e" bz/flycheck-posframe-show
  [remap bz/Q] bz/flycheck-posframe-show
  "C-c C-y" (@ bz/flycheck-copy
               (->> (flycheck-overlay-errors-at (point))
                    (flycheck-posframe-format-errors)
                    (kill-new))))

;; (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)


;; Check files in temp directory
(setq flycheck-temp-prefix "flycheck")

(bz/face flycheck-error :u "Red1")
(bz/face flycheck-warning :u "DarkOrange2")
(bz/face flycheck-info :u "yellow green")


;;; Fix broken faces

;; If these faces aren't defined in this way, the whole kitten kaboodle comes crashing down

(flycheck-define-error-level 'lsp-flycheck-error-unnecessary
  :severity 1
  :compilation-level 0
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  :fringe-face 'flycheck-fringe-error)

(defface lsp-flycheck-error-unnecessary
  '((t :inherit flycheck-error))
  "Face for unnecessary LSP errors."
  :group 'lsp-mode)
(defface lsp-flycheck-info-unnecessary
  '((t :inherit flycheck-error))
  "Face for unnecessary LSP errors."
  :group 'lsp-mode)
(flycheck-define-error-level 'lsp-flycheck-info-unnecessary
  :severity -1  ; Adjust severity as needed (e.g., -1 for "unnecessary" info)
  :overlay-category 'flycheck-error-overlay-unnecessary
  :fringe-bitmap 'question-mark
  :fringe-face 'flycheck-fringe-unnecessary)

(bz/face lsp-lsp-flycheck-warning-unnecessary-face :u "DarkOrange")


;;; When to update

(setq-default flycheck-check-syntax-automatically
              '(idle-buffer-switch mode-enabled idle-change new-line))

(bz/hook flycheck-mode-hook bz/flycheck-mode-setup
  (if flycheck-mode
      (add-hook 'after-save-hook 'flycheck-buffer nil t)
    (remove-hook 'after-save-hook 'flycheck-buffer t)))

;; (bz/hook (typescript-mode-hook js-mode-hook) bz/flycheck-disable-idle-check :remove
;;          (setq-local flycheck-check-syntax-automatically '(idle-buffer-switch mode-enabled)))

;; (setq flycheck-idle-change-delay 10)


;;; Flycheck posframe

(setq flycheck-display-errors-delay 0.01)
;; (setq flycheck-display-errors-delay 999999999)
(flycheck-posframe-mode 1)
(setq-default flycheck-display-errors-function #'flycheck-posframe-show-posframe)

(setq-default flycheck-error-message-buffer "*Flycheck error messages*")

;; Show errors in echo area

(setq-default flycheck-clear-displayed-errors-function
              (defun bz/flycheck-posframe-clear-displayed-errors ()
                (ignore-errors (posframe-hide flycheck-posframe-buffer))))

(setq flycheck-posframe-position 'bz/window-bottom-right-corner)

(bz/face flycheck-posframe-background-face :bg "#303444")
(bz/face flycheck-posframe-border-face :fg "gray50")
(bz/face flycheck-posframe-error-face :fg red :w bold)
(bz/face flycheck-posframe-warning-face :fg orange)
(bz/face flycheck-posframe-info-face :fg green)
(setq flycheck-posframe-border-width 1)

;; Manually show the posframe

(bz/face bz/flycheck-message :fg gray1)
(bz/face bz/flycheck-message-code :bg bg2)

(defun bz/flycheck-posframe-show ()
  (interactive)
  ;; For some reason it hides after a second if not in a timer
  ;; (run-with-timer 0 nil #'flycheck-posframe-show-posframe (flycheck-overlay-errors-at (point)))
  (let ((errs (flycheck-overlay-errors-at (point)))
        (mode major-mode)
        beg end rep)
    (bingle-text-posframe
     (with-temp-buffer
       (insert (flycheck-posframe-format-errors errs))
       (put-text-property (point-min) (point-max) 'face 'bz/flycheck-message)
       (goto-char (point-min))
       (while (search-forward-regexp "'\\(.*?\\)'" nil t)
         (ignore-errors
           (setq beg (match-beginning 1) end (match-end 1))
           (let ((pretty (bingle--fontify-for-mode (match-string 1) mode)))
             (delete-region (1- beg) (1+ end))
             (insert (bz/add-face (format "`%s`" pretty) 'bz/flycheck-message-code))
             (forward-char 1))))
       (buffer-string))
     :focus t
     ;; :max-width (- (frame-width) 10)
     :max-width 120
     :max-height 20
     :posframe-args (list :background-color (bz/color bg2)))))


;;; Control what emacs errors to show

(bz/hook emacs-lisp-mode-hook bz/emacs-lisp-setup-flycheck
  (flycheck-mode 1)

  ;; Always
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

  ;; When not in a package
  (when (and buffer-file-name (file-in-directory-p buffer-file-name "~/.emacs.d/modules"))
    (setq-local flycheck-emacs-lisp-load-path 'inherit
                flycheck-emacs-lisp-initialize-packages t)))

;; Disable checkdoc
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

;; Don't complain about missing errors
(setq checkdoc--argument-missing-flag nil)
(add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'checkdoc--argument-missing-flag)

;; Don't require a docstring
(setq checkdoc-force-docstrings-flag nil)

;; Don't complain about verb tense (it confuses plural nouns for verbs all the time)
(setq checkdoc-verb-check-experimental-flag nil)

(setq-default flycheck-emacs-lisp-load-path 'inherit)


;;; et debug

;; Automatically load et
(setq flycheck-emacs-lisp-check-form
      (let ((print-level nil)
            (print-length nil))
        (prin1-to-string (cl-list* 'progn '(ignore-errors (require 'et-compile))
                                   (remove '(ignore-errors (require 'et-compile))
                                           (cdr (read flycheck-emacs-lisp-check-form)))))))


;;; Provide

(provide 'bz-flycheck)

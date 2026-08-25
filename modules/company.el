;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'company)
(require 'company-posframe)
(require 'dash)


;;; Enable globally

(global-company-mode 0)

;; ...but don't autocomplete
(setq company-idle-delay 0)


;;; Basic settings

(setq-default
 ;;company-show-numbers t
 company-show-quick-access t
 company-minimum-prefix-length 1
 company-require-match nil
 company-tooltip-limit 10
 company-tooltip-margin 1
 ;; company-idle-delay 0.0
 ;; company-tooltip-idle-delay 0.0
 company-tooltip-minimum-width 40
 company-tooltip-maximum-width 80
 company-tooltip-width-grow-only t
 company-abort-on-unique-match nil
 )

(setq company-frontends '(company-pseudo-tooltip-frontend))


;;; Faces

(bz/face company-tooltip :bg bg3 :fg fg)
(bz/face company-tooltip-common company-mode)
(bz/face company-tooltip-selection :bg gray3)
(bz/face company-tooltip-common :fg gray1)
(bz/face company-preview :fg gray2 :bg nil)
(bz/face company-preview-common company-preview)
(bz/face company-preview-search company-preview)
(bz/face company-scrollbar-fg :bg gray2)
(bz/face company-scrollbar-bg company-tooltip)


;;; Keybindings

(bz/keys company-active-map
  :sparse t
  "<tab>" company-complete-selection
  ;; "<M-tab>" company-complete-selection
  "<backtab>" company-complete-common
  "C-g" company-abort
  "C-j" company-next-page
  "C-k" company-previous-page
  "C-M-j" company-select-last
  "C-M-k" company-select-first
  "C-S-j" (dotimes (i 4) (company-select-next))
  "C-S-k" (dotimes (i 4) (company-select-previous))
  "M-j" company-select-next
  "M-k" company-select-previous
  "M-J" (dotimes (i 4) (company-select-next))
  "M-K" (dotimes (i 4) (company-select-previous))

  "C-p" bz/company-posframe-quickhelp-show-or-focus)

(bz/keys company-mode-map
  :sparse t
  ;; "M-<tab>" company-complete
  )

(bz/keys bz/insert-map
  "M-<tab>" company-complete)

;; Bind Alt+n to the nth completion
(dotimes (i 10)
  (define-key company-active-map (kbd (format "M-%s" (% (1+ i) 10)))
              (eval `(lambda () (interactive) (company--complete-nth ,i)))))


;;; Company posframe

(when (display-graphic-p)
  (bz/package company-posframe)
  (company-posframe-mode 1)

  (bz/face company-posframe-quickhelp company-tooltip)
  (bz/face company-posframe-quickhelp-header company-tooltip
           :s italic :fg gray1)

  (setq company-posframe-show-indicator nil)
  (setq company-posframe-quickhelp-delay 1000000000000)
  (setq company-posframe-quickhelp-show-header t)
  (setq company-posframe-show-params '(:border-width 1 :border-color "gray50"))

  ;; 69 is the unchangable width of the quickhelp window
  (setq company-tooltip-minimum-width 69
        company-tooltip-maximum-width 69)

  (setq company-posframe-quickhelp-show-params
        (plist-put company-posframe-quickhelp-show-params
                   :poshandler 'company-posframe-quickhelp-bottom-poshandler))

  (defun company-posframe-quickhelp-bottom-poshandler (_info)
    (with-current-buffer company-posframe-buffer
      (let ((pos posframe--last-posframe-pixel-position))
        (let ((h (frame-pixel-height posframe--frame)))
          (cons (car pos)
                (if (< (+ (cdr pos) (* 2 h)) (frame-pixel-height)) (+ (cdr pos) h)
                  (- (cdr pos) (frame-pixel-height posframe--frame) 40))))))))


;;; Disable yasnippets

(bz/hook company-after-completion-hook bz/kill-yasnippet
  (when mark-active (delete-region (point) (mark)))
  (ignore-errors (yas/exit-all-snippets)))


;;; Autocomplete

(setq-default company-idle-delay 0.01)
;; (setq-default company-idle-delay 1)
;; (setq-default company-idle-delay 0.5)

(defun bz/company-autocomplete-mode (&optional arg)
  (interactive)
  ;; Only disable if arg is 0, or arg is nil and its already enabled
  (if (or (eq arg 0) (and (null arg) company-idle-delay))
      (progn (setq-local company-idle-delay nil)
             (message "Autocomplete disabled"))
    (setq-local company-idle-delay 0.01)
    (message "Autocomplete enabled")))

(bz/advise :remove bz/normal company-abort)


;;; Select posframe

(defun bz/company-posframe-quickhelp-show-or-focus ()
  (interactive)
  (-if-let* ((buf (get-buffer company-posframe-quickhelp-buffer))
             (frame (--first (eq buf (cdr (frame-parameter it 'posframe-buffer))) (frame-list)))
             (_ (frame-visible-p frame)))
      (select-frame frame)
    (company-posframe-quickhelp-show)))


;; Override
(defun company-posframe-quickhelp-hide ()
  (unless (memq this-command '(bz/company-posframe-quickhelp-show-or-focus))
    (posframe-hide company-posframe-quickhelp-buffer)))


;;; Don't delete suffix

;; (bz/advise :around company-complete-common bz/company-keep-suffix (func &rest args)
;;   (message "Advising")
;;   (newline)
;;   (backward-char)
;;   (apply func args)
;;   (delete-forward-char))


;;; Provide

(provide 'bz-company)

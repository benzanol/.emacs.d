;;; Vertico
(qv/package vertico)
(vertico-mode 1)
(setq vertico-count 15)

(qv/face minibuffer-prompt :fg blue :w bold)

(qv/keys minibuffer-local-map
  :sparse t
  "C-j" next-history-element
  "C-k" previous-history-element
  "C-g" keyboard-escape-quit
  "TAB" minibuffer-complete
  "<C-return>" exit-minibuffer
  [remap newline] exit-minibuffer)

(qv/keys vertico-map
  :sparse t
  :parent minibuffer-local-map
  [remap qvk-down] vertico-next
  [remap qvk-up] vertico-previous
  [remap qvk-down4] (@ qv/vertico-down4 (vertico-next 4))
  [remap qvk-up4] (@ qv/vertico-up4 (vertico-previous 4))
  [remap end-of-buffer] vertico-last
  [remap beginning-of-buffer] vertico-first

  [remap newline] vertico-exit
  [remap minibuffer-complete] vertico-insert
  [remap minibuffer-exit] vertico-exit-input)

;;; Read file name
(setq read-file-name-function 'qv/read-file-name)

(defun qv/read-file-backspace ()
  (interactive)
  (if (not (string= "/" (substring (minibuffer-contents) -1)))
      (delete-backward-char 1)
    (let ((pos (point)))
      (backward-char 1)
      (or (and (search-backward "/" nil t) (prog1 t (forward-char 1)))
          (beginning-of-line))
      (delete-region (point) pos))))

(defun qv/read-file-name (&rest args)
  (define-key minibuffer-local-map (kbd "DEL")
    'qv/read-file-backspace)
  (unwind-protect
      (apply 'read-file-name-default args)
    (define-key minibuffer-local-map (kbd "DEL") nil)))

;;; Custom Format
(defun qv/vertico-format-args (args)
  (let ((cand (car args)) (pos 0) (max (length (car args))))
    (while (< pos max)
      (let* ((face (get-text-property pos 'face cand))
             (newface (if face `(:inherit (,face variable-pitch)) 'variable-pitch))
             (change (or (next-property-change pos cand) max)))
        (put-text-property pos change 'face newface cand)
        (setq pos change)))
    (cons cand (cdr args))))
(advice-add 'vertico--format-candidate :filter-args 'qv/vertico-format-args)
;;(advice-remove 'vertico--format-candidate 'qv/vertico-format-args)

;;; Variable pitch prompt
(defvar qv/variable-pitch-minibuffer nil
  "Whether to make the user typed minibuffer prompt variable pitch.")

(qv/hook minibuffer-setup-hook qv/set-prompt-end
  (when qv/variable-pitch-minibuffer
    (setq qv/prompt-end (point))
    (if (eq (get-text-property (1- (point)) 'face) 'minibuffer-prompt)
        (add-hook 'post-command-hook 'qv/variable-minibuffer nil t)
      (remove-hook 'post-command-hook 'qv/variable-minibuffer t))))

(defun qv/variable-minibuffer ()
  (let ((inhibit-read-only t))
    (put-text-property qv/prompt-end (point-max)
                       'face 'variable-pitch)))

;;; Vertico posframe
(qv/package vertico-posframe)
(vertico-posframe-mode 0)

(setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
(setq vertico-posframe-width 90)
(setq vertico-posframe-height 20)

;;; Prescient
(qv/package prescient)

(prescient-persist-mode)
(setq prescient-sort-full-matches-first t
      prescient-save-file "~/.emacs.d/.cache/prescient/prescient-save-el")

;;; Orderless
(qv/package orderless)
(setq completion-styles '(orderless initials))
;;(setq completion-styles '(basic partial-completion emacs22 substring initials))

;;; Marginalia
(qv/package marginalia)
(marginalia-mode 1)

(setq qv/marginalia-variable-faces
      '(marginalia-documentation))

(qv/face marginalia-documentation variable-pitch :s italic)

;;; Embark
(qv/package embark)

(qv/key minibuffer-local-map "C-a" embark-act)

;;(setq embark-prompter 'embark-keymap-prompter)
(setq embark-prompter 'embark-completing-read-prompter)
(setq embark-indicators nil)

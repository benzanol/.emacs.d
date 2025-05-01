;;; Packages

(bz/package vertico)
(vertico-mode 1)
(setq vertico-count 15)

(bz/face minibuffer-prompt :fg blue :w bold)
(bz/face vertico-current :bg bg3)


(bz/package orderless)
(setq completion-styles '(orderless initials))
(bz/face match :w bold :s italic :fg nil :bg nil)
(bz/face orderless-match-face-0 :w bold :s italic :fg fg)
(bz/face orderless-match-face-1 :w bold :s italic :fg fg)
(bz/face orderless-match-face-2 :w bold :s italic :fg fg)
(bz/face orderless-match-face-3 :w bold :s italic :fg fg)
;;(setq completion-styles '(basic partial-completion emacs22 substring initials))


(bz/package marginalia)
(marginalia-mode 1)
(bz/face marginalia-documentation variable-pitch :s italic)
;; Marginalia leads to the weird filtering where it doesn't show all buffers that match
(push '(bz/switch-to-buffer . bz/buffer) marginalia-command-categories)


(bz/package embark)
;;(setq embark-prompter 'embark-keymap-prompter)
(setq embark-prompter 'embark-completing-read-prompter)
(setq embark-indicators nil)


;;; Keybindings

(bz/keys minibuffer-local-map
  :sparse t
  "C-j" next-history-element
  "C-k" previous-history-element
  "C-g" keyboard-escape-quit
  "TAB" minibuffer-complete
  "<C-return>" exit-minibuffer
  [remap newline] exit-minibuffer)

(bz/keys read-expression-map :sparse t :parent minibuffer-local-map)

(bz/keys vertico-map
  :sparse t
  :parent minibuffer-local-map

  [remap bz/up] vertico-previous
  [remap bz/down] vertico-next
  [remap bz/up4] (@ bz/vertico-up4 (vertico-previous 4))
  [remap bz/down4] (@ bz/vertico-down4 (vertico-next 4))
  [remap beginning-of-buffer] vertico-first
  [remap end-of-buffer] vertico-last

  [remap newline] vertico-exit
  [remap minibuffer-complete] vertico-insert
  [remap minibuffer-exit] vertico-exit-input

  "C-p" (@ bz/vertico-insert-candidate
           (run-with-timer 0 nil #'insert (vertico--candidate))
           (abort-minibuffers))

  "C-a" embark-act)


;;; Read file name

(setq read-file-name-function 'bz/read-file-name)

(defun bz/read-file-backspace ()
  (interactive)
  (if (not (string= "/" (substring (minibuffer-contents) -1)))
      (delete-backward-char 1)
    (let ((pos (point)))
      (backward-char 1)
      (or (and (search-backward "/" nil t) (prog1 t (forward-char 1)))
          (beginning-of-line))
      (delete-region (point) pos))))

(bz/keys bz/read-file-name-map
  :sparse t
  :parent minibuffer-local-map
  "DEL" bz/read-file-backspace)

(defun bz/read-file-name (&rest args)
  (define-key minibuffer-local-map (kbd "DEL")
    'bz/read-file-backspace)
  (unwind-protect
      (apply 'read-file-name-default args)
    (define-key minibuffer-local-map (kbd "DEL") nil)))


;;; Variable pitch candidates

(setq bz/fixed-pitch-minibuffer-commands
      '(video-set-mark video-goto-mark ytdlp-download bz/read-font))

(bz/advise :before vertico--format-candidate bz/vertico-format-args (cand &rest args)
  (unless (memq current-minibuffer-command bz/fixed-pitch-minibuffer-commands)
    (add-face-text-property 0 (length cand) 'variable-pitch nil cand)))

;;; Colorize minibuffers

(bz/face bz/minibuffer :bg bg2 :h 1.0)
;; (bz/face bz/echo-area :bg bg2 :h 1.0 :family "TeX Gyre Adventor")
(bz/face bz/echo-area :bg bg2 :h 1.0)

(defun bz/minibuffer-colorize ()
  ;; Because otherwise Minibuf-0 doesn't properly display
  (ignore-errors
    (when (eq (point-min) (point-max)) (insert " "))
    (if (string= (buffer-name) " *Minibuf-1*")
        (buffer-face-set 'bz/minibuffer)
      (buffer-face-set 'bz/echo-area))))

(setq bz/colorized-minibuffers '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*" " *Minibuf-1*"))
(dolist (buf bz/colorized-minibuffers)
  (with-current-buffer (get-buffer-create buf) (bz/minibuffer-colorize)))

(bz/hook minibuffer-mode-hook bz/minibuffer-colorize)
(bz/hook minibuffer-inactive-mode-hook bz/minibuffer-colorize)


;;; Limit echo area to one line

(bz/hook post-command-hook bz/trim-echo-area
  (let ((inhibit-read-only t) (inhibit-modification-hooks t))
    (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
      (with-current-buffer buf
        (when (and (buffer-modified-p) (> (point-max) 1))
          (goto-char (point-min))
          (while (and (search-forward "\n" nil t) (<= (1+ (point)) 160))
            (delete-region (match-beginning 0) (match-end 0))
            (insert (propertize "\\n" 'face 'shadow)))
          (when (> (point-max) 160)
            (delete-region 160 (point-max)))

          (set-buffer-modified-p nil))))))

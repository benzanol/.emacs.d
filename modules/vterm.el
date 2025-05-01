(bz/package vterm)
(bz/package multi-vterm)


(setq vterm-shell "zsh")

(bz/face vterm-color-red :fg "red" :bg "red")
(bz/face vterm-color-blue :fg blue :bg blue)
(bz/face vterm-color-green :fg green :bg green)
(bz/face vterm-color-yellow :fg yellow :bg yellow)
(bz/face vterm-color-black :fg gray2 :bg gray2)


(defun bz/vterm ()
  (interactive)
  (let* ((terms (--filter (eq 'vterm-mode (buffer-local-value 'major-mode it))
                          (buffer-list)))
         (names (--map (replace-regexp-in-string
                        "\\*vterm \\(.*\\)\\*" "\\1"
                        (buffer-name it))
                       terms))
         (name (completing-read "Name: " names))
         (vterm-name (format "*vterm %s*" name))
         (exist (or (get-buffer vterm-name) (get-buffer name))))

    (if (and exist (eq 'vterm-mode (buffer-local-value 'major-mode exist)))
        (switch-to-buffer exist)

      (multi-vterm)
      (when (not (string= name ""))
        (rename-buffer vterm-name)))))


;; BLAZINGLY fast
(setq vterm-timer-delay 0)


(setq vterm-mode-map (make-sparse-keymap))
(vterm--exclude-keys vterm-mode-map nil)
;; Remove all meta bindings
(define-key vterm-mode-map (kbd "ESC") nil)

(bz/keys vterm-mode-map
  "<escape>" (vterm-send-string "")
  "<backspace>" (vterm-send-string "")
  "<return>" (vterm-send-string "")
  "<M-return>" (vterm-send-string "")
  "<tab>" (vterm-send-string "	")
  "M-<tab>" (vterm-send-string "	")

  "C-S-v" (vterm-insert (current-kill 0))
  "C-S-p" (vterm-insert (getenv "GITHUB_TOKEN"))

  "<up>" vterm--self-insert
  "<down>" vterm--self-insert
  "<left>" vterm--self-insert
  "<right>" vterm--self-insert
  "<M-left>" vterm-send-M-b
  "<M-right>" vterm-send-M-f

  "M-q" vterm-copy-mode
  [remap bz/keyboard-quit] vterm-copy-mode

  "C-\\" nil
  "C-\\ C-\\" vterm--self-insert
  "C-\\ C-n" vterm-copy-mode)

(bz/keys vterm-copy-mode-map
  :sparse t
  [remap bz/insert] vterm-copy-mode)


(bz/hook vterm-mode-hook bz/vterm-setup
  (yas-minor-mode 0)
  (display-line-numbers-mode 0)
  (bz/recenter-cursor-mode 0)
  (company-mode 0))

(bz/hook vterm-copy-mode-hook bz/vterm-copy-setup
  (bz/recenter-cursor-mode (if vterm-copy-mode 1 0))
  (if vterm-copy-mode (bz/normal) (bz/nokeys)))

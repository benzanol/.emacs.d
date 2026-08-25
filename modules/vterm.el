;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-keys)
(require 'bz-recenter)

(require 'dash)
(require 'multi-vterm)
(require 'vterm)


(setq vterm-shell "zsh")

(bz/face bz/vterm :fg "#d0cfcc")

(bz/face vterm-color-red :fg red :bg red)
(bz/face vterm-color-blue :fg blue :bg blue)
(bz/face vterm-color-cyan :fg cyan :bg cyan)
(bz/face vterm-color-green :fg green :bg green)
(bz/face vterm-color-yellow :fg yellow :bg yellow)
(bz/face vterm-color-magenta :fg purple :bg purple)
(bz/face vterm-color-black :fg "#707480" :bg "#707480")

(bz/face term-color-red :fg red :bg red)
(bz/face term-color-blue :fg blue :bg blue)
(bz/face term-color-cyan :fg cyan :bg cyan)
(bz/face term-color-green :fg green :bg green)
(bz/face term-color-yellow :fg yellow :bg yellow)
(bz/face term-color-magenta :fg purple :bg purple)
(bz/face term-color-black :fg gray3 :bg gray3)

;; (bz/face vterm-color-underline :fg fg :bg bg2 :u nil)


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


(bz/keys vterm-mode-map :sparse t)
(vterm--exclude-keys vterm-mode-map nil)
;; Remove all meta bindings
(define-key vterm-mode-map (kbd "ESC") nil)

;; Fixed window size
(defvar-local bz/vterm-window-size nil)
(defun bz/vterm-window-size (&rest _)
  (or bz/vterm-window-size (cons 80 (frame-height))))

(bz/keys vterm-mode-map
  "<escape>" (vterm-send-string "")
  "<backspace>" (vterm-send-string "")
  "<return>" (vterm-send-string "")
  "<M-return>" (vterm-send-string "")
  "<tab>" (vterm-send-string "	")
  "<backtab>" (vterm-send-string "[Z")
  "M-<tab>" (vterm-send-string "	")
  "C-<backspace>" (vterm-send-string "")
  "M-<backspace>" (vterm-send-string "")
  "C-<delete>" (vterm-send-string "d")
  "M-<delete>" (vterm-send-string "")
  "<C-left>" vterm-send-M-b
  "<C-right>" vterm-send-M-f

  "S-<return>" vterm-self-insert

  "C-S-v" (vterm-insert (current-kill 0))
  "C-S-g" (vterm-insert (getenv "GITHUB_TOKEN"))
  "C-S-p" wosp-terminal-run-or-add-pinned
  "C-S-r" wosp-terminal-remove-pinned
  "C-S-s" wosp-terminal-set-startup-command
  "C-S-a" wosp-terminal-set-action
  "C-S-t" (vterm-send-string "require(\"./build/pipeline.js\").runExample()\r\n")
  "C-S-l" (@ bz/lock-process-window-size
             (if (eq window-adjust-process-window-size-function #'bz/vterm-window-size)
                 (progn (kill-local-variable 'window-adjust-process-window-size-function)
                        (message "Terminal size unlocked"))
               (setq-local bz/vterm-window-size (cons (max 10 (- (window-width) 5)) (window-height))
                           window-adjust-process-window-size-function #'bz/vterm-window-size)
               (message "Terminal size locked at %s" bz/vterm-window-size))
             (set-window-hscroll nil 0))

  "<up>" vterm--self-insert
  "<down>" vterm--self-insert
  "<left>" vterm--self-insert
  "<right>" vterm--self-insert
  "C-/" vterm--self-insert
  "C-?" vterm--self-insert

  "M-q" vterm-copy-mode
  [remap bz/keyboard-quit] vterm-copy-mode

  "C-\\" nil
  "C-\\ C-\\" vterm--self-insert
  "C-\\ C-n" vterm-copy-mode)

(bz/keys vterm-copy-mode-map
  :sparse t
  [remap bz/insert] vterm-copy-mode)


(bz/hook vterm-mode-hook bz/vterm-setup
  ;; (setq-local window-adjust-process-window-size-function #'bz/vterm-window-size)
  (buffer-face-mode-invoke 'bz/vterm 1)

  (yas-minor-mode 0)
  (display-line-numbers-mode 0)
  (bz/recenter-cursor-mode 0)
  (company-mode 0))

(bz/hook vterm-copy-mode-hook bz/vterm-copy-setup
  (bz/recenter-cursor-mode (if vterm-copy-mode 1 0))
  (if vterm-copy-mode (bz/normal) (bz/nokeys)))



(bz/face bz/claude-code-diff-add :bg "#284734")
(bz/face bz/claude-code-diff-remove :bg "#5f3033")
(defun bz/vterm-replace-colors ()
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (pred (lambda (_ spec)
                (and (listp spec)
                     (or (member (plist-get spec :foreground) '("#333333"))
                         (member (plist-get spec :background) `("#373737" "#003300" "#330000"))))))
        (face-prop 'font-lock-face)
        match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward face-prop nil pred))
        (let ((face (prop-match-value match)))
          (when (equal (plist-get face :foreground) "#333333") (plist-put face :foreground "#808490"))
          (when (equal (plist-get face :background) "#373737") (plist-put face :background (bz/color bg2)))
          ;; (when (equal (plist-get face :background) "#003300") (plist-put face :background ))
          (when (equal (plist-get face :background) "#003300")
            (plist-put face :background 'unset)
            (setq face (list 'bz/claude-code-diff-add face)))
          (when (equal (plist-get face :background) "#330000")
            (plist-put face :background 'unset)
            (setq face (list 'bz/claude-code-diff-remove face)))

          (put-text-property (prop-match-beginning match) (prop-match-end match) face-prop face))))))

(bz/advise :after vterm--redraw bz/around-vterm-redraw (&rest args)
  (when (get-buffer-window (current-buffer))
    ;; (message "Drawing")
    ;; (apply func args)
    (bz/vterm-replace-colors)))


;;; Provide

(provide 'bz-vterm)

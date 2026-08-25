;;; Doom modeline  -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'doom-modeline)


(doom-modeline-mode)
(setq doom-modeline-height 28)
(setf (alist-get 'exwm-mode all-the-icons-mode-icon-alist)
      '(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1 :face all-the-icons-purple))

(setq doom-modeline-buffer-encoding nil)

;; (setq global-mode-string '("%e" (:eval (propertize (format-time-string "%H:%M:%S %b %d ") 'face 'italic))))
(setq global-mode-string nil)

;; In non-file buffers, don't use the file modified face for the buffer file name
(bz/advise :around doom-modeline-face bz/doom-modeline-face-advice (func &optional face inactive-face)
  (if (and (eq face 'doom-modeline-buffer-modified)
           (null buffer-file-name))
      'doom-modeline-buffer-file
    (funcall func face inactive-face)))


;;; Doom modleine layout

(doom-modeline-def-segment bz/wingroup
  (let* ((sym (window-parameter (selected-window) 'window-instance)))
    (if sym (format ">>%s<<" sym)
      "--")))

(doom-modeline-def-segment bz/buffer-position
  "The current character position (point)."
  (format " %%l:%%c %s " (point)))

(doom-modeline-def-segment bz/selection-info
  "Selection info."
  (when mark-active
    (propertize
     (format " %sC%s%s"
             (abs (- (point) (mark)))
             (let ((words (length (s-split-words (buffer-substring-no-properties (point) (mark))))))
               (if (= words 1) "" (format " %sW" words)))
             (let ((lines (abs (- (line-number-at-pos) (line-number-at-pos (mark))))))
               (if (= lines 1) "" (format " %sL" lines))))
     'face 'bold)))

(doom-modeline-def-segment bz/check
  "Displays color-coded error status in the current buffer with pretty icons."
  (when-let* ((sep (doom-modeline-spc))
              (vsep (doom-modeline-vspc))
              (seg (cond
                    ((bound-and-true-p flymake-mode)
                     doom-modeline--flymake)
                    ((bound-and-true-p flycheck-mode)
                     doom-modeline--flycheck))))
    (ignore-errors
      (concat
       sep
       (propertize
        ;; No good way to check for eglot/flymake
        (if (and (bound-and-true-p flycheck-mode) (flycheck-running-p))
            (concat (all-the-icons-material "history" :face '(shadow bold)) sep)
          (let ((str
                 (cl-loop for tail on (split-string seg " ") by #'cddr
                          for idx upfrom 0
                          concat (if (equal "0" (cadr tail)) ""
                                   (let* ((num (doom-modeline-display-text (cadr tail)))
                                          (face (get-text-property 0 'face num)))
                                     (concat (propertize (pcase idx (0 "{") (1 "[") (_ "(")) 'face face)
                                             num
                                             (propertize (pcase idx (0 "}") (1 "]") (_ ")")) 'face face)
                                             sep))))))
            (if (not (s-blank-p str)) str
              (concat (all-the-icons-material "check" :face 'doom-modeline-info) sep))))
        'help-echo (get-text-property 0 'help-echo seg)
        'mouse-face 'doom-modeline-highlight
        'local-map (get-text-property 0 'local-map seg))
       sep))))

;; #ffa500
(bz/face bz/mc2-modeline-all :fg "orange" :w bold)
(bz/face bz/mc2-modeline-one :fg "SkyBlue3" :w bold)
(bz/face bz/mc2-modeline-none :fg "DarkOliveGreen3" :w bold)
(doom-modeline-def-segment bz/mc2
  "Multicursor count"
  (if (null (bound-and-true-p mc2-mode)) ""
    (propertize (format "[%s]" (length mc2-cursors)) 'face
                (intern (format "bz/mc2-modeline-%s" mc2-mode)))))

(doom-modeline-def-modeline 'bz/modeline
  '(bar matches buffer-info remote-host bz/buffer-position bz/selection-info bz/mc2)
  '(bz/wingroup misc-info minor-modes input-method buffer-encoding major-mode process vcs bz/check))


(doom-modeline-set-modeline 'bz/modeline 'default)
(dolist (buf (buffer-list))
  (with-current-buffer buf (doom-modeline-set-modeline 'bz/modeline)))


;;; Provide

(provide 'bz-modeline)

;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-tabline-original)

(require 'cl-seq)
(require 'dash)
(require 'tab-line)


;; This file is just for tabline configuration, and should be
;; decoupled from various tabline organization methods (tabline-*)
;; that I have attempted.


;;; Faces

(setq tab-line-close-button-show nil)
(setq tab-line-new-button-show nil)

(bz/face tab-line variable-pitch :h 1.025 :s normal :fg gray1 :bg bg2 :iv nil)
(bz/face tab-line-tab tab-line :fg nil :bg nil :box (:line-width 2 :color gray3))
(bz/face tab-line-tab-current tab-line-tab :fg nil :bg nil :w bold :fg green)
(bz/face tab-line-tab-inactive tab-line-tab :fg gray1 :bg nil :fg blue)
(bz/face tab-line-tab-modified nil :s italic)

;; (bz/face tab-line-tab-special :s normal)

;; Don't apply the special face
(bz/advise :override tab-line-tab-face-special
           bz/tab-line-tab-face-special-advice (_tab _tabs face _buffer-p _selected-p)
  face)


;;; Keybindings

(bz/keys tab-line-mode-map
  :doc "Keymap for tab-line-mode"
  :sparse t
  "M-a" tab-line-switch-to-prev-tab
  "M-d" tab-line-switch-to-next-tab
  "M-q" (@ bz/tab-line-left (bz/tab-line-move 'left))
  "M-e" (@ bz/tab-line-right (bz/tab-line-move 'right))
  "M-s" bz/tab-line-add-directory
  "M-w" bz/tab-line-remove-directory
  "M-W" (@ bz/tab-line-kill-other-tabs
           (dolist (buf (cdr bz/tab-line-tabs))
             (unless (eq buf (current-buffer))
               (kill-buffer buf)))))


;;; Custom tab name format

(setq tab-line-tab-name-function 'bz/tab-line-name)

(defun bz/tab-line-name (buffer &optional buffers)
  (let ((str (buffer-name (get-buffer buffer))))
    ;; Show index files as the directory name
    (setq str (replace-regexp-in-string "^index\\.\\(.*?\\)<\\(.*\\)>$" "<\\2>.\\1" str))
    (when (string-match-p "<.*>$" str)
      (cl-loop with name = (file-name-nondirectory (buffer-file-name buffer))
               for other in buffers
               when (and (not (eq other buffer))
                         (equal name (file-name-nondirectory (buffer-file-name other))))
               return nil
               finally do (setq str name)))

    (if (string-match "\\*terminal/.*?:\\([^:]*\\)\\*" str)
        (setq str (format "⏵%s" (match-string 1 str)))
      (setq str (concat "  " str)))

    (concat str "  ")))


;;; Provide

(provide 'bz-tabline)

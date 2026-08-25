;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-keys)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-packages/lens/"))
(require 'lens)
;; (require 'lui)

;; (load-file "~/.emacs.d/my-packages/lens/ui/chat.el")

;; (bz/hook lui-text-box-enter-hook bz/insert)
;; (bz/hook lui-text-box-exit-hook bz/normal)

;; (bz/keys *
;;   "C-x C-i" (@ bz/lui-delete-or-insert
;;                (if-let ((r (lens-at-point t))) (lens-remove r)
;;                  (call-interactively 'lui-insert-ui))))


;;; Provide

(provide 'bz-lens)

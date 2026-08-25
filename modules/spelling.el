;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'flyspell)


(bz/hook prog-mode-hook bz/flyspell-prog-mode
  (flyspell-prog-mode)
  ;; (run-with-timer 0.5 nil #'flyspell-buffer)
  )

(bz/hook (org-mode-hook text-mode-hook) flyspell-mode)


(setq flyspell-duplicate-distance 0)

(bz/face flyspell-incorrect nil :u (:style wave :color "skyblue2"))
(bz/face flyspell-duplicate nil :u nil)
(bz/face lazy-highlight nil :b (:color "skyblue3"))

(setq ispell-help-timeout 1000)


(bz/keys bz/flyspell-actions-map
  :sparse t
  "S i" ispell-buffer
  "S s" flyspell-mode
  "S p" flyspell-prog-mode
  "S j" flyspell-goto-next-error
  "S b" flyspell-buffer)

(bz/keys flyspell-mode-map
  :sparse t
  [remap bz/spell-check] ispell-word
  [remap bz/spell-actions] ,bz/flyspell-actions-map)


;;; Provide

(provide 'bz-spelling)
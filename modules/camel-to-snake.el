;;  -*- lexical-binding: t; -*-

(define-minor-mode camel-to-snake-mode
  "Converts camel case to snake case in real time.

This only activates when the variable starts with a lowercase
letter, so that it will not disrupt typing of upper camel case."
  :global nil
  :init nil
  (if camel-to-snake-mode
      (add-hook 'post-self-insert-hook 'camel-to-snake-post-command nil 'local)
    (remove-hook 'post-self-insert-hook 'camel-to-snake-post-command 'local)))

(defun camel-to-snake-post-command ()
  (let ((case-fold-search nil))
    (when (and (string-match-p "[A-Z]" (this-command-keys))
               ;; Make sure the symbol starts with lowercase
               (save-excursion
                 (while (looking-back "[a-zA-Z0-9_]" 1) (backward-char))
                 (looking-at-p "_*[a-z]")))
      (delete-backward-char 1)
      (insert "_" (downcase (this-command-keys))))))


;;; Provide

(provide 'bz-camel-to-snake)

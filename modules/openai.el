(require 'json)
(require 'dash)

(setq qv/openai-key "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")

(qv/keys *
  "C-x C-o" qv/openai-replace)

(qv/keys openai-overlay-map
  :sparse t
  "RET" qv/openai-keep
  "q" qv/openai-revert
  "r" qv/openai-replace)

(defun qv/openai-keep ()
  "Keep the current openai generated text."
  (interactive)
  (when-let ((o (--first (overlay-get it 'openai) (overlays-at (point)))))
    (delete-overlay o)))

(defun qv/openai-revert ()
  "Delete the openai generated text and restore the prompt."
  (interactive)
  (when-let ((o (--first (overlay-get it 'openai) (overlays-at (point)))))
    (delete-region (overlay-start o) (overlay-end o))
    (insert (overlay-get o 'prompt))
    (delete-overlay o)))

(defun qv/openai-replace (&optional arg beg end)
  "Use the text in the current line, or in the active region, as an openai prompt.

Asynchronously fetch a response from openai, then replace the prompt text, and give
the user the option to keep the text, generate new text, or revert the text.

If arg (prefix argument) is non-nil, then keep the original prompt text."

  (interactive "P")

  (let ((o (or (--first (overlay-get it 'openai) (overlays-at (point)))
               (make-overlay
                (or beg (and mark-active (mark)) (line-beginning-position))
                (or end (and mark-active (point)) (line-end-position))
                nil t nil))))
    (if (overlay-get o 'openai)
        (progn (delete-region (overlay-start o) (overlay-end o))
               (goto-char (overlay-start o))
               (insert (overlay-get o 'prompt))
               (move-overlay o (overlay-start o) (point))
               (goto-char (overlay-start o)))

      (overlay-put o 'face (list :background (qv/color gray3)))
      (overlay-put o 'keymap openai-overlay-map)
      (overlay-put o 'openai t)
      (overlay-put o 'prompt (buffer-substring-no-properties
                              (overlay-start o) (overlay-end o))))

    (qv/openai-generate
     (overlay-get o 'prompt)
     `(lambda (str)
        (switch-to-buffer (overlay-buffer ,o))
        (if ',arg (progn (goto-char (overlay-end ,o)) (newline))
          (delete-region (overlay-start ,o) (overlay-end ,o))
          (goto-char (overlay-start ,o)))
        (insert str)
        (move-overlay ,o (overlay-start ,o) (point))
        (goto-char (overlay-start ,o))))))

(defun qv/openai-generate (prompt func)
  "FUNC will be called with 1 argument, the openai output"

  (let* ((words (reverse (split-string prompt " ")))
         (keys '(:max :temp))
         props)

    (while (memq (intern (cadr words)) keys)
      (push (string-to-number (pop words)) props)
      (push (intern (pop words)) props))

    (make-process
     :name "openai"

     :filter
     `(lambda (p &rest args)
        (thread-last (car args)
          (json-read-from-string)
          (alist-get 'choices)
          ((lambda (x) (aref x 0)))
          (alist-get 'text)
          (replace-regexp-in-string "\\`\n+" "")
          (funcall ',func)))

     :command
     (list
      "curl" "https://api.openai.com/v1/completions"
      "-H" "Content-Type: application/json"
      "-H" (concat "Authorization: Bearer " qv/openai-key)
      "-d"
      (json-encode
       `((model . text-davinci-002)
         (prompt . ,(string-join (reverse words) " "))
         (temperature . ,(or (plist-get props :temp) 0.7))
         (max_tokens . ,(or (plist-get props :max) 256))
         (top_p . 1)
         (frequency_penalty . 0)
         (presence_penalty . 0)))))))


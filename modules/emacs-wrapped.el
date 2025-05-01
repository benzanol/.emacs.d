(defvar emacs-wrapped-file "~/.emacs.d/emacs-wrapped.json")
(defvar emacs-wrapped-save-delay 20)
(defvar emacs-wrapped-track-delay 5)
(defvar emacs-wrapped nil)

(defun emacs-wrapped-save ()
  (interactive)
  (dolist (assoc emacs-wrapped)
    (setcdr assoc (--sort (> (cdr it) (cdr other)) (cdr assoc))))

  (with-temp-buffer
    (json-insert emacs-wrapped)
    (json-pretty-print (point-min) (point-max))
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) emacs-wrapped-file))))

(defmacro emacs-wrapped-increment-place (category entry &optional amount)
  `(let ((val (alist-get ,entry (alist-get ,category emacs-wrapped))))
     (setf (alist-get ,entry (alist-get ,category emacs-wrapped))
           (+ (or ,amount 1) (or val 0)))))


(defun emacs-wrapped--track-action ()
  (when (symbolp this-command)
    (emacs-wrapped-increment-place 'actions this-command)))

(defun emacs-wrapped--track-command-advice (arg &optional name typed)
  (and name (stringp name)
       (emacs-wrapped-increment-place 'commands (intern name))))

(defun emacs-wrapped--track-on-timer ()
  (emacs-wrapped-increment-place 'modes major-mode emacs-wrapped-track-delay)

  (when buffer-file-name
    (emacs-wrapped-increment-place
     'files (intern (expand-file-name buffer-file-name))
     emacs-wrapped-track-delay)))


(defun emacs-wrapped-enable ()
  (interactive)
  (emacs-wrapped-disable)
  (let ((json-key-type 'symbol))
    (setq emacs-wrapped (ignore-errors (json-read-file emacs-wrapped-file))))

  (run-with-timer 0 emacs-wrapped-save-delay #'emacs-wrapped-save)

  (add-hook 'post-command-hook #'emacs-wrapped--track-action)
  (advice-add #'execute-extended-command :before #'emacs-wrapped--track-command-advice)
  (run-with-timer 0 emacs-wrapped-track-delay #'emacs-wrapped--track-on-timer))

(defun emacs-wrapped-disable ()
  (interactive)
  (cancel-function-timers #'emacs-wrapped-save)

  (remove-hook 'post-command-hook #'emacs-wrapped--track-action)
  (advice-remove 'execute-extended-command #'emacs-wrapped--track-command-advice)
  (cancel-function-timers #'emacs-wrapped--track-on-timer))

(define-minor-mode emacs-wrapped-mode
  "Track the user's actions to display at the end of the year."
  :global t
  :init-value nil
  (if emacs-wrapped-mode
      (emacs-wrapped-enable)
    (emacs-wrapped-disable)))

(qv/package eww)

(qv/keys eww-mode-map
  :sparse t
  "h" nil "l" nil
  "H" nil "L" nil
  "n" nil "v" nil
  "g" nil
  "r" eww-reload
  "p" eww-back-url
  "P" eww-forward-url
  "o" eww
  "O" ((setq current-prefix-arg '(4)) (call-interactively 'eww))
  "S" shr-save-contents
  "RET" eww-follow-link
  "<return>" eww-follow-link
  "<C-return>" eww-follow-link
  "<S-return>" eww-open-in-new-buffer
  "c u" (qv/copy (plist-get eww-data :url))
  [remap tab-line-close-tab] (kill-buffer (current-buffer)))

(qv/keys eww-link-keymap
  :sparse t)

(qv/keys eww-image-link-keymap
  :sparse t
  "a" shr-show-alt-text
  "S" image-save)


;;; Eww tab group
(setq qv/eww-tabs '(tabs))
(qv/hook eww-mode-hook qv/eww-setup
  (nconc qv/eww-tabs (list (current-buffer)))
  (setq-local qv/tab-line-tabs qv/eww-tabs)

  (let ((tab-line-mode-hook nil))
    (tab-line-mode 1)))

;;; Custom faces
(qv/require custom)
(qv/face eww-form-checkbox :fg nil :bg nil)
(qv/face eww-form-submit custom-button :fg nil :bg nil :w bold
         :b (:color ,(qv/color gray3)))

;;; Favicons

(defun qv/eww-generate-title ()
  (let* ((title (plist-get eww-data :title)))

    (when (string= title "")
      (setq title (qv/eww-base-url)))

    (if (<= (length title) 15) title
      (concat (substring title 0 12) "..."))))

(defun qv/eww-base-url ()
  (replace-regexp-in-string
   "^\\(https?://\\)?\\([^/]*\\.\\)?\\([^/.]+\\.[^/.]+\\)\\(/.*\\)$" "\\3"
   (plist-get eww-data :url)))

(setq qv/eww-current-url nil)
(defun qv/eww-download-favicon (&rest args)
  (interactive)
  (when-let*
      ((*eww-mode* (eq major-mode 'eww-mode))
       (url (qv/eww-base-url))
       (*continue* (unless (equal url qv/eww-current-url)
                     (setq qv/eww-current-url url)))
       (favicon (format "https://www.google.com/s2/favicons?domain=%s" url))
       (hash (sxhash url))
       (dir "/tmp/eww-favicons/")
       (file (format "%s%s" dir hash))
       (filter
        `(lambda (p &rest args)
           (with-current-buffer ,(current-buffer)
             (when (and (not (process-live-p p)) (equal (qv/eww-base-url) ,url))
               (let* ((prop (list '(raise 0.3) (create-image ,file)))
                      (n (format "%s%s %s"
                                 (propertize "@" 'display '(height 0.1))
                                 (propertize " " 'display prop)
                                 (qv/eww-generate-title))))
                 (rename-buffer n)))))))

    (message "FAVICON!!!")

    (rename-buffer (format "@ %s" (qv/eww-generate-title)))

    (unless (f-directory-p dir) (make-directory dir t))

    (if (file-exists-p file)
        (funcall filter nil)
      (make-process
       :name "eww-favicon"
       :command (list "wget" favicon "-O" file)
       :filter filter))))

(setq qv/eww-favicon-timer (run-with-timer 1 1 'qv/eww-download-favicon))

;;(advice-add 'eww :after 'qv/eww-download-favicon)
;;(advice-add 'eww-browse-url :after 'qv/eww-download-favicon)
;;(advice-add 'eww-reload :after 'qv/eww-download-favicon)
;;(advice-add 'eww-restore-history :after 'qv/eww-download-favicon)

;;; Open org urls
(qv/hook org-open-at-point-functions qv/org-eww-at-point
  (let ((context (org-element-context)))
    (if (string= (plist-get (cadr context) :type) "https")
        (switch-to)
        (eww)

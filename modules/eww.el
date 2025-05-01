(bz/package eww)

(bz/keys eww-mode-map
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
  "c u" (bz/copy (plist-get eww-data :url))
  [remap tab-line-close-tab] (kill-buffer (current-buffer)))

(bz/keys eww-link-keymap
  :sparse t)

(bz/keys eww-image-link-keymap
  :sparse t
  "a" shr-show-alt-text
  "S" image-save)


;;; Eww tab group
(setq bz/eww-tabs '(tabs))
(bz/hook eww-mode-hook bz/eww-setup
  (nconc bz/eww-tabs (list (current-buffer)))
  (setq-local bz/tab-line-tabs bz/eww-tabs)

  (let ((tab-line-mode-hook nil))
    (tab-line-mode 1)))

;;; Custom faces
(bz/require custom)
(bz/face eww-form-checkbox :fg nil :bg nil)
(bz/face eww-form-submit custom-button :fg nil :bg nil :w bold
         :b (:color ,(bz/color gray3)))

;;; Favicons

(defun bz/eww-generate-title ()
  (let* ((title (plist-get eww-data :title)))

    (when (string= title "")
      (setq title (bz/eww-base-url)))

    (if (<= (length title) 15) title
      (concat (substring title 0 12) "..."))))

(defun bz/eww-base-url ()
  (replace-regexp-in-string
   "^\\(https?://\\)?\\([^/]*\\.\\)?\\([^/.]+\\.[^/.]+\\)\\(/.*\\)$" "\\3"
   (plist-get eww-data :url)))

(setq bz/eww-current-url nil)
(defun bz/eww-download-favicon (&rest args)
  (interactive)
  (when-let*
      ((*eww-mode* (eq major-mode 'eww-mode))
       (url (bz/eww-base-url))
       (*continue* (unless (equal url bz/eww-current-url)
                     (setq bz/eww-current-url url)))
       (favicon (format "https://www.google.com/s2/favicons?domain=%s" url))
       (hash (sxhash url))
       (dir "/tmp/eww-favicons/")
       (file (format "%s%s" dir hash))
       (filter
        `(lambda (p &rest args)
           (with-current-buffer ,(current-buffer)
             (when (and (not (process-live-p p)) (equal (bz/eww-base-url) ,url))
               (let* ((prop (list '(raise 0.3) (create-image ,file)))
                      (n (format "%s%s %s"
                                 (propertize "@" 'display '(height 0.1))
                                 (propertize " " 'display prop)
                                 (bz/eww-generate-title))))
                 (rename-buffer n)))))))

    (message "FAVICON!!!")

    (rename-buffer (format "@ %s" (bz/eww-generate-title)))

    (unless (f-directory-p dir) (make-directory dir t))

    (if (file-exists-p file)
        (funcall filter nil)
      (make-process
       :name "eww-favicon"
       :command (list "wget" favicon "-O" file)
       :filter filter))))

(setq bz/eww-favicon-timer (run-with-timer 1 1 'bz/eww-download-favicon))

;;(bz/advise :after 'eww 'bz/eww-download-favicon)
;;(bz/advise :after 'eww-browse-url 'bz/eww-download-favicon)
;;(bz/advise :after 'eww-reload 'bz/eww-download-favicon)
;;(bz/advise :after 'eww-restore-history 'bz/eww-download-favicon)

;;; Open org urls
;; (bz/hook org-open-at-point-functions bz/org-eww-at-point
;;   (let ((context (org-element-context)))
;;     (if (string= (plist-get (cadr context) :type) "https")
;;         (switch-to)
;;         (eww))))


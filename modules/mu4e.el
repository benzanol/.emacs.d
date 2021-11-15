;;; Installation
(setq qv/mu4e-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path qv/mu4e-path)
(require 'mu4e)

;; Set value of ~/.mbsyncrc
;; mbsync -a
;; mu init --maildir=~/Mail --my-address=adam.tillou@gmail.com --my-address=adamtillou23@students.amherstschools.org --my-address=adamtill@buffalo.edu
;; mu index

;;; Mail Settings
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-view-prefer-html t)

(setq mu4e-maildir "~/Mail")

(qv/face mu4e-title-face fixed-pitch :fg blue)

;;; Accounts
(setq qv/mu4e-context-gmail
      (make-mu4e-context
       :name "Gmail"
       :match-func
       (lambda (msg)
         (when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
       :vars '((user-mail-address . "adam.tillou@gmail.com")
               (user-full-name . "Adam Tillou")
               (mu4e-inbox-folder . "/Gmail/Inbox")
               (mu4e-starred-folder . "/Gmail/[Gmail]/Starred")
               (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
               (mu4e-sent-folder . "/Gmail/[Gmail]/Sent Mail")
               (mu4e-refile-folder . "/Gmail/[Gmail]/All Mail")
               (mu4e-trash-folder . "/Gmail/[Gmail]/Trash"))))

(setq qv/mu4e-context-school
      (make-mu4e-context
       :name "School"
       :match-func
       (lambda (msg)
         (when msg (string-prefix-p "/School" (mu4e-message-field msg :maildir))))
       :vars '((user-mail-address . "adam.tillou@gmail.com")
               (user-full-name . "Adam Tillou")
               (mu4e-inbox-folder . "/School/Inbox")
               (mu4e-starred-folder . "/School/[Gmail]/Starred")
               (mu4e-drafts-folder . "/School/[Gmail]/Drafts")
               (mu4e-sent-folder . "/School/[Gmail]/Sent Mail")
               (mu4e-refile-folder . "/School/[Gmail]/All Mail")
               (mu4e-trash-folder . "/School/[Gmail]/Trash"))))

(setq qv/mu4e-context-math
      (make-mu4e-context
       :name "Math"
       :match-func
       (lambda (msg)
         (when msg (string-prefix-p "/GMP" (mu4e-message-field msg :maildir))))
       :vars '((user-mail-address . "adamtill@buffalo.edu")
               (user-full-name . "Adam Tillou")
               (mu4e-inbox-folder . "/Math/Inbox")
               (mu4e-starred-folder . "/Math/[Gmail]/Starred")
               (mu4e-drafts-folder . "/Math/[Gmail]/Drafts")
               (mu4e-sent-folder . "/Math/[Gmail]/Sent Mail")
               (mu4e-refile-folder . "/Math/[Gmail]/All Mail")
               (mu4e-trash-folder . "/Math/[Gmail]/Trash"))))

(setq mu4e-contexts (--map (eval (intern (format "qv/mu4e-context-%s" it)))
                           '(gmail school math)))

(add-hook 'mu4e-context-changed-hook 'qv/mu4e-changed-context)
(defun qv/mu4e-changed-context ()
  (mu4e~headers-jump-to-maildir (eval mu4e-inbox-folder)))

;;; Sending Mail
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "imap.gmail.com")

;;; Contact Completion
(defun qv/mu4e-insert-contact (contact)
  (interactive
   (let ((contact-list '()))
     (maphash (lambda (key val) (push key contact-list)) mu4e~contacts)
     (list (completing-read "Contact: " (reverse contact-list)))))
  (insert (replace-regexp-in-string "^.*<\\(.*@.*\\..*\\)>$" "\\1" contact)))

;;; Hooks
(add-hook 'mu4e-view-mode-hook 'qv/mu4e-view-mode-setup)
(defun qv/mu4e-view-mode-setup ()
  (undo-tree-mode 0)
  (visual-fill-column-mode 1)
  (setq-local visual-fill-column-width 80)
  (setq-local truncate-lines nil)
  (setq word-wrap t))

(add-hook 'mu4e-headers-mode-hook 'qv/mu4e-header-mode-setup)
(setq qv/mu4e-first-header t)
(defun qv/mu4e-header-mode-setup ()
  (undo-tree-mode 0)
  (setq qv/mu4e-first-header t)
  (display-line-numbers-mode 0))

(defun qv/toggle-line-wrapping ()
  (interactive)
  (if truncate-lines
      (progn (setq-local truncate-lines nil)
             (visual-fill-column-mode 1))
    (setq-local truncate-lines t)
    (visual-fill-column-mode 0)))

;;; Jumping to Maildirs
(setq qv/mu4e-maildir-map (make-sparse-keymap))
(setq qv/mu4e-maildir-keys
      '(("i" . mu4e-inbox-folder)
        ("a" . mu4e-refile-folder)
        ("t" . mu4e-trash-folder)
        ("d" . mu4e-drafts-folder)
        ("s" . mu4e-starred-folder)))
(setcdr qv/mu4e-maildir-map nil)
(dolist (dir qv/mu4e-maildir-keys)
  (define-key qv/mu4e-maildir-map (kbd (car dir))
    (eval `(lambda () (interactive)
             (mu4e~headers-jump-to-maildir (eval ,(cdr dir)))
             (with-current-buffer "*mu4e-headers*"
               (mu4e-headers-view-message))))))

;;; Keybindings
(qv/keys * "C-x C-m" ((mu4e-context-switch nil "Gmail") (mu4e~headers-jump-to-maildir "Inbox")))


(setq qv/mu4e-general-map (make-sparse-keymap))
(qv/keys qv/mu4e-general-map
  "a" mu4e-view-mark-for-refile
  "d" mu4e-view-mark-for-trash
  "D" mu4e-view-mark-for-untrash
  "m" mu4e-view-mark-for-move
  "s" mu4e-view-mark-for-flag
  "S" mu4e-view-mark-for-unflag
  "c" mu4e-compose-new
  "u" mu4e-view-unmark
  "U" mu4e-view-unmark-all
  "f" ,qv/mu4e-maildir-map
  "F" mu4e~headers-jump-to-maildir
  "e" mu4e-context-switch
  "x" ((mu4e-mark-execute-all 'noconfirm)
       (run-with-timer 1 nil (lambda () (switch-to-buffer mu4e~headers-buffer-name)))))

(setcdr mu4e-view-mode-map nil)
(qv/keys mu4e-view-mode-map
  :parent qv/mu4e-general-map
  "RET" org-open-at-point
  "|" qv/toggle-line-wrapping
  "C-j" mu4e-view-headers-next
  "C-k" mu4e-view-headers-prev
  "o" mu4e-view-attachment-action
  "r" mu4e-view-refresh)

(setcdr mu4e-headers-mode-map nil)
(qv/keys mu4e-headers-mode-map
  "RET" mu4e-headers-view-message
  :parent qv/mu4e-general-map
  "j" ((rectangle-next-line 1) (beginning-of-line))
  "k" ((rectangle-previous-line 1) (beginning-of-line))
  "J" ((rectangle-next-line 4) (beginning-of-line))
  "K" ((rectangle-previous-line 4) (beginning-of-line))
  "C-j" mu4e-headers-next
  "C-k" mu4e-headers-prev
  "/" mu4e-headers-search)

(qv/keys mu4e-compose-mode-map
  "C-c c" qv/mu4e-insert-contact)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html#FOOT14
;;; Split View
(setq mu4e-split-view 'vertical)
(setq mu4e-headers-visible-lines 16)
(setq mu4e-headers-visible-columns 60)

;;; Custom Header Format
(qv/face mu4e-header-from :h 1.0 :w semibold)
(qv/face mu4e-header-starred :fg yellow :h 1.0 :w semibold)
(qv/face mu4e-header-subject :h 1.0 :w normal)
(qv/face mu4e-header-date fixed-pitch :fg gray1 :h 0.8 :w bold)
(qv/face mu4e-header-separator fixed-pitch :st gray2 :x t)
(qv/face mu4e-header-highlight-face :fg blue :u nil)

(defun mu4e~header-line-format () "")
(setq mu4e-headers-visible-columns 50)

(setq qv/mu4e-flag-icon-alist
      '((flagged . "")
        (attach . "")
        (seen . "")))

(defun mu4e~message-header-description (msg)
  (let* ((from (string-join (--map (propertize (cond ((not (consp it)) "???")
                                                     ((stringp (car it)) (car it))
                                                     ((stringp (cdr it)) (cdr it))
                                                     (t "???"))
                                               'help-echo (if (stringp (cdr it)) (cdr it) nil))
                                   (mu4e-message-field msg :from)) ","))
         (from-text (substring (concat from (make-string mu4e-headers-visible-columns ?\s))
                               0 (- mu4e-headers-visible-columns 20)))
         (date (mu4e-message-field msg :date))
         (formatted-date (format-time-string "%m/%d/%y %H:%M" date))
         (subject (mu4e-message-field msg :subject))
         (short-subject (if (<= (length subject) (- mu4e-headers-visible-columns 5)) subject
                          (format "%s..." (substring subject 0
                                                     (- mu4e-headers-visible-columns 8)))))
         (fake-newline (propertize " " 'display '(string "\n")))
         (flags (mu4e-message-field msg :flags))
         (icons (--map (if (memq (car it) flags) (cdr it) "")
                       qv/mu4e-flag-icon-alist))
         (separator-string (propertize (make-string (- mu4e-headers-visible-columns 3) ?\s)
                                       'face 'mu4e-header-separator)))
    (format "%s%s%s%s%s%s %s%s%s"
            (if (not qv/mu4e-first-header) ""
              (progn (setq qv/mu4e-first-header nil) (concat fake-newline "  ")))
            (propertize from-text 'face (if (memq 'flagged flags)
                                            'mu4e-header-starred 'mu4e-header-from))
            (propertize (if (memq 'attach flags) (alist-get 'attach qv/mu4e-flag-icon-alist) "")
                        'face 'fixed-pitch)
            (propertize " " 'display `(space . (:align-to ,(- mu4e-headers-visible-columns 15))))
            (propertize formatted-date 'face 'mu4e-header-date)
            fake-newline
            (propertize short-subject 'face 'mu4e-header-subject)
            fake-newline
            separator-string)))

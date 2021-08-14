(defun qv/run-in-background (command)
  (interactive "MSystem Command: ")
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun change-font-size (face increment)
  (set-face-attribute face nil :height
                      (+ (face-attribute face :height) increment)))

(global-set-key (kbd "C-+") (lambda () (interactive) (change-font-size 'default 16)))
(global-set-key (kbd "C-_") (lambda () (interactive) (change-font-size 'default -16)))

(defun qv/fuzzy-find-file (&optional dir)
  (interactive)
  (setq dir (expand-file-name (or dir "~")))
  (completing-read
   "Find File: "
   (mapcar (lambda (s) (replace-regexp-in-string (regexp-quote dir) "~" s))
           (split-string (shell-command-to-string (format "find '%s' -type f" dir))))))

(setq display-buffer-base-action '(display-buffer-same-window ()))

(setq qv/current-activity
      (list "default" (cons 'current-layout "default")
            (cons 'layouts
                  (list (cons "default"
                              (cons nil (current-window-configuration)))))
            (cons 'buffers (mapcar (lambda (buf) (cons nil buf)) (buffer-list)))
            (cons 'point (point))))
(setq qv/activities (list qv/current-activity))

(defun qv/add-activity (name &optional buffers)
  "Add a new activity with the name specified by the symbol NAME
By default, only the current buffer is added to the new activity,
but BUFFERS specifies which should be used instead."
  (interactive "MActivity Name: ")
  (if (assoc name qv/activities)
      (message "Activity already exists")
    (setq qv/activities
          (append qv/activities
                  (list (list name
                              (cons 'current-layout "default")
                              (cons 'layouts
                                    (list
                                     (cons
                                      "default"
                                      (cons nil (current-window-configuration)))))
                              (cons 'buffers ())
                              (cons 'point (point))))))
    (qv/switch-to-activity name)
    (while (not (equal " " (call-interactively 'qv/add-buffer-to-activity))))
    (switch-to-buffer (or (cdar (alist-get 'buffers qv/current-activity))
                          "*scratch*"))
    (delete-other-windows)
    (setcdr (cdar (alist-get 'layouts qv/current-activity))
            (current-window-configuration))))

(defun qv/rename-activity (activity name)
(interactive (list (assoc (completing-read "Rename Activity: " qv/activities) qv/activities)
(completing-read "New Name: " nil)))
(setcar activity name))

(defun qv/remove-activity (activity)
  "Delete ACTIVITY from the activities list"
  (interactive (list (assoc (completing-read
                             "Remove Activity: "
                             qv/activities)
                            qv/activities)))
  (let ((new-activity-list ()))
    (dolist (i qv/activities)
      (unless (eq i activity)
        (setq new-activity-list (append new-activity-list (list i)))))
    (setq qv/activities new-activity-list)))

(defvar qv/last-activity nil "The last activity used before the current one.")
(defun qv/switch-to-activity (name)
  "Set the current activity to the activity with the name NAME"
  (interactive
   (let ((other-activities nil))
     (dolist (i qv/activities)
       (unless (eq i qv/current-activity)
         (setq other-activities (append other-activities (list (car i))))))
     (list (completing-read
            (concat "Activity (" (car qv/current-activity) "): ")
            other-activities nil nil "^"))))

  (setq qv/last-activity qv/current-activity)
  (when qv/current-activity (qv/save-current-layout))
  (setq qv/current-activity (assoc name qv/activities))
  (set-window-configuration (cddr (assoc (alist-get 'current-layout qv/current-activity)
                                         (alist-get 'layouts qv/current-activity)))))

(defun qv/add-buffer-to-activity (buffer &optional activity)
  "Move a certain buffer to the current activity, then return that buffer."
  (interactive
   (list (completing-read
          "Select a buffer: "
          (let ((complete-list '(" "))
                (activity-buffers (mapcar 'cdr (alist-get 'buffers qv/current-activity))))
            (dolist (i (buffer-list))
              (unless (memq i activity-buffers)
                (setq complete-list (append complete-list (list (buffer-name i))))))
            complete-list))))
  (if (equal buffer " ")
      buffer
    (let ((buffer-object (if (bufferp buffer) buffer (get-buffer buffer))))
      (dolist (i qv/activities) (qv/remove-buffer-from-activity buffer-object i))
      (setcdr (assoc 'buffers (or activity qv/current-activity))
              (append (alist-get 'buffers (or activity qv/current-activity))
                      (list (cons nil (get-buffer buffer-object)))))
      buffer-object)))

(defun qv/remove-buffer-from-activity (buffer &optional activity)
  "Remove BUFFER from the list of buffers that are a part of ACTIVITY"
  (let ((new-buffer-list ()))
    (dolist (i (alist-get 'buffers (or activity qv/current-activity)))
      (unless (eq (cdr i) buffer) (setq new-buffer-list (append new-buffer-list (list i)))))
    (setcdr (assoc 'buffers (cdr (or activity qv/current-activity))) new-buffer-list)))

(defun qv/activity-switch-buffer ()
  "Switch to a certain buffer that is part of the current activity"
  (interactive)
  (let ((new-buffer-list ()) (current-in-activity nil))
    (dolist (i (alist-get 'buffers qv/current-activity))
      (when (buffer-name (cdr i))
        (if (eq (cdr i) (current-buffer))
            (setq current-in-activity i)
          (setq new-buffer-list (append new-buffer-list (list i))))))
    (let ((new-buffer
           (completing-read
            "Buffer in Activity: "
            (mapcar (lambda (element)
                      (buffer-name (cdr element)))
                    new-buffer-list)))
          (new-obj nil)
          (new-list nil))
      (dolist (i new-buffer-list)
        (if (eq (cdr i) (get-buffer new-buffer))
            (setq buffer-obj i)
          (setq new-list (append new-list (list i)))))
      (switch-to-buffer new-buffer)
      (setcdr (assoc 'buffers qv/current-activity)
              (append (list buffer-obj)
                      (if current-in-activity (list current-in-activity) nil)
                      new-list)))))

(defun qv/save-current-layout ()
  (setcdr (cdr (assoc (alist-get 'current-layout qv/current-activity)
                      (alist-get 'layouts qv/current-activity)))
          (current-window-configuration))
  (setcdr (assoc 'point qv/current-activity) (point)))

(defun qv/add-layout (name &optional layout)
  (interactive "MLayout Name: ")
  (let ((layout-list (assoc 'layouts qv/current-activity)))
    (if (assoc name layout-list)
        (message "Layout already exists")
      (qv/save-current-layout)
      (qv/activity-switch-buffer)
      (delete-other-windows)
      (setcdr layout-list
              (append (cdr layout-list)
                      (list (cons name (cons nil (current-window-configuration))))))
      (setcdr (assoc 'current-layout qv/current-activity) name))))

(defun qv/remove-layout (name)
  "Delete the layout with NAME from the layout list of the current activity"
  (interactive
   (list (completing-read "Remove Layout: " (alist-get 'layouts qv/current-activity))))
  (if (equal name (alist-get 'current-layout qv/current-activity))
      (message "You can't delete the current layout")
    (let ((new-layout-list ()))
      (dolist (i (alist-get 'layouts qv/current-activity))
        (unless (equal (car i) name)
          (setq new-layout-list (append new-layout-list (list i)))))
      (setcdr (assoc 'layouts qv/current-activity) new-layout-list))))

(defun qv/switch-to-layout (name)
  (interactive (list (completing-read "Select Layout: " (alist-get 'layouts qv/current-activity))))
  (qv/save-current-layout)
  (setcdr (assoc 'current-layout qv/current-activity) name)
  (set-window-configuration (cddr (assoc name (alist-get 'layouts qv/current-activity)))))

(defun qv/window-move-right ()
  (interactive)
  (qv/window-move nil t))

(defun qv/window-move-left ()
  (interactive)
  (qv/window-move nil nil))

(defun qv/window-move-down ()
  (interactive)
  (qv/window-move t t))

(defun qv/window-move-up ()
  (interactive)
  (qv/window-move t nil))

(defun qv/window-move (vertical forward)
  (when (one-window-p) (error "Only one window"))
  (let ((win (selected-window))
        (win-state (window-state-get (selected-window)))
        (tree (car (window--subtree (window-parent))))
        (split-direction
         (if vertical (if forward 'below 'above) (if forward 'right 'left))))
    ;; Create a split window to move the current window to
    (select-window
     ;; If the movement is in the same direction as the current list
     (if (eq (car tree) vertical)

         ;; If there are two windows, and they should be swapped
         (if (and (eq (window-child-count (window-parent)) 2)
                  (xor forward (eq (selected-window) (window-last-child (window-parent))))
                  (if (window-next-sibling)
                      (window-live-p (window-next-sibling))
                    (window-live-p (window-prev-sibling))))
             (let* ((other-win (if forward (window-next-sibling) (window-prev-sibling))))
               (split-window other-win nil split-direction))

           ;; If the window is at the end of the current stack, move it out if possible
           (if (or (and forward (eq (selected-window) (window-last-child (window-parent))))
                   (and (not forward) (eq (selected-window) (window-child (window-parent)))))
               (if (window-parent (window-parent))
                   (split-window (window-parent (window-parent)) nil split-direction)
                 (error "Nowhere to move"))

             ;; Move the window along the current list
             (let* ((next-win (if forward (window-next-sibling) (window-prev-sibling)))
                    (next-win (if (window-live-p next-win) next-win
                                (window-last-child next-win))))
               (split-window next-win nil
                             (if vertical 'right 'below)))))

       ;; Move the window out of the current stack
       (split-window (window-parent) nil split-direction)))

    (delete-window win)
    (window-state-put win-state (selected-window))))

(setq qv/table-vertical-line "│")

(defun qv/align-column (start-line end-line &optional end-string)
  ;; By default align from the cursor to the end of the buffer
  (interactive (list (if (eq (current-column) 0)
                         (1+ (count-lines (buffer-end -1) (point)))
                       (count-lines (buffer-end -1) (point)))
                     (count-lines (buffer-end -1) (buffer-end 1))))
  (let ((current-line start-line)
        (max-x 0))
    ;; Add a tab to the end of each line, and figure out which one ends up being longest
    (while (<= current-line end-line)
      (goto-line current-line)
      (end-of-line)
      (insert "\t")
      ;; If the current line is longer than the max line, update max-x
      (setq max-x (max max-x (car (posn-x-y (event-start nil)))))
      (setq current-line (1+ current-line)))
    ;; Add more tabs to the end of each line until they match the longest one, then add a pipe
    (setq current-line start-line)
    (while (<= current-line end-line)
      (goto-line current-line)
      (end-of-line)
      (while (> max-x (car (posn-x-y (event-start nil))))
        (insert "\t")
        (end-of-line))
      (when end-string
        (insert end-string))
      (setq current-line (1+ current-line)))))

(defun qv/align-table (start-line end-line)
  ;; By default the table is from the cursor to the end of the buffer
  (interactive (list (if (eq (current-column) 0)
                         (1+ (count-lines (buffer-end -1) (point)))
                       (count-lines (buffer-end -1) (point)))
                     (count-lines (buffer-end -1) (buffer-end 1))))
  ;; Add an extra line at the beginning as a template for the horizontal lines
  (goto-line start-line)
  (beginning-of-line)
  (insert (replace-regexp-in-string
           "[^|\n]+" " "
           (buffer-substring-no-properties
            (point)
            (save-excursion (end-of-line) (1+ (point))))))
  (setq end-line (1+ end-line))
  ;; Replace the pipes at the beginning and end with box characters
  (narrow-to-region (save-excursion (goto-line start-line) (beginning-of-line) (point))
                    (save-excursion (goto-line end-line) (end-of-line) (point)))
  (goto-char (buffer-end -1))
  (replace-regexp "^|\\(.*?\\) *|$"
                  (concat qv/table-vertical-line "\\1 | EOL"))
  (widen)
  (goto-line start-line)
  ;; Loop through the lines of the table
  (while (string-match-p "|"
                         (buffer-substring-no-properties
                          (save-excursion (beginning-of-line) (point))
                          (save-excursion (end-of-line) (point))))
    (let ((current-line start-line)
          (row-ends nil))
      (while (<= current-line end-line)
        (goto-line current-line)
        (beginning-of-line)
        (search-forward "|")
        ;; Copy everything after the current column into the list
        (setq row-ends
              (append
               (list
                (buffer-substring-no-properties
                 (save-excursion (forward-char) (point))
                 (save-excursion (end-of-line) (point))))
               row-ends))
        ;; Delete everything after the current column
        (delete-region
         (save-excursion (search-backward-regexp "[^ |]") (forward-char) (point))
         (save-excursion (end-of-line) (point)))
        ;; Go to the next line
        (setq current-line (1+ current-line)))
      ;; Insert tabs to align the next columns
      (qv/align-column start-line end-line (concat qv/table-vertical-line " "))
      ;; Insert the columns that were previously taken away
      (setq current-line start-line)
      (while (<= current-line end-line)
        (goto-line current-line)
        (end-of-line)
        (insert (nth (- end-line current-line) row-ends))
        (setq current-line (1+ current-line)))))
  ;; Remove the EOL strings
  (narrow-to-region (save-excursion (goto-line start-line) (beginning-of-line) (point))
                    (save-excursion (goto-line end-line) (end-of-line) (point)))
  (goto-char (buffer-end -1))
  (replace-regexp " EOL$" "")
  (widen)
  ;; Turn the template line into an actual horizontal line
  (goto-line start-line)
  (beginning-of-line)
  (narrow-to-region (point)
                    (save-excursion (end-of-line) (point)))
  (replace-regexp (concat "^" qv/table-vertical-line) "┌")
  (beginning-of-line)
  (replace-regexp (concat qv/table-vertical-line "$") "┐")
  (beginning-of-line)
  (replace-string qv/table-vertical-line "┬")
  (beginning-of-line)
  (replace-string " " "")
  (beginning-of-line)
  (replace-string "\t" "─")
  (widen)
  (let ((top-line (buffer-substring-no-properties
                   (save-excursion (beginning-of-line) (point))
                   (save-excursion (end-of-line) (point)))))
    (goto-line end-line)
    (end-of-line)
    (newline)
    (insert top-line))
  (beginning-of-line)
  (narrow-to-region (point)
                    (save-excursion (end-of-line) (point)))
  (replace-regexp "┌" "└")
  (beginning-of-line)
  (replace-regexp "┐" "┘")
  (beginning-of-line)
  (replace-string "┬" "┴")
  (widen))

(defun qv/prime-factorization (num &optional num2)
  (let ((factor 2))
    (while (and (< factor num) (not (eq (% num factor) 0)))
      (setq factor (1+ factor)))
    (if (eq factor num)
        (list num)
      (sort (append (qv/prime-factorization factor)
                    (qv/prime-factorization (/ num factor)))
            '<))))

(defun qv/nix-package-search-function (str arg pred)
  (if (< (length str) 4)
      nil
    (split-string
     (replace-regexp-in-string
      "^\\*" ""
      (replace-regexp-in-string
       "^[^*].*\n" ""
       (replace-regexp-in-string
        "\n+" "\n"
        (replace-regexp-in-string
         "^\\* nixpkgs\\.\\([^ ]*\\) ([^)]*)\n *\\(.*\\)" "*\\1: \\2"
         (replace-regexp-in-string
          "\\[\\(0;1m\\|0;2m\\|31;1m\\|0m\\)" ""
          (shell-command-to-string (format "nix search %s" str)))))))
     "\n")))

(defun qv/nix-package-install ()
  (interactive)
  (completing-read "Install Package: " 'qv/nix-package-search-function))

;;(qv/run-in-background
;; (format
;;  "export NIXPKGS_ALLOW_UNFREE=1 ; nix-env -iA nixos.%s"))

(defun qv/set-face-spec (face &rest attributes)
  (let ((gui-attrs (copy-tree attributes))
        (tty-attrs (copy-tree attributes)))
    (when (consp (plist-get attributes :foreground))
      (plist-put gui-attrs :foreground
                 (alist-get 'gui (plist-get gui-attrs :foreground)))
      (plist-put tty-attrs :foreground
                 (alist-get 'tty (plist-get tty-attrs :foreground))))
    (when (consp (plist-get attributes :background))
      (plist-put gui-attrs :background
                 (alist-get 'gui (plist-get gui-attrs :background)))
      (plist-put tty-attrs :background
                 (alist-get 'tty (plist-get tty-attrs :background))))
    (if (facep face)
        (face-spec-set name `((t . ,gui-attrs)))
      (defface face `((t . ,gui-attrs)) ""))))

(defun qv/face (name in fg bg &rest spec)
  (setq spec (or spec (list :placeholder :placeholder)))
  (cond ((or (symbolp in) (listp in)) (plist-put spec :inherit in))
        (in (plist-put spec :inherit nil)))
  (cond ((or (stringp fg) (listp fg)) (plist-put spec :foreground fg))
        (fg (plist-put spec :foreground nil)))
  (cond ((or (stringp bg) (listp bg)) (plist-put spec :background bg))
        (bg (plist-put spec :background nil)))
  (let ((new-spec '()))
    (dolist (i spec)
      (unless (eq i :placeholder)
        (setq new-spec (append new-spec (list i)))))
    (apply 'qv/set-face-spec name new-spec)))

(setq qv/fg-color     "#B0C0CC")
(setq qv/bg-color     '((gui . "#1B1F26") (tty . "#000000")))
(setq qv/bg2-color    '((gui . "#14161B") (tty . "#000000")))
(setq qv/bg3-color    '((gui . "#1F252E") (tty . "#000000")))
(setq qv/gray1-color  "#A4A8AC")
(setq qv/gray2-color  "#7C8084")
(setq qv/gray3-color  "#484B54")
(setq qv/black-color  "#0E1216")
(setq qv/red-color    "#D75F5F")
(setq qv/yellow-color "#FFD75F")
(setq qv/orange-color "#FFA500")
(setq qv/green-color  "#40E040")
(setq qv/cyan-color   "#5FFFD7")
(setq qv/blue-color   "#5FAFD7")
(setq qv/purple-color "#AF87D7")

(setq qv/variable-pitch-mode-line-spacing 0.25)

(defun qv-startup/basic-faces ()
  (qv/face 'default nil qv/fg-color qv/bg-color
           :family "IBM Plex Sans Condensed Medium"
           ;;:family "Ropa Sans"
           ;;:family "Magra"
           :weight 'normal
           :height 72)
  (qv/face 'variable-pitch nil nil nil
           :family "Droid Serif"
           :weight 'normal
           :height 1.0)
  (qv/face 'fixed-pitch nil nil nil
           :family "Iosevka"
           :weight 'normal
           :height 1.0)

  (qv/face 'region nil nil qv/gray3-color)
  (qv/face 'line-number 'fixed-pitch qv/gray2-color nil :height 0.9)
  (qv/face 'highlight nil "black" "#33AABB")
  (qv/face 'shadow nil qv/gray2-color nil)
  (qv/face 'link nil qv/blue-color nil))

(defun qv-startup/custom-mode-faces ()
  (qv/face 'custom-button nil nil qv/bg2-color
           :box `(:color ,qv/gray3-color)
           :weight 'semibold)
  (qv/face 'widget-field nil 0 qv/bg2-color
           :weight 'regular))

(add-hook 'custom-mode-hook 'qv-startup/custom-mode-faces)
(advice-add 'custom-buffer-create :after 'evil-normal-state)

(defun qv-startup/layout-faces ()
  (qv/face 'mode-line nil qv/fg-color qv/bg2-color
           :box `(:color "RoyalBlue3"))
  (qv/face 'mode-line-inactive nil qv/gray2-color qv/bg2-color
           :box `(:color ,(alist-get 'gui qv/bg2-color)))
  (qv/face 'fringe 'mode-line-inactive nil nil)
  (qv/face 'vertical-border 'mode-line-inactive qv/bg2-color nil))

(defun qv-startup/doom-modeline-faces ()
  (qv/face 'doom-modeline-buffer-modified nil 0 nil
           :weight 'bold :slant 'italic)
  (qv/face 'doom-modeline-buffer-file nil 0 nil
           :weight 'semibold))

(defun qv-startup/font-lock-faces ()
  (qv/face 'font-lock-comment-face nil qv/gray2-color nil
           :weight 'bold
           :slant 'italic)
  (qv/face 'font-lock-string-face nil qv/green-color nil
           :slant 'italic)
  (qv/face 'font-lock-type-face nil qv/yellow-color nil)
  (qv/face 'font-lock-keyword-face nil qv/yellow-color nil)
  (qv/face 'font-lock-function-name-face nil qv/red-color nil)
  (qv/face 'font-lock-variable-name-face nil qv/red-color nil)
  (qv/face 'font-lock-constant-face nil qv/blue-color nil)
  (qv/face 'font-lock-builtin-face nil qv/blue-color nil))

(defun qv-startup/whitespace-faces ()
  (qv/face 'whitespace-newline 'fixed-pitch qv/gray2-color nil
           :height 0.8)
  (qv/face 'whitespace-tab 'fixed-pitch qv/gray2-color nil
           :height 0.9)
  (qv/face 'whitespace-indentation 'whitespace-tab nil nil)
  (qv/face 'whitespace-line nil nil nil)
  (qv/face 'whitespace-empty nil nil nil))

(defun qv-startup/which-key-faces ()
  (qv/face 'which-key-command-description-face nil qv/fg-color nil)
  (qv/face 'which-key-key-face nil qv/blue-color nil)
  (qv/face 'which-key-group-description-face nil qv/yellow-color nil)
  (qv/face 'which-key-separator-face nil qv/gray2-color nil))

;; Minibuffer Faces
(defun qv-startup/minibuffer-faces ()
  (qv/face 'minibuffer-prompt nil qv/blue-color nil
           :weight 'bold)
  (qv/face 'ivy-current-match nil "#EECC44" 0
           :weight 'semibold
           :underline "#EECC44")
  (qv/face 'ivy-minibuffer-match-face-1 nil nil nil)
  (qv/face 'ivy-minibuffer-match-face-2 nil "#77CC00" nil)
  (qv/face 'ivy-minibuffer-match-face-3 nil "#DD88FF" nil)
  (qv/face 'ivy-minibuffer-match-face-4 nil "#55CCEE" nil)
  (qv/face 'ivy-org nil nil nil)
  (qv/face 'ivy-posframe nil nil qv/bg3-color :height 2.0))

(defun qv-startup/rainbow-faces ()
  (qv/face 'rainbow-delimiters-depth-1-face nil "tomato" nil)
  (qv/face 'rainbow-delimiters-depth-2-face nil "orange" nil)
  (qv/face 'rainbow-delimiters-depth-3-face nil "yellow" nil)
  (qv/face 'rainbow-delimiters-depth-4-face nil "green1" nil)
  (qv/face 'rainbow-delimiters-depth-5-face nil "cyan" nil)
  (qv/face 'rainbow-delimiters-depth-6-face nil "royalblue2" nil)
  (qv/face 'rainbow-delimiters-depth-7-face nil "mediumorchid2" nil))

;; Org Outline Faces
(defun qv-startup/org-outline-faces ()
  (qv/face 'org-document-title nil qv/fg-color nil
           :family qv/org-header-family
           :weight 'bold
           :underline nil
           :height 2.5)
  (qv/face 'org-document-info nil qv/fg-color nil
           :family qv/org-header-family
           :weight 'normal
           :underline nil
           :height 1.3)
  (qv/face 'org-level-1 nil qv/blue-color nil
           :family qv/org-header-family
           :weight 'bold
           :height 1.5)
  (qv/face 'org-level-2 nil qv/yellow-color nil
           :family qv/org-header-family
           :weight 'bold
           :height 1.35)
  (qv/face 'org-level-3 nil qv/red-color nil
           :family qv/org-header-family
           :weight 'bold
           :height 1.25)
  (qv/face 'org-level-4 nil qv/purple-color nil
           :family qv/org-header-family
           :weight 'bold
           :height 1.2))

(defun qv-startup/org-special-faces ()
  ;; Org special faces
  (qv/face 'org-special-keyword 'fixed-pitch qv/gray2-color nil
           :height 0.8)
  (qv/face 'org-meta-line 'org-special-keyword nil nil)
  (qv/face 'org-document-info-keyword 'org-special-keyword nil nil)
  (qv/face 'org-verbatim 'fixed-pitch qv/gray2-color nil)
  (qv/face 'org-code 'org-verbatim nil qv/bg2-color)
  (qv/face 'org-block 'fixed-pitch nil qv/bg2-color :extend 't)
  (qv/face 'org-block-begin-line 'org-block qv/gray3-color nil)
  (qv/face 'org-block-end-line 'org-block qv/gray3-color nil)
  (qv/face 'org-checkbox 'fixed-pitch nil nil)
  (qv/face 'org-ellipsis nil nil nil
           :underline nil))

;; Company Completion Faces
(defun qv-startup/company-faces ()
  (qv/face 'company-tooltip nil nil "#383B48")
  (qv/face 'company-tooltip-common nil nil nil)
  (qv/face 'company-tooltip-selection 'ivy-current-match nil nil)
  (qv/face 'company-preview nil qv/gray2-color nil)
  (qv/face 'company-preview-common 'company-preview nil nil)
  (qv/face 'company-preview-search 'company-preview nil nil)
  (qv/face 'company-scrollbar-fg nil nil qv/gray2-color)
  (qv/face 'company-scrollbar-bg 'company-tooltip nil nil))

;; Terminal Faces
(defun qv-startup/terminal-faces ()
  (qv/face 'term-color-blue nil qv/blue-color nil))

(qv-startup/basic-faces)
(qv-startup/layout-faces)
(qv-startup/font-lock-faces)
(qv-startup/whitespace-faces)

(setq display-time-format "%h %d  %H:%M |")

(display-time-mode 1)

(setq custom-theme-directory "~/.emacs.d/themes")

(qv/run-in-background (concat "xmodmap " (expand-file-name "~/.Xmodmap")))

(setq-default inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(setq-default truncate-lines t)

(setq-default delete-by-moving-to-trash t)

(setq-default auto-save-default t)
(setq-default make-backup-files nil)

(defun make-auto-save-file-name ()
  (concat (expand-file-name "~/.emacs.d/auto-saves/")
          (replace-regexp-in-string "/" "!" buffer-file-name)))

(setq create-lockfiles nil)

(setq-default tab-width 4)

(setq-default indent-tabs-mode nil)

(global-display-line-numbers-mode t)

(global-set-key (kbd "C-x C-l") 'display-line-numbers-mode)

(setq window-combination-resize t)

(setq enable-recursive-minibuffers nil)

(winner-mode)

(setq-default scroll-margin 100000)
(setq-default maximum-scroll-margin 1.0)

(setq-default x-stretch-cursor t)

(set-fringe-mode 10)

(recentf-mode 1)

(global-set-key
 (kbd "C-x C-f")
 (lambda (arg) (interactive "P")
   (if arg
       (counsel-recentf)
     (counsel-find-file))))

(defun qv/term-hook ()
  (interactive)

  (qv-startup/terminal-faces)

  (buffer-face-set 'fixed-pitch)

  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0.0)

  (term-line-mode)
  (evil-define-key '(normal visual) 'local "i"
    (lambda () (interactive)
      (evil-emacs-state) (term-char-mode)))
  (evil-define-key '(normal visual) 'local "a"
    (lambda () (interactive)
      (evil-emacs-state) (term-char-mode) (term-send-end)))
  (evil-define-key '(normal visual) 'local "I"
    (lambda () (interactive)
      (evil-emacs-state) (term-char-mode) (term-send-home)))

  ;; For some reason, "A" and only "A" results in the command being executed
  ;; after entering char mode
  (evil-define-key '(normal visual) 'local "A" (lambda () (interactive)))

  (term-char-mode)
  (evil-emacs-state)
  (local-set-key (kbd "s-q") (lambda () (interactive)
                               (evil-normal-state) (term-line-mode)))
  (local-set-key (kbd "ESC") (lambda () (interactive)
                               (evil-normal-state) (term-line-mode))))

(defun terminal ()
  (interactive)
  (term "zsh")
  (qv/term-hook))

(use-package all-the-icons)
(use-package all-the-icons-dired)

(setq all-the-icons-scale-factor 1.4)
(setq all-the-icons-fileicon-scale-factor 1.0)

(qv/face 'all-the-icons-dired-dir-face 'dired-directory nil nil)

(use-package dired-subtree)

(setq dired-subtree-line-prefix "    ")

(define-key dired-mode-map (kbd "l") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "h") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "SPC") 'dired-subtree-toggle)

(defvar qv/dired-git-mode nil "Non-nil if dired git mode is enabled")
(defun qv/dired-git-mode (&optional arg)
  "If arg is omitted or nil, toggle dired git mode.
If arg is zero or negative, disable dired git mode.
Otherwise, enable dired git mode"
  (interactive)
  (setq qv/dired-git-mode
        (if arg (if (and (numberp arg) (<= 0 arg)) nil t)
          (not qv/dired-git-mode)))
  (if qv/dired-git-mode
      (qv/dired-git-insert-icons)
    (remove-overlays nil nil 'qv/git-icon t)))

(defvar qv/dired-git-status-alist '(("  " . committed)
                                    (" M" . modified)
                                    ("MM" . modified)
                                    ("M " . added)
                                    ("??" . untracked)
                                    ("!!" . ignored))
  "The character output by the git status command for each git status")

(defvar qv/dired-git-icon-alist '((committed . "✓")
                                  (modified . "✶")
                                  (added . "✚")
                                  (untracked . "✗")
                                  (ignored . "!"))
  "The icon displayed before a file for each git status")

(defface dired-git-committed-face '((t :foreground "green" :family "Sans"))
  "Face for the dired git committed icon")
(defface dired-git-modified-face '((t :foreground "brown1" :family "Sans"))
  "Face for the dired git modified icon")
(defface dired-git-added-face '((t :foreground "MediumPurple1" :family "Sans"))
  "Face for the dired git added icon")
(defface dired-git-untracked-face '((t :foreground "brown1" :family "Sans"))
  "Face for the dired git untracked icon")
(defface dired-git-ignored-face '((t :foreground "orange" :weight bold))
  "Face for the dired git ignored icon")

(defun qv/dired-git-insert-icons (&optional beg end)
  (remove-overlays nil nil 'qv/git-icon t)
  (let ((git-dir-alist '()) (git-alist '()))
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (< (point) (if end (min (point-max) end) (point-max)))
        (when (dired-move-to-filename nil)
          (when-let*
              ((filename (dired-get-filename nil 'noerror))
               (parent (expand-file-name (format "%s/.." filename)))
               (git-output (shell-command-to-string
                            (format "cd '%s' ; git rev-parse --show-toplevel" parent)))
               ;; Figure out the git directory for the current file
               (git-dir
                (progn
                  (unless (assoc parent git-dir-alist)
                    (setq
                     git-dir-alist
                     (append
                      git-dir-alist
                      (list
                       (cons
                        parent
                        (unless (string=
                                 (car (split-string git-output " "))
                                 "fatal:")
                          (car (split-string git-output "\n"))))))))
                  (cdr (assoc parent git-dir-alist)))))
            (unless (assoc git-dir git-alist)
              (setq git-alist
                    (append
                     git-alist
                     (list (cons
                            git-dir
                            (mapcar
                             (lambda (s)
                               (if (< (length s) 3) nil
                                 (cons (format "%s/%s" git-dir
                                               (substring s 3 (length s)))
                                       (substring s 0 2))))
                             (split-string
                              (shell-command-to-string
                               (format "cd '%s' ; git status -s" git-dir))
                              "\n")))))))
            (when-let*
                ((ov (make-overlay (- (point) 2) (- (point) 1)))
                 (letter (substring
                          (or (cdr (assoc filename (cdr (assoc git-dir git-alist)))) "  ")
                          0 2))
                 (status (cdr (assoc letter qv/dired-git-status-alist)))
                 (icon-name (alist-get status qv/dired-git-icon-alist))
                 (icon (propertize
                        icon-name 'face (intern (format "dired-git-%s-face" status)))))
              (setq myvar icon)
              (overlay-put ov 'qv/git-icon t)
              (overlay-put ov 'after-string icon))))
        (next-line)))))

(defvar qv/dired-showing-hidden t)
(defun qv/dired-show-hidden (&optional arg)
  "If arg is nil or unspecified, toggle showing hidden.
    If arg is zero or negative, hide hidden files.
    Otherwise, show hidden files"
  (interactive)
  (setq qv/dired-showing-hidden
        (if (numberp arg)
            (if (> arg 0) t nil)
          (not qv/dired-showing-hidden)))
  ;; Yes, there should be two spaces between the argument groups,
  ;; because that noticably increases the speed for some reason
  (if qv/dired-showing-hidden
      (setq dired-listing-switches "-lvA  --group-directories-first")
    (setq dired-listing-switches "-lv  --group-directories-first"))
  (dired-revert)
  (qv/dired-reload))
(define-key dired-mode-map (kbd ".") 'qv/dired-show-hidden)

(defun qv/dired-open ()
  (interactive)
  (let ((filename (dired-get-filename)))
    (if (file-directory-p filename)
        (progn (kill-buffer (current-buffer)) (dired filename))
      (other-window 1)
      (find-file filename))))

(defun qv/dired-up ()
  (interactive)
  (let ((up-dir (expand-file-name (format "%s/.." dired-directory))))
    (kill-buffer (current-buffer))
    (dired up-dir)))

(define-key dired-mode-map (kbd "RET") 'qv/dired-open)
(define-key dired-mode-map (kbd "L") 'qv/dired-open)
(define-key dired-mode-map (kbd "H") 'qv/dired-up)

(define-key dired-mode-map (kbd "j") 'dired-next-line)
(define-key dired-mode-map (kbd "k") 'dired-previous-line)
(define-key dired-mode-map (kbd "J") 'qv/down-four)
(define-key dired-mode-map (kbd "K") 'qv/up-four)

(define-key dired-mode-map (kbd "d") 'dired-find-file)
(define-key dired-mode-map (kbd "D")
  (lambda () (interactive) (dired-delete-file (dired-get-filename) t t)))

(define-key dired-mode-map (kbd "/") 'isearch-forward)
(define-key dired-mode-map (kbd "?") 'isearch-backward)
(define-key dired-mode-map (kbd "n") 'isearch-repeat-forward)
(define-key dired-mode-map (kbd "N") 'isearch-repeat-backward)

(define-key dired-mode-map (kbd "=")
  (lambda () (interactive) (let ((window-size-fixed nil)) (window-resize nil 2 t))))
(define-key dired-mode-map (kbd "-")
  (lambda () (interactive) (let ((window-size-fixed nil)) (window-resize nil -2 t))))

(defun qv/dired-toggle-mark (&optional arg)
  "Toggle whether the current file is marked.
      If arg is negative or zero, disable the mark.
      If arg is positive, enable the mark."
  (interactive)
  (dired-move-to-filename)
  (let ((mark
         (if arg (if (and (numberp arg) (<= arg 0)) nil t)
           (if (eq (plist-get (text-properties-at (point)) 'face) 'dired-marked) nil t))))
    (if mark-active
        (dolist (i (number-sequence (line-number-at-pos (min (point) (mark)))
                                    (line-number-at-pos (max (point) (mark)))))
          (deactivate-mark) (goto-line i) (qv/dired-toggle-mark (if mark 1 0)))
      (if mark (dired-mark 1) (dired-unmark 1))
      (previous-line) (dired-move-to-filename))))
(define-key dired-mode-map (kbd "m")
  (lambda () (interactive) (qv/dired-toggle-mark 1)))
(define-key dired-mode-map (kbd "u")
  (lambda () (interactive) (qv/dired-toggle-mark 0)))
(define-key dired-mode-map (kbd "M")
  (lambda () (interactive) (dired-unmark-all-marks) (dired-toggle-marks)))
(define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)
(define-key dired-mode-map (kbd "t") 'qv/dired-toggle-mark)
(define-key dired-mode-map (kbd "T") 'dired-toggle-marks)

(define-key dired-mode-map (kbd "r")
  (lambda () (interactive) (revert-buffer) (qv/dired-reload)))

(define-key dired-mode-map (kbd "V") 'set-mark-command)

(define-key dired-mode-map (kbd "a") nil)
(define-key dired-mode-map (kbd "a f") 'dired-create-empty-file)
(define-key dired-mode-map (kbd "a d") 'dired-create-directory)

(defun qv/dired-startup ()
  (display-line-numbers-mode 0)
  (setq-local tab-width 1)

  (setq dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-mode)

  (setq dired-listing-switches "-lvA  --group-directories-first")

  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0.0)
  (setq-local scroll-step 2)

  (setq-local window-size-fixed 'width)

  (setq-local line-spacing 0.1)

  (qv/face 'dired-directory nil qv/blue-color nil)
  (qv/face 'dired-header 'dired-directory nil nil
           :height 1.1 :weight 'bold :underline t)
  (qv/face 'dired-marked nil qv/purple-color nil)
  (qv/face 'dired-mark 'dired-marked nil nil)
  (dotimes (i 5)
    (qv/face (intern (format "dired-subtree-depth-%s-face" (1+ i))) nil nil nil))

  (rename-buffer
   (format "Dired: %s"
           (replace-regexp-in-string
            (concat "^" (regexp-quote (expand-file-name "~"))) "~"
            dired-directory))))

(add-hook 'dired-mode-hook 'qv/dired-startup)

(advice-add 'dired :after (lambda (&optional arg pred) (evil-emacs-state)))

(defun qv/dired-reload (&optional arg pred)
  (interactive)
  (when qv/dired-git-mode (qv/dired-git-insert-icons))
  (all-the-icons-dired--refresh))

(advice-add 'dired :after 'qv/dired-reload)
(advice-add 'dired-add-entry :after 'qv/dired-reload)
(add-hook 'dired-subtree-after-remove-hook 'qv/dired-reload)
(add-hook 'dired-subtree-after-insert-hook 'qv/dired-reload)

(defun qv/elisp-syntax ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("setq \\([^ ]+\\)" (1 font-lock-variable-name-face))
     ("(\\(lambda\\) (" (1 font-lock-variable-name-face))
     ("(\\(interactive\\))" (1 font-lock-constant-face)))))

(add-hook 'emacs-lisp-mode-hook 'qv/elisp-syntax)

(global-whitespace-mode 1)

(setq whitespace-space-regexp "[z-a]")
(setq whitespace-hspace-regexp "[z-a]")

(setq whitespace-display-mappings
      '((newline-mark 10 [?↲ 10])
        (tab-mark 9 [?» ?  ?  ? ])))

(global-whitespace-mode 0)

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s h") 'global-whitespace-mode)



(add-hook 'prog-mode-hook 'qv/prog-mode-startup)

(defun qv/prog-mode-startup ()
  (buffer-face-set nil 'fixed-pitch)

  (hs-minor-mode)
(hs-hide-all)

  (evil-define-key 'normal 'local (kbd "SPC") 'hs-toggle-hiding)
  (evil-define-key 'normal 'local (kbd "gzs") 'hs-show-all)
  (evil-define-key 'normal 'local (kbd "gzh") 'hs-hide-all)
  (evil-define-key 'normal 'local (kbd "gzl") 'hs-hide-level)
  (evil-define-key 'normal 'local (kbd "gzb") 'hs-hide-block)
  )

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  '(("lambda" . "λ")))
            (prettify-symbols-mode)))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable))

(defun qv/helpful-code-overlay ()
  (save-excursion
    (beginning-of-buffer)

    (ignore-errors
      (search-forward-regexp "^Signature$")
      (next-line) (beginning-of-line)
      (overlay-put (make-overlay (point) (line-end-position))
                   'face 'fixed-pitch))

    (ignore-errors
      (search-forward-regexp "^References$")
      (next-line 2) (beginning-of-line)
      (let ((start (point)))
        (search-forward-regexp "^Find all references")
        (previous-line)
        (overlay-put (make-overlay start (point))
                     'face 'fixed-pitch)))

    (ignore-errors
      (search-forward-regexp "^Source Code$")
      (next-line) (beginning-of-line)
      (let ((start (point)))
        (or (prog1 (search-forward-regexp "^Symbol Properties$" nil t) (previous-line))
            (end-of-buffer))
        (previous-line)
        (overlay-put (make-overlay start (point))
                     'face 'fixed-pitch)))

    (ignore-errors
      (next-line 2) (beginning-of-line)
      (overlay-put (make-overlay (1+ (point)) (point-max))
                   'face 'fixed-pitch))))

(advice-add 'helpful-update :after 'qv/helpful-code-overlay)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (qv-startup/which-key-faces))

(use-package neotree)

(setq neo-theme 'icons)

(use-package ov)

(use-package ivy
  :custom
  (ivy-mode +1)
  (ivy-height 19))

(setq ivy-minibuffer-map
      `(keymap
        (remap
         . (keymap
            (next-line . ivy-next-line)
            (previous-line . ivy-previous-line)
            (qv/down-four . (lambda () (interactive)
                              (dotimes (i 4) (ivy-next-line))))
            (qv/up-four . (lambda () (interactive)
                            (dotimes (i 4) (ivy-previous-line))))
            (beginning-of-buffer . ivy-beginning-of-buffer)
            (end-of-buffer . ivy-end-of-buffer)))
        (10 . next-history-element)
        (11 . previous-history-element)
        (13 . ivy-done)
        (7 . minibuffer-keyboard-quit)
        (C-return . ivy-immediate-done)
        (tab . ivy-partial-or-done)
        (S-return . newline)))

(defun qv/minibuffer-startup ()
  (ignore-errors (qv-startup/minibuffer-faces)))

(add-hook 'minibuffer-setup-hook 'qv/minibuffer-startup)

(use-package ivy-posframe
  :after ivy
  :config
  (ivy-posframe-mode 1)
  :custom
  (ivy-posframe-width 80)
  (ivy-posframe-height (1+ ivy-height))
  (ivy-posframe-style 'frame-center))

(defvar qv/detail-function-alist
  '((helpful-callable . helpful--signature)
    (helpful--variable . helpful--signature)
    (execute-extended-command . helpful--signature))
  "List of functions to get the information line for various minibuffer functions")

(setq qv/detail-function-alist
      `((helpful-callable . helpful--signature)
        (helpful-variable . (lambda (v) (documentation-property
                                         v 'variable-documentation t)))
        (execute-extended-command . helpful--signature)))

(defvar qv/ivy-detail-max-length 100
  "The maximum number of characters to show in the description.")

(defface ivy-base '((t :height 1.2))
  "Base face for displaying minibuffer completions in ivy.")

(defface ivy-details '((t :inherit '(fixed-pitch) :height 0.7))
  "Face for displaying the details line.")

(defface ivy-current-details '((t :inherit (ivy-current-match ivy-details-face)))
  "Face for displaying the details of the current entry.")

(defface ivy-details-indent '((t :inherit ivy-details :height 0.8))
  "The indentation before each item")

(defun qv/ivy-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (let ((details-function (ivy-alist-setting qv/detail-function-alist)))
    (if details-function
        (ivy--format-function-generic
         (eval
          `(lambda (str)
             (let ((detail (qv/ivy-detail-string str ',details-function)))
               (concat
                (propertize "  " 'face 'ivy-details-indent)
                (propertize str 'face 'ivy-current-match)
                (propertize "\n" 'face 'ivy-details-indent)
                (propertize "  " 'face 'ivy-details-indent)
                (propertize detail 'face 'ivy-current-details)
                (propertize "\n" 'line-spacing 0.1 'face 'ivy-details-indent)))))
         (eval
          `(lambda (str)
             (let ((detail (qv/ivy-detail-string str ',details-function)))
               (concat
                (propertize "  " 'face 'ivy-details-indent)
                str
                (propertize "\n" 'face 'ivy-details-indent)
                (propertize "  " 'face 'ivy-details-indent)
                (propertize detail 'face 'ivy-details)
                (propertize "\n" 'line-spacing 0.2 'face 'ivy-details-indent)))))
         cands "")
      (ivy-format-function-default cands))))

(defun qv/ivy-detail-string (candidate function)
  (let ((detail-string (funcall function (intern candidate))))
    (if (not detail-string)
        ""
      (setq detail-string (replace-regexp-in-string "\n" " " detail-string))
      (when (> (length detail-string) qv/ivy-detail-max-length)
        (setq detail-string
              (concat
               (substring
                detail-string
                0 (- qv/ivy-detail-max-length 3))
               "...")))
      (unless (stringp detail-string) (message detail-string))
      detail-string)))

(defun qv/ivy-variable-description (func)
  (let ((doc (documentation func t)))

    (setcdr (assoc t ivy-format-functions-alist) 'qv/ivy-format-function)

    (global-set-key (kbd "<f3>")
                    (lambda () (interactive)
                      (message "%s" (ivy-alist-setting qv/detail-function-alist))))

(use-package ivy-prescient
  :custom
  (ivy-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package counsel
  :custom
  (counsel-mode nil)
  (counsel-linux-app-format-function 'counsel-linux-app-format-function-name-only))

(setq counsel-find-file-map
      `(keymap
        (? . counsel-find-file-undo)
        (? . counsel-up-directory)
        (? . counsel-down-directory)
        (10 . ivy-next-line)
        ( . ivy-previous-line)
        (tab . counsel-down-directory)
        (C-backspace . counsel-up-directory)
        (backspace . (lambda () (interactive)
                       (if (string= (minibuffer-contents-no-properties) "")
                           (counsel-up-directory) (backward-delete-char 1))))))

(use-package embark)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "C-a") 'embark-act)
            (local-set-key (kbd "C-j") 'next-history-element)
            (local-set-key (kbd "C-k") 'previous-history-element)))

(setq embark-prompter 'embark-completing-read-prompter)

(use-package orderless
  :custom (completion-styles '(basic orderless)))

(use-package undo-tree
  :init (setq undo-tree-map '(keymap (24 . (keymap (117 . undo-tree-visualize)))))
  :config (global-undo-tree-mode 1)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history"))))

(use-package evil-multiedit)

(use-package company)
(add-hook 'prog-mode-hook 'company-mode)

(add-hook 'evil-insert-state-exit-hook 'company-abort)

(setq company-show-numbers t)
(setq company-require-match nil)
(setq company-tooltip-limit 20)
(setq company-tooltip-margin 1)
(setq company-idle-delay 0.0)
(setq company-tooltip-minimum-width 30)
(setq company-tooltip-maximum-width 50)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-width-grow-only t)

(add-hook 'prog-mode-hook 'qv-startup/company-faces)

;; (use-package company-tabnine)
;; 
;; (setq company-backends (list #'company-tabnine))
;; (setq company-tabnine-wait 0.1)

(add-hook 'prog-mode-hook
        (lambda ()
            (local-set-key (kbd "<tab>") 'company-complete)))

(setq company-active-map (make-sparse-keymap))
(define-key company-active-map (kbd "C-g") 'company-abort)
(define-key company-active-map (kbd "s-q") 'company-abort)

(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-M-j") 'company-select-last)
(define-key company-active-map (kbd "C-M-k") 'company-select-first)
(define-key company-active-map (kbd "C-S-j")
  (lambda () (interactive) (dotimes (i 4) (company-select-next))))
(define-key company-active-map (kbd "C-S-k")
  (lambda () (interactive) (dotimes (i 4) (company-select-previous))))
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map (kbd "M-J")
  (lambda () (interactive) (dotimes (i 4) (company-select-next))))
(define-key company-active-map (kbd "M-K")
  (lambda () (interactive) (dotimes (i 4) (company-select-previous))))

(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "<backtab>") 'company-complete-common)

(dotimes (i 10)
  (define-key company-active-map (kbd (format "M-%s" (% (1+ i) 10)))
    (eval `(lambda () (interactive) (company--complete-nth ,i)))))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(setq doom-modeline-buffer-state-icon nil)
(setq doom-modeline-icon nil)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 7)
  :config (qv-startup/rainbow-faces))

(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-auto-indent t)
  (setq-default evil-move-beyond-eol t)
  (setq-default evil-move-cursor-back nil))

(evil-define-motion qv/down-line (count) (next-line (or count 1)) (recenter))
(evil-define-motion qv/up-line (count) (previous-line (or count 1)) (recenter))

(evil-define-key '(normal visual operator) 'global "j" 'qv/down-line)
(evil-define-key '(normal visual operator) 'global "k" 'qv/up-line)

(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)

(evil-define-motion qv/forward-four (count) (forward-char (* 4 (or count 1))))
(evil-define-motion qv/backward-four (count) (backward-char (* 4 (or count 1))))
(evil-define-motion qv/down-four (count) (next-line (* 4 (or count 1))) (recenter))
(evil-define-motion qv/up-four (count) (previous-line (* 4 (or count 1))) (recenter))

(evil-define-key '(normal visual operator) 'global "L" 'qv/forward-four)
(evil-define-key '(normal visual operator) 'global "H" 'qv/backward-four)
(evil-define-key '(normal visual operator) 'global "J" 'qv/down-four)
(evil-define-key '(normal visual operator) 'global "K" 'qv/up-four)

(global-set-key (kbd "M-L") 'qv/forward-four)
(global-set-key (kbd "M-H") 'qv/backward-four)
(global-set-key (kbd "M-J") 'qv/down-four)
(global-set-key (kbd "M-K") 'qv/up-four)

(evil-define-key '(normal visual operator) 'global "gl"
  (lambda () (interactive)
    (if truncate-lines (end-of-line) (end-of-visual-line))))
(evil-define-key '(normal visual operator) 'global "gh"
  (lambda () (interactive)
    (if truncate-lines (beginning-of-line) (beginning-of-visual-line))))
(evil-define-key '(normal visual operator) 'global "gL" 'end-of-line)
(evil-define-key '(normal visual operator) 'global "gH" 'beginning-of-line)

(evil-define-key '(normal visual operator) 'global "gj" 'end-of-buffer)
(evil-define-key '(normal visual operator) 'global "gk" 'beginning-of-buffer)

(global-set-key (kbd "M-I") 'beginning-of-line)
(global-set-key (kbd "M-A") 'end-of-line)

(global-set-key (kbd "M-C-l") 'end-of-visual-line)
(global-set-key (kbd "M-C-h") 'beginning-of-visual-line)
(global-set-key (kbd "M-C-S-l") 'end-of-line)
(global-set-key (kbd "M-C-S-h") 'beginning-of-line)

(global-set-key (kbd "M-C-j") 'end-of-buffer)
(global-set-key (kbd "M-C-k") 'beginning-of-buffer)

(global-set-key (kbd "M-w") 'evil-forward-word-begin)
(global-set-key (kbd "M-W") 'evil-forward-WORD-begin)
(global-set-key (kbd "M-b") 'evil-backward-word-begin)
(global-set-key (kbd "M-B") 'evil-backward-WORD-begin)

(global-subword-mode 1)

(defun qv/delete-forward-char ()
  (interactive)
  (delete-region (point) (min (save-excursion (forward-char) (point))
                              (save-excursion (end-of-line) (point)))))

(defun qv/delete-backward-char ()
  (interactive)
  (delete-region (point) (max (save-excursion (backward-char) (point))
                              (save-excursion (beginning-of-line) (point)))))

(defun qv/delete-forward-word ()
  (interactive)
  (delete-region (point) (min (save-excursion (forward-word) (point))
                              (save-excursion (end-of-line) (point)))))

(defun qv/delete-backward-word ()
  (interactive)
  (delete-region (point) (max (save-excursion (backward-word) (point))
                              (save-excursion (beginning-of-line) (point)))))

(evil-define-key 'normal 'global "x" 'qv/delete-forward-char)
(evil-define-key 'normal 'global "z" 'qv/delete-backward-char)
(evil-define-key 'normal 'global "X" 'qv/delete-forward-word)
(evil-define-key 'normal 'global "Z" 'qv/delete-backward-word)

(global-set-key (kbd "M-z") 'qv/delete-backward-char)
(global-set-key (kbd "M-x") 'qv/delete-forward-char)
(global-set-key (kbd "M-Z") 'qv/delete-backward-word)
(global-set-key (kbd "M-X") 'qv/delete-forward-word)

(evil-define-key 'visual 'global "x" 'qv/delete-without-saving)
(evil-define-key 'visual 'global "z" 'qv/delete-without-saving)
(evil-define-key 'visual 'global "X" 'qv/delete-without-saving)
(evil-define-key 'visual 'global "Z" 'qv/delete-without-saving)

(evil-define-key 'operator 'global "v" 'evil-inner-word)
(evil-define-key 'operator 'global "V" 'evil-inner-WORD)

(evil-define-key 'operator 'global "z" 'evil-first-non-blank)
(evil-define-key 'operator 'global "x" 'evil-end-of-line)

(defun qv/visual-indent ()
  (interactive)
  (evil-normal-state)
  (let ((point-pos (point))
        (mark-pos (save-excursion (exchange-point-and-mark) (beginning-of-line) (point))))
    (evil-shift-right (min point-pos mark-pos) (max point-pos mark-pos)))
  (evil-visual-restore))

(defun qv/visual-unindent ()
  (interactive)
  (evil-normal-state)
  (let ((point-pos (point))
        (mark-pos (save-excursion (exchange-point-and-mark) (beginning-of-line) (point))))
    (evil-shift-left (min point-pos mark-pos) (max point-pos mark-pos)))
  (evil-visual-restore))

(evil-define-key 'normal 'global ">" 'evil-shift-right-line)
(evil-define-key 'normal 'global "<" 'evil-shift-left-line)
(evil-define-key 'visual 'global ">" 'qv/visual-indent)
(evil-define-key 'visual 'global "<" 'qv/visual-unindent)

(global-set-key (kbd "M-a") 'evil-shift-left-line)
(global-set-key (kbd "M-d") 'evil-shift-right-line)
(global-set-key (kbd "M-<") 'evil-shift-left-line)
(global-set-key (kbd "M->") 'evil-shift-right-line)

(defun qv/format-buffer-indentation ()
  (interactive)
  (evil-indent 0 (save-excursion (end-of-buffer) (point)))
  (exchange-point-and-mark)
  (setq mark-active nil))

(evil-define-key 'normal 'global "g=" 'qv/format-buffer-indentation)

(evil-define-operator qv/delete-without-saving (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE without saving to the kill ring if no register is specified."
  (interactive "<R><x><y>")
  (evil-delete beg end type (or register 95) yank-handler))

(evil-define-operator qv/change-without-saving (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE without saving to the kill ring if no register is specified."
  (interactive "<R><x><y>")
  (evil-change beg end type (or register 95) yank-handler))

(evil-define-key '(normal visual operator) 'global "d" 'qv/delete-without-saving)
(evil-define-key '(normal visual operator) 'global "c" 'qv/change-without-saving)
(evil-define-key '(normal visual operator) 'global "s" 'evil-delete)

(evil-define-operator qv/delete-line-without-saving (beg end type register yank-handler)
  "Delete whole line without saving to the kill ring if no register is specified."
  :motion evil-line-or-visual-line
  (interactive "<R><x>")
  (evil-delete beg end type (or register 95) yank-handler))

(evil-define-operator qv/change-line-without-saving (beg end type register yank-handler)
  "Delete whole line without saving to the kill ring if no register is specified."
  :motion evil-line-or-visual-line
  (interactive "<R><x>")
  (evil-change beg end type (or register 95) yank-handler))

(evil-define-key '(normal visual) 'global "D" 'qv/delete-line-without-saving)
(evil-define-key '(normal visual) 'global "C" 'qv/change-line-without-saving)
(evil-define-key '(normal visual) 'global "S" 'evil-delete-whole-line)
(evil-define-key 'normal 'global "Y" 'evil-yank-line)

(global-set-key (kbd "M-q") 'evil-force-normal-state)

(evil-define-key '(normal visual) 'global "/" 'isearch-forward)
(evil-define-key '(normal visual) 'global "g/" 'isearch-forward-regexp)
(evil-define-key '(normal visual) 'global "?" 'isearch-backward)
(evil-define-key '(normal visual) 'global "g?" 'isearch-backward-regexp)
(evil-define-key '(normal visual) 'global "n" 'isearch-repeat-forward)
(evil-define-key '(normal visual) 'global "N" 'isearch-repeat-backward)

(evil-define-key 'normal 'global "u" 'undo-tree-undo)
(evil-define-key 'normal 'global "U" 'undo-tree-redo)

(evil-define-key 'normal 'global "M" 'evil-join)

(evil-define-key 'visual 'global (kbd "<return>") 'eval-region)

(global-set-key (kbd "S-DEL") 'kill-region)

(use-package org)

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-ellipsis " ")

(setq org-return-follows-link t)

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords
 'org-mode
 '(("\\(^\n\\)" (1 '(:height 0.5)))))

(setq org-pretty-entities t)

(setq qv/org-header-family "WignersFriend")
(setq qv/org-headers-one-color nil)
(qv-startup/org-outline-faces)
(qv-startup/org-special-faces)

(add-hook 'org-mode-hook 'org-indent-mode)

(defvar qv/variable-pitch-mode-line-spacing 0.2)
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1)
                           (setq line-spacing qv/variable-pitch-mode-line-spacing)))

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

(add-hook 'org-mode-hook (lambda () (ignore-errors (org-connect-show-separator 1))))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c /") 'counsel-outline)))

(add-hook
 'org-mode-hook
 (lambda ()
   (evil-define-key 'normal 'local (kbd "<return>") 'org-open-at-point)
   (evil-define-key 'visual 'local (kbd "<return>") 'eval-region)
   (evil-define-key 'insert 'local (kbd "<return>") 'newline)))

(defun qv/org-toggle-fold ()
  (interactive)
  (org-back-to-heading)
  (let ((char (point)))
    (outline-toggle-children)
    (goto-char char)))

(evil-define-key 'normal 'global " " 'qv/org-toggle-fold)

(defun qv/org-toggle-branches ()
  (interactive)
  (unless (org-at-heading-p) (outline-back-to-heading))
  (if (and (save-excursion (next-line) (org-at-heading-p))
           (>= (org-current-level) (save-excursion (next-line) (org-current-level))))
      (outline-show-branches) (outline-hide-subtree)))

(evil-define-key 'normal 'global "g " 'qv/org-toggle-branches)

(add-hook 'org-mode-hook (lambda () (local-unset-key (kbd "M-h"))))

(defun visual-equation ()
  (interactive)
  (let ((beg (min (point) (mark))))
    (evil-append 1)
    (evil-normal-state)
    (backward-char)
    (insert "〉")
    (goto-char beg)
    (insert "〈")
    (setq myb beg)
    (qv/org-equation-overlays)))

(add-hook
 'org-mode-hook
 (lambda ()
   (evil-define-key 'normal 'local (kbd "M-`") 'qv/org-equation-overlays)
   (evil-define-key 'visual 'local (kbd "M-`") 'visual-equation)
   (evil-define-key 'insert 'local (kbd "M-`")
     (lambda () (interactive)
       (insert "〈〉")
       (backward-char)
       (qv/org-equation-overlays)))))

(defvar qv/org-showing-meta-text nil
  "If non-nil, hide meta lines in org mode buffers.")

(setq qv/org-hide-exclude-keywords
      '("begin_src"
        "end_src"))

(setq qv/org-show-value-keywords
      '("title"
        "author"
        "description"))

(defun qv/org-show-meta-text (&optional state)
  "If STATE is positive, show meta text
If STATE is negative, hide meta text.
If STATE is 0, do not make any change, but make sure
that the text is being displayed/hidden properly.
Otherwise, toggle meta text."
  (interactive)
  (setq-local qv/org-showing-meta-text
              (if (numberp state)
                  (if (eq state 0) qv/org-showing-meta-text
                    (if (< state 0) nil t))
                (not qv/org-showing-meta-text)))

  (remove-overlays nil nil 'qv/hide-meta-lines t)

  (unless qv/org-showing-meta-text
    (let ((original-position (point)))

      (beginning-of-buffer)
      (while (search-forward-regexp "^#\\+[a-zA-Z]" nil t)
        (beginning-of-line)
        (let* ((case-fold-search t)
               (exclude-regexp
                (concat "\\(#\\+"
                        (string-join qv/org-hide-exclude-keywords "[: \n]\\|#\\+")
                        "[: \n]\\)"))
               (end-regexp
                (concat "\\(#\\+"
                        (string-join qv/org-show-value-keywords ":? \\|#\\+")
                        ":? \\|\n\\)"))
               (beg (point))
               (end (save-excursion (search-forward-regexp end-regexp nil t) (point)))
               (line (buffer-substring-no-properties beg end)))
          (when (string= line (replace-regexp-in-string
                               exclude-regexp "" line))
            (let ((overlay (make-overlay beg end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'qv/hide-meta-lines t)))
          (end-of-line)))

      (goto-char original-position))))

(defvar qv/org-showing-drawers nil
  "If non-nil, hide drawers in org mode buffers.")
(setq-default qv/org-showing-drawers nil)

(defun qv/org-show-drawers (&optional state)
  "If STATE is positive, show drawers
If STATE is negative, hide drawers.
If STATE is 0, do not make any change, but make sure
that drawers are being displayed/hidden properly.
Otherwise, toggle drawers."
  (interactive)
  (setq-local qv/org-showing-drawers
              (if (numberp state)
                  (if (eq state 0) qv/org-showing-drawers
                    (if (< state 0) nil t))
                (not qv/org-showing-drawers)))

  (defvar-local qv/org-drawer-overlays nil
    "Store the overlays for drawers and meta text in the current buffer")
  (mapcar 'delete-overlay qv/org-drawer-overlays)
  (setq-local qv/org-drawer-overlays nil)

  (unless qv/org-showing-drawers
    (let ((original-position (point)))

      (beginning-of-buffer)
      (while (search-forward-regexp org-drawer-regexp nil t)
        (beginning-of-line)
        (when (ignore-error t (org-element-drawer-parser nil (list (point))))
          (let* ((props (cadr (org-element-drawer-parser nil (list (point)))))
                 (beg (plist-get props ':begin))
                 (end (plist-get props ':end))
                 (overlay (make-overlay (1- beg) (1- end))))
            (overlay-put overlay 'invisible t)
            (setq-local qv/org-drawer-overlays
                        (append qv/org-drawer-overlays (list overlay)))
            (goto-char (1- end))))
        (forward-char))
      (goto-char original-position))))

(add-hook 'org-mode-hook
          (lambda ()
            (qv/org-show-meta-text -1)
            (qv/org-show-drawers 1)
            (local-set-key (kbd "C-c C-h") 'qv/org-show-meta-text)
            (local-set-key (kbd "C-c C-S-h") 'qv/org-show-drawers)))

(add-hook 'org-mode-hook 'qv/org-equation-overlays)

(defun qv/org-equation-overlays ()
  "Search the buffer for equations surrounded by ``, and
italicize them using an overlay so as not to invalidate
other formatting."
  (interactive)

  (remove-overlays nil nil 'qv/equation t)

  (let ((original-position (point)))
    (beginning-of-buffer)
    (while (search-forward-regexp "〈.*?〉" nil t)
      (search-backward "〈")

      (let ((overlay (make-overlay (point) (search-forward "〉"))))
        (overlay-put overlay 'face '(:slant italic :height 1.05))
        (overlay-put overlay 'qv/equation t)))

    (beginning-of-buffer)
    (while (search-forward "√" nil t)
      (let ((overlay (make-overlay (1- (point)) (point))))
        (overlay-put overlay 'face '(:slant normal))
        (overlay-put overlay 'qv/equation t)
        (when (string= (buffer-substring (point) (1+ (point))) "{")
          (overlay-put overlay 'display '((raise 0.1))))))

    (goto-char original-position)))

(use-package visual-fill-column
  :after org)

(defun qv/visual-fill-column-hook ()
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'qv/visual-fill-column-hook)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("•" "⁃" "•" "⁃" "•" "⁃" "•")))

(defun qv/make-code-block (language)
  (interactive '("emacs-lisp"))
  (beginning-of-line)
  (evil-set-marker 8472)
  (exchange-point-and-mark)
  (evil-normal-state)
  (evil-set-marker 8473)
  (beginning-of-line)
  (insert (concat "#+BEGIN_SRC " language "\n"))
  (evil-goto-mark 8472)
  (insert "#+END_SRC\n")
  (evil-goto-mark 8473))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-b") 'qv/make-code-block)))

(setq qv/tangle-files
      '("~/.emacs.d/init.el.org"))

;; (add-hook
;;   'org-mode-hook
;;   (lambda () (when (member buffer-file-name (mapcar 'expand-file-name qv/tangle-files))
;;                (local-set-key (kbd "C-c C-c") 'org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-t") 'org-babel-tangle)))

(defvar qv/latex-enabled t)

(defun qv/toggle-latex (&optional arg)
  (interactive)
  (setq-local qv/latex-enabled
              (if (numberp arg)
                  (if (eq arg 0) qv/latex-enabled
                    (if (< arg 0) nil t))
                (not qv/latex-enabled)))
  (if qv/latex-enabled
      (org-latex-preview '(16))
    (org-latex-preview '(64))))

(add-hook 'org-mode-hook
          (lambda ()
            (qv/toggle-latex 0)
            (local-set-key (kbd "C-c C-l") 'qv/toggle-latex)))

(plist-put org-format-latex-options :scale 3)

(setq qv/insert-symbols-alist
      '(("`" . "∙")
        ("." . "·")
        (";" . "°")
        ("-" . "−")
        ("~" . "≈")
        ("+" . "±")
        ("/" . "⁄")
        ("*" . "×")
        ("m r" . "√")
        ("m i" . "∞")
        ("g a" . "∡")
        ("g A" . "∢")
        ("g t" . "Δ")
        ("g =" . "∥")
        ("g +" . "⟂")
        ("s u" . "∪")
        ("s i" . "∩")
        ("s e" . "∈")
        ("l p" . "π")
        ("l t" . "θ")
        ("f 1 /" . "⅟")
        ("f 1 2" . "½")
        ("f 1 3" . "⅓")
        ("f 2 3" . "⅔")
        ("f 1 4" . "¼")
        ("f 3 4" . "¾")
        ("f 1 5" . "⅕")
        ("f 2 5" . "⅖")
        ("f 3 5" . "⅗")
        ("f 4 5" . "⅘")
        ("f 1 6" . "⅙")
        ("f 5 6" . "⅚")
        ("f 1 7" . "⅐")
        ("f 1 8" . "⅛")
        ("f 5 8" . "⅝")
        ("f 7 8" . "⅞")
        ("f 1 9" . "⅑")))

(defface qv/delimiter '((t :height 0.1))
  "Face for easily changing whether equation delimiters (`)
are visible (full height) or invisible (tiny height).")

(font-lock-add-keywords
 'org-mode
 '(("∡" (0 '(:height 1.05)))
   ("√" (0 `(:family ,(face-attribute 'fixed-pitch ':family))))
   ("[〈〉]" (0 'qv/delimiter))
   ("\\(⌈\\)\\(.+?\\)\\(⌉\\)"
    (1 'qv/delimiter) (2 '(:overline t)) (3 'qv/delimiter))
   ("√\\({\\)\\([^}\n]*\\)\\(}\\)"
    (1 '(:overline t :inherit qv/delimiter)) (2 '(:overline t))
    (3 '(:overline t :inherit qv/delimiter)))))

(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i M-h") 'org-insert-heading)
(global-set-key (kbd "M-i M-l") 'org-insert-link)
(global-set-key (kbd "M-i M-d") 'org-insert-drawer)
(global-set-key (kbd "M-i M-m")
                (lambda () (interactive)
                  (setq org-pretty-entities (not org-hide-emphasis-markers)
                        org-hide-emphasis-markers (not org-hide-emphasis-markers))
                  (org-mode-restart)))
(global-set-key (kbd "M-i M-b")
                (lambda () (interactive)
                  (insert "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
                  (previous-line)))

(dolist (i qv/insert-symbols-alist)
  (global-set-key (kbd (concat "M-i " (car i)))
                  (eval `(lambda () (interactive) (insert ,(cdr i))))))

(global-set-key (kbd "M-i e")
                (lambda () (interactive)
                  (insert (format "〈%s〉" (read-string "Equation: ")))
                  (qv/org-equation-overlays)))
(global-set-key (kbd "M-i E")
                (lambda () (interactive)
                  (insert "〈〉")
                  (backward-char)
                  (qv/org-equation-overlays)))
(global-set-key (kbd "M-e")
                (lambda () (interactive)
                  (insert (format "〈%s〉" (read-string "Equation: ")))
                  (qv/org-equation-overlays)))
(global-set-key (kbd "M-E")
                (lambda () (interactive)
                  (insert "〈〉")
                  (backward-char)
                  (qv/org-equation-overlays)))

(global-set-key (kbd "M-i g l")
                (lambda () (interactive)
                  (insert (format "⌈%s⌉" (read-string "Line: ")))))
(global-set-key (kbd "M-i g L")
                (lambda () (interactive)
                  (insert "⌈⌉")
                  (backward-char)))

(defun qv/format-code-block-indentation ()
  (interactive)
  (let ((case-fold-search t)
        (original-pos (cons (line-number-at-pos) (current-column)))
        (original-indentation (current-indentation))
        (mode nil) (text nil) (beg nil) (end nil))
    
    ;; Find the start and end of the source block
    (beginning-of-line)
    (search-forward-regexp "^[ 	]*#\\+END_SRC")
    (previous-line) (end-of-line) (setq end (point))
    (search-backward-regexp "^[ 	]*#\\+BEGIN_SRC[ \n]")
    
    ;; Figure out the language of the source block
    (setq mode (car (read-from-string
                     (concat (replace-regexp-in-string
                              "#\\+BEGIN_SRC \\([^ ]+\\).*" "\\1"
                              (buffer-substring-no-properties
                               (point) (save-excursion (end-of-line) (point))))
                             "-mode"))))

    ;; Save and delete the contents of the source block
    (next-line) (beginning-of-line) (setq beg (point))
    (setq text (buffer-substring-no-properties beg end))
    (delete-region beg end)

    ;; Insert the contents into a temporary buffer, indent it, and copy it
    (with-temp-buffer
      (if (commandp mode)
          (eval (list mode))
        (emacs-lisp-mode))
      (insert text)
      (indent-region (buffer-end -1) (buffer-end 1))
      (setq text (buffer-substring-no-properties (buffer-end -1) (buffer-end 1))))

    ;; Return to the original buffer and insert the indented text
    (insert text)
    (goto-line (car original-pos))
    (move-to-column (+ (cdr original-pos)
                       (current-indentation)
                       (- original-indentation)))))

(add-hook 'org-mode-hook
          (lambda () (evil-define-key 'normal 'local "g=" 'qv/format-code-block-indentation)))

(qv/run-in-background "xgamma -gamma 1.2")
(qv/run-in-background "xrandr -s 1920x1080")

(use-package exwm)

(setq exwm-manage-configurations '((t char-mode t)))

(defun qv/exwm-update-class ()
  (exwm-workspace-rename-buffer (concat ":" exwm-class-name ":")))

(add-hook 'exwm-update-class-hook #'qv/exwm-update-class)

(setq exwm-input-global-keys nil)

(exwm-input-set-key (kbd "s-x") 'execute-extended-command)
(exwm-input-set-key (kbd "s-r") 'qv/run-in-background)
(exwm-input-set-key (kbd "s-e") 'eval-expression)
(exwm-input-set-key (kbd "s-E") 'repeat-complex-command)
(exwm-input-set-key (kbd "s-s") 'set-variable)
(exwm-input-set-key (kbd "s-o") 'counsel-linux-app)

(exwm-input-set-key (kbd "s-s")
                    (lambda (key-sequence) (interactive "kKey Sequence: ")
                      (call-interactively (key-binding key-sequence))))

(exwm-input-set-key (kbd "s-q")
                    (lambda () (interactive)
                      (eval (list (key-binding (kbd "C-g"))))))

;; When deleting a window, go to the closest window in the layout
(exwm-input-set-key (kbd "s-w")
                    (lambda () (interactive)
                      (let ((remove (selected-window))
                            (goto (or (window-next-sibling) (window-prev-sibling))))
                        (while (and (windowp goto) (not (window-live-p goto)))
                          (setq goto (window-child goto)))
                        (select-window goto)
                        (delete-window remove))))

(exwm-input-set-key (kbd "s-W") 'kill-buffer)

(exwm-input-set-key (kbd "s-B") 'switch-to-buffer)
(exwm-input-set-key (kbd "s-b") 'qv/activity-switch-buffer)
(exwm-input-set-key (kbd "s-M-b") 'qv/add-buffer-to-activity)
(exwm-input-set-key (kbd "s-a") 'qv/switch-to-activity)
(exwm-input-set-key (kbd "s-A") 'qv/add-activity)
(exwm-input-set-key (kbd "s-C-a") 'qv/remove-activity)
(exwm-input-set-key (kbd "s-d") 'qv/switch-to-layout)
(exwm-input-set-key (kbd "s-D") 'qv/add-layout)
(exwm-input-set-key (kbd "s-C-d") 'qv/remove-layout)
(exwm-input-set-key (kbd "s-c")
                    (lambda () (interactive)
                      (qv/switch-to-activity (car qv/last-activity))))
(exwm-input-set-key (kbd "s-C")
                    (lambda () (interactive)
                      (qv/switch-to-activity
                       (car (nth (mod (1+ (-elem-index
                                           qv/current-activity
                                           qv/activities))
                                      (length qv/activities))
                                 qv/activities)))))

(exwm-input-set-key
 (kbd "s-C-Z")
 (lambda () (interactive)
   (start-process-shell-command "Kill emacs" nil
    "kill $(ps -ef | grep emacs | awk '{print $3}')")))

(exwm-input-set-key (kbd "s-C-S-S")
                    (lambda () (interactive)
                      (qv/run-in-background "slock")
                      (qv/run-in-background "systemctl suspend")))

(exwm-input-set-key (kbd "s-C-R")
                    (lambda () (interactive)
                      (qv/run-in-background "systemctl reboot")))

(exwm-input-set-key (kbd "s-C-P")
                    (lambda () (interactive)
                      (qv/run-in-background "shutdown now")))

(exwm-input-set-key (kbd "s-c")
                    (lambda () (interactive)
                      (start-process-shell-command "Click" nil "xdotool click 1")))

(exwm-input-set-key (kbd "s-M-c")
                    (lambda () (interactive)
                      (start-process-shell-command "Click" nil "xdotool click 3")))

(exwm-input-set-key (kbd "s-C")
                    (lambda () (interactive)
                      (start-process-shell-command "Click" nil "xdotool click 3")))

(exwm-input-set-key (kbd "s-C-c")
                    (lambda () (interactive)
                      (start-process-shell-command
                       "Focus" nil
                       (string-join
                        '("xdotool mousemove 960 540"
                          "xdotool click 1"
                          "xdotool mousemove 1920 1080")
                        " ; "))))

(exwm-input-set-key (kbd "s-f") 'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-h") 'windmove-left)
(exwm-input-set-key (kbd "s-l") 'windmove-right)
(exwm-input-set-key (kbd "s-j") 'windmove-down)
(exwm-input-set-key (kbd "s-k") 'windmove-up)

(exwm-input-set-key (kbd "s-C-h") 'qv/window-move-left)
(exwm-input-set-key (kbd "s-C-l") 'qv/window-move-right)
(exwm-input-set-key (kbd "s-C-j") 'qv/window-move-down)
(exwm-input-set-key (kbd "s-C-k") 'qv/window-move-up)

(exwm-input-set-key (kbd "s-C-M-h") 'windmove-swap-states-left)
(exwm-input-set-key (kbd "s-C-M-l") 'windmove-swap-states-right)
(exwm-input-set-key (kbd "s-C-M-j") 'windmove-swap-states-down)
(exwm-input-set-key (kbd "s-C-M-k") 'windmove-swap-states-up)

(exwm-input-set-key (kbd "s-H")
                    (lambda () (interactive)
                      (split-window nil nil 'right)
                      (switch-to-buffer "*scratch*")))
(exwm-input-set-key (kbd "s-L")
                    (lambda () (interactive)
                      (split-window nil nil 'left)
                      (switch-to-buffer "*scratch*")))
(exwm-input-set-key (kbd "s-J")
                    (lambda () (interactive)
                      (split-window nil nil 'up)
                      (switch-to-buffer "*scratch*")))
(exwm-input-set-key (kbd "s-K")
                    (lambda () (interactive)
                      (split-window nil nil 'down)
                      (switch-to-buffer "*scratch*")))

(exwm-input-set-key (kbd "s-M-h")
                    (lambda () (interactive)
                      (window-resize (selected-window) -4 t)))
(exwm-input-set-key (kbd "s-M-l")
                    (lambda () (interactive)
                      (window-resize (selected-window) +4 t)))
(exwm-input-set-key (kbd "s-M-j")
                    (lambda () (interactive)
                      (window-resize (selected-window) -1 nil)))
(exwm-input-set-key (kbd "s-M-k")
                    (lambda () (interactive)
                      (window-resize (selected-window) +1 nil)))

(exwm-input-set-key (kbd "s-M-H")
                    (lambda () (interactive)
                      (window-resize (selected-window) -16 t)))
(exwm-input-set-key (kbd "s-M-L")
                    (lambda () (interactive)
                      (window-resize (selected-window) +16 t)))
(exwm-input-set-key (kbd "s-M-J")
                    (lambda () (interactive)
                      (window-resize (selected-window) -5 nil)))
(exwm-input-set-key (kbd "s-M-K")
                    (lambda () (interactive)
                      (window-resize (selected-window) +5 nil)))

(exwm-enable)

(defun qv/set-opacity (opacity)
  (set-frame-parameter (selected-frame) 'alpha (cons opacity opacity))
  (add-to-list 'default-frame-alist (cons 'alpha (cons opacity opacity)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(qv/set-opacity 90)

(qv/run-in-background "compton --config /home/qv/.config/compton/compton.conf")

(defun qv/set-wallpaper (wallpaper)
  (qv/run-in-background
   (concat "feh --bg-scale "
           (expand-file-name "~/Media/Wallpaper/")
           wallpaper)))

(qv/set-wallpaper "RedNebulaWallpaper.jpeg")

(defun qv/current-pulseaudio-sink ()
  "Return the number of the active pulse audio sink"
  (string-to-number (replace-regexp-in-string
                     ".*Sink #\\([[:digit:]]+\\)--State: RUNNING.*" "\\1"
                     (replace-regexp-in-string
                      "\n[[:space:]]*" "--"
                      (shell-command-to-string "pactl list sinks")))))

(defun qv/pulseaudio-sink-volume (sink)
  "Return the current volume of the sink number specified by the argument"
  (string-to-number
   (replace-regexp-in-string
    (concat ".*Sink #" (number-to-string sink) "--.*?Volume:.*? \\([[:digit:]]+\\)%.*")
    "\\1"
    (replace-regexp-in-string
     "\n[[:space:]]*" "--"
     (shell-command-to-string "pactl list sinks")))))

(defun qv/pulseaudio-sink-description (sink)
  "Return the description of the sink number specified by the argument"
  (replace-regexp-in-string
   (concat ".*Sink #" (number-to-string sink) "--.*?Description: \\(.*?\\)--.*")
   "\\1"
   (replace-regexp-in-string
    "\n[[:space:]]*" "--"
    (shell-command-to-string "pactl list sinks"))))

(defun qv/change-volume (delta &optional sink)
  "Change the system volume by DELTA using pactl. \
If SINK is specified, use that as the output device instead of the active sink"
  (let ((use-sink (or sink (qv/current-pulseaudio-sink))))
    (if delta
        (qv/run-in-background
         (concat "pactl set-sink-volume "
                 (number-to-string use-sink) " +"
                 (number-to-string delta) "%"))
      (qv/run-in-background
       (concat "pactl set-sink-volume " (number-to-string use-sink) " 0%")))
    (message (concat "Volume is now "
                     (number-to-string (qv/pulseaudio-sink-volume use-sink))
                     "%% for "
                     (qv/pulseaudio-sink-description use-sink)))))

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive) (qv/change-volume 5)))

(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive) (qv/change-volume -5)))

(exwm-input-set-key (kbd "<XF86AudioMute>")
                    (lambda () (interactive) (qv/change-volume nil)))

(setq qv/system-brightness 1.0)

(defun qv/change-brightness (delta &optional monitor)
  "Change the system brightness by DELTA using xrandr. \
If delta is a float, multiply the current brightness by delta instead. \
If MONITOR is specified, change the brightness of it instead of eDP-1"
  (setq qv/system-brightness (min 1.5 (max 0 (+ qv/system-brightness delta))))
  (qv/run-in-background (concat "xrandr --output "
                                  (or monitor "eDP-1")
                                  " --brightness "
                                  (number-to-string qv/system-brightness)))
  (message (concat "Brightness is now "
                   (substring
                    (concat (number-to-string
                             (* 0.01 (round qv/system-brightness 0.01)))
                            "0000")
                    0 4))))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                    (lambda () (interactive)
                      (qv/change-brightness 0.05)))

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                    (lambda () (interactive)
                      (qv/change-brightness -0.05)))

(defun qv/xinput-id (device-name)
  "Return the xinput id of the device with DEVICE-NAME"
  (string-to-number
   (shell-command-to-string (concat "xinput list | grep '" device-name
                                    "[ 	]*id=' | sed \"s/.*id=\\\\([0-9]*\\\\).*/\\\\1/\""))))

(defun qv/xinput-property-number (device property-name)
  "Return the number of the property with PROPERTY-NAME
for the device with id or name of DEVICE"
  (string-to-number
   (shell-command-to-string
    (concat "xinput list-props "
            (number-to-string (if (numberp device) device
                                (qv/xinput-id device)))
            " | sed -n \"s/.*" property-name " *(\\\\([0-9]*\\\\).*/\\\\1/p\""))))

(defun qv/xinput-set-property (device property value)
  (interactive
   (let ((id (qv/xinput-id
              (completing-read
               "Select Device: "
               (split-string (shell-command-to-string
                              (concat
                               "xinput list | sed -n \"s/"
                               "[^a-zA-Z]*\\\\([a-zA-Z].*[^ 	]\\\\)[ 	]*id=.*/"
                               "\\\\1/p\"")) "\n")))))
     (list id (qv/xinput-property-number
               id (car (split-string
                        (completing-read
                         "Property: "
                         (split-string
                          (shell-command-to-string
                           (concat "xinput list-props 12"
                                   "| sed -n \""
                                   "s/[ 	]*\\\\(.*[^ 	]\\\\)"
                                   "[ 	]*([0-9]*):[ 	]*\\\\(.*\\\\)/"
                                   "\\\\1 (\\\\2)/p\""
                                   "| sed \"s/libinput *//\""))
                          "\n")) " (")))
           (read-string "New value: "))))
  (qv/run-in-background
   (concat "xinput set-prop "
           (number-to-string (if (numberp device)
                                 device
                               (qv/xinput-id device))) " "
           (number-to-string (if (numberp property)
                                 property
                               (qv/xinput-property-number device property))) " "
           (if (numberp value) (number-to-string value) value)))
  value)

(qv/xinput-set-property 12 331 1)

(global-set-key (kbd "C-s C-h C-t")
                (lambda () (interactive)
                  (qv/xinput-set-property
                   12 331
                   (- (1- (string-to-number
                           (shell-command-to-string
                            "xinput list-props 12 | sed -n 's/.*(331):\\t//p'")))))))

(qv/run-in-background "xrandr -s 1920x1080")

(evil-define-key 'normal 'global (kbd "C-d C-h") 'org-connect-history-backward)
(evil-define-key 'normal 'global (kbd "C-d C-l") 'org-connect-history-forward)

(evil-define-key 'normal 'global (kbd "C-d C-a") 'org-connect-add-file-link)
(evil-define-key 'normal 'global (kbd "C-d C-n") 'org-connect-add-note)
(evil-define-key 'normal 'global (kbd "C-d C-d") 'org-connect-remove-current-link)
(evil-define-key 'normal 'global (kbd "C-d C-S-d")
  (lambda () (interactive) (org-connect-remove-current-link t)))

(evil-define-key 'normal 'global (kbd "C-d C-f") 'org-connect-open-file)
(evil-define-key 'normal 'global (kbd "C-d C-b") 'org-connect-open-buffer)

(evil-define-key 'normal 'global (kbd "C-d C-j") 'org-connect-move-down)
(evil-define-key 'normal 'global (kbd "C-d C-k") 'org-connect-move-up)

(defun org-connect-open-file (file)
  (interactive "fOpen File: ")
  (org-connect-open file))

(defun org-connect-open-buffer (buffer)
  (interactive "bOpen Buffer: ")
  (org-connect-open buffer))

(defcustom org-connect-links nil
  "An alist of links for the current buffer of the form returned by org-connect--parse-links."
  :local t)

(defconst org-connect-link-drawer-regexp
  (concat "\\([^z-a]*?\\(?:^[ 	]*:LINKS:[ 	]*$\\)\\)\n"
          "\\([^z-a]*?\\)\n?"
          "\\(\\(?:^[ 	]*:END:[ 	]*$\\)[^z-a]*\\)$")
  "The regexp used to match the drawer for storing links in a note.")

(defconst org-connect-document-info-regexp
"\\([^z-a]*#\\+[-_a-zA-Z0-9]: .*\n\\)\\([^z-a]*\\)$"
  "The regexp used to match the beginning and end of the document info")

(defvar org-connect-file-name-convention '(hyphen . nil)
"A cons pair which indicates the naming convention to use for notes.
The car value signifies how to separate words of a note,
it can be 'underscore 'space, 'hyphen, or 'camel (for camel case.)
Any nil or non-nil value other than those specified will default to 'underscore.
The cdr value is either nil or non-nil to specify whether to capitalize
the first letter of each word. If the car is 'camel, this instead
specifies whether the first word should be capitalized.")

(defun org-connect-name-from-file (filename)
"Return the name of the note from the file path FILENAME
based on the conventions set by org-connect-file-name-convention"
  (capitalize
   (let ((case-fold-search nil)
         (separator (car org-connect-file-name-convention)))
     (replace-regexp-in-string
      (cond ((eq separator 'hyphen) "-")
            ((eq separator 'space) " ")
            ((eq separator 'camel) "\\([a-z]\\)\\([A-Z]\\)")
            (t "_"))
      (if (eq separator 'camel) "\\1 \\2" " ")
      (file-name-base filename)))))

(defun org-connect-file-from-name (name)
  "Return the name of the file from the note name NAME
based on the conventions set by org-connect-file-name-convention"
  (let ((new-name (substring name))
        (separator (car org-connect-file-name-convention))
        (case (cdr org-connect-file-name-convention)))
    (if (eq separator 'camel)
        (if case
            (s-upper-camel-case new-name)
          (s-lower-camel-case new-name))
      (if case
          (setq new-name (capitalize new-name))
        (setq new-name (downcase new-name)))
      (replace-regexp-in-string
       " "
       (cond ((eq separator 'hyphen) "-")
             ((eq separator 'space) " ")
             (t "_"))
       new-name))))

(defun org-connect-parse-links (&optional file-or-buffer)
  "Parse the links drawer of the current buffer, and return an
alist with the keys being the link types, and the values being
the list of links for that type in the form of file paths.
If FILE-OR-BUFFER is set, use the file path or buffer specified
instead of the current buffer."
  (let* ((current-type nil)
         (link-alist (list (list current-type)))
         (file (org-connect-check-file-or-buffer file-or-buffer))
         (contents
          (if (get-file-buffer file)
              (with-current-buffer
                  (get-file-buffer file)
                (buffer-substring-no-properties 1 (buffer-end 1)))
            (org-file-contents file))))

    ;; Loop through a list of the lines between the first :LINKS: and :END: tags
    (when (string-match-p org-connect-link-drawer-regexp contents)
      (dolist (i (split-string
                  (replace-regexp-in-string
                   org-connect-link-drawer-regexp "\\2" contents) "\n"))

        ;; Remove all spaces from the beginning and end of the links
        (setq i (replace-regexp-in-string "^[ 	]*" "" i))
        (setq i (replace-regexp-in-string "[ 	]*$" "" i))
        
        ;; Check if the line is a valid link
        (if (string-equal (replace-regexp-in-string "^\\[\\[.*\\]\\[.*\\]\\]" "" i) "")
            ;; If it is a link, add it to the current category
            (setcdr (assoc current-type link-alist)
                    (append (cdr (assoc current-type link-alist)) (list i)))
          ;; If it isn't a link, add a new category
          (setq current-type i)
          (setq link-alist (append link-alist (list (cons i '())))))))
    link-alist))

(defun org-connect-write-links (links &optional file-or-buffer)
  (let* ((file (org-connect-check-file-or-buffer file-or-buffer))
         (buf (get-file-buffer file))
         (contents (org-file-contents file))
         (drawer-body
          (replace-regexp-in-string
           "^\n" ""
           (replace-regexp-in-string
            "\n$" ""
            (if (stringp links) links (string-join (flatten-list links) "\n")))))
         (before-text "")
         (after-text contents))

    ;; Try to find a place to put the drawer other than the beginning
    (if (string-match-p org-connect-link-drawer-regexp contents)
        (setq before-text (replace-regexp-in-string
                           org-connect-link-drawer-regexp "\\1" contents)
              after-text (replace-regexp-in-string
                          org-connect-link-drawer-regexp "\\3" contents))
      (when (string-match-p org-connect-document-info-regexp contents)
        (setq before-text (replace-regexp-in-string
                           org-connect-document-info-regexp "\\1:LINKS:\n" contents)
              after-text (replace-regexp-in-string
                          org-connect-document-info-regexp "\n:END:\n\\2" contents))))
    (if buf
        (with-current-buffer buf
          (delete-region (buffer-end -1) (buffer-end 1))
          (insert (concat before-text "\n" drawer-body "\n" after-text))
          (save-buffer)
          (org-mode-restart))
      (with-temp-buffer
        (insert (concat before-text "\n" drawer-body "\n" after-text))
        (write-file file)))))

(defvar org-connect-preset-categories
  '("Children"
    "Parents"
    "Documentation"
    "Resources"
    "Reference")
  "A list of categories to provide completion for when adding a link.")

(defun org-connect-add-file-link (category path &optional file no-backlink)
  "Add a link to the file PATH under the section CATEGORY,
and a corresponding backlink in the file PATH.
If FILE is specified, add the link with path FILE,
otherwise use the current buffer.
If NO-BACKLINK is non-nil, don't create a backlink."
  (interactive
   (list (completing-read "Link Category: " org-connect-preset-categories)
         (read-file-name "File to Link: ")))
  (let* ((actual-file
          (expand-file-name
           (or file
               (if (eq (current-buffer) org-connect-link-buffer)
                   org-connect-viewing-file
                 (buffer-file-name (current-buffer))))))
         (link-alist (org-connect-parse-links actual-file))
         (existing-category (assoc category link-alist))
         (link-string (concat "[[file:" path "]["
                              (org-connect-name-from-file path) "]]")))
    (if existing-category
        (setcdr existing-category
                (append (cdr existing-category)
                        (list link-string)))
      (setq link-alist
            (append link-alist
                    (list (list category link-string)))))
    (org-connect-write-links link-alist actual-file)
    (when (string-equal actual-file (expand-file-name org-connect-viewing-file))
      (save-window-excursion (org-connect-update-link-view actual-file)))

    ;; Create the backlink
    (unless no-backlink
      (org-connect-add-file-link
       (cond ((string= category "Children") "Parents")
             ((string= category "Parents") "Children")
             (t "Backlinks"))
       actual-file path t))))

(defun org-connect-remove-current-link (&optional remove-category)
  "Remove the link under the cursor in the view buffer.
If REMOVE-CATEGORY is non-nil and the cursor is on a category heading,
remove the category heading."
  (interactive)

  (unless (eq (current-buffer) org-connect-link-buffer)
    (error "Not in the link view buffer"))
  
  ;; Return an error if the cursor is not on a link
  (unless (or remove-category
              (string-match-p "^[ 	]*\\[\\[.*\\]\\[.*\\]\\][ 	]*$"
                              (buffer-substring-no-properties
                               (save-excursion (beginning-of-line) (point))
                               (save-excursion (end-of-line) (point)))))
    (error "Not on a link"))
  
  (let* ((category
          (save-excursion
            (when (search-backward-regexp "^[ 	]*[^[ 	]" nil t)
              (buffer-substring-no-properties
               (save-excursion (beginning-of-line-text) (point))
               (save-excursion (end-of-line) (point))))))
         (remove-category
          (cond ((string= category "Children") "Parents")
                ((string= category "Parents") "Children")
                ("Backlinks")))
         (linked-file
          (expand-file-name
           (replace-regexp-in-string 
            "^\\[\\[file:\\([^]]*\\)\\]\\[.*\\]\\].*$" "\\1"
            (buffer-substring-no-properties
             (save-excursion (beginning-of-line-text) (point))
             (save-excursion (end-of-line) (point)))))))

    ;; Remove the link from the current file
    (read-only-mode 0)
    (delete-region (save-excursion (beginning-of-line) (point))
                   (min (save-excursion (end-of-line) (1+ (point)))
                        (buffer-end 1)))
    (read-only-mode 1)
    (org-connect-write-link-buffer)
    
    ;; Remove the backlink from the linked file
    ;; TODO: Figure out whether or not there are still other links,
    ;; in which case don't remove the backlink
    (let ((new-links '())
          (removed-link nil))
      (dolist (i (org-connect-parse-links linked-file))
        (let ((new-item (list (car i))))
          (if (equal remove-category (car i))
              (dolist (j (cdr i))
                (if (string-match-p (regexp-quote org-connect-viewing-file) j)
                    (setq removed-link t)
                  (setq new-item (append new-item (list j)))))
            (setq new-item i))
          (unless (eq (length new-item) 1)
            (setq new-links (append new-links (list new-item))))))

      (org-connect-write-links new-links linked-file))))

(defvar org-connect-dont-write-moves nil
"If non-nil, dont write the updated link order to file after running
the org-connect-move or org-connect-move-up/down functions")

(defun org-connect-move-up ()
  "Run the org-connect-move command in the up direction"
  (interactive)
  (org-connect-move t))

(defun org-connect-move-down ()
  "Run the org-connect-move command in the down direction"
  (interactive)
  (org-connect-move nil))

(defun org-connect-move (direction)
  "If DIRECTION is non-nil, move the item under the cursor
in the view buffer up, otherwise move it down. Then, save the
updated order of links to the real file containing the links.
If the variable org-connect-dont-write-moves is non-nil,
do not save the new order of links to the real buffer."
  (if (string-match-p
       "^[ 	]*\\[\\[.*\\]\\[.*\\]\\]"
       (buffer-substring-no-properties
        (save-excursion (beginning-of-line) (point))
        (save-excursion (end-of-line) (point))))
      (org-connect-move-link direction org-connect-dont-write-moves)
    (org-connect-move-category direction org-connect-dont-write-moves)))

(defun org-connect-move-link (direction &optional dont-write)
  "If DIRECTION is non-nil, move the link under the cursor
in the view buffer up, otherwise move it down. Then, save the
updated order of links to the real file containing the links.
If DONT-WRITE is non-nil, do not save the new order of links
to the real buffer."
  (unless (eq (current-buffer) org-connect-link-buffer)
    (error "Not in the link view buffer"))
  (read-only-mode 0)
  (let* ((move-start (save-excursion (beginning-of-line) (point)))
         (move-end (save-excursion (end-of-line) (1+ (point))))
         (region-text (buffer-substring-no-properties move-start move-end)))
    (delete-region move-start move-end)
    (ignore-error t
      (if direction (previous-line) (next-line)))
    (beginning-of-line)
    (insert region-text)
    (previous-line)
    (beginning-of-line))
  (read-only-mode 1)
  (unless dont-write (org-connect-write-link-buffer)))

(defun org-connect-move-category (direction &optional dont-write)
  "If DIRECTION is non-nil, move the category under the cursor
in the view buffer up, otherwise move it down. Then, save the
updated order of links to the real file containing the links.
If DONT-WRITE is non-nil, do not save the new order of links
to the real buffer."
  (unless (eq (current-buffer) org-connect-link-buffer)
    (error "Not in the link view buffer"))
  (read-only-mode 0)
  ;; Find the start of the current category
  (let* ((original-point (point))
         (category-start
          (save-excursion (end-of-line)
                          (or (search-backward-regexp "^[ 	]*[^[ 	]" nil t)
                              (beginning-of-buffer))
                          (point)))
         (category-end
          (save-excursion (end-of-line)
                          (or (search-forward-regexp "^[ 	]*[^[ 	]" nil t)
                              (end-of-buffer))
                          (beginning-of-line)
                          (point)))
         (region-text (buffer-substring-no-properties category-start category-end)))

    ;; Delete the contents of the current category
    (delete-region category-start category-end)

    ;; Navigate to where to put the category
    (ignore-error t
      (if direction
          (progn (beginning-of-line)
                 (search-backward-regexp "^[ 	]*[^[ 	]" nil t))
        (next-line)
        (unless (search-forward-regexp "^[ 	]*[^[ 	]" nil t)
          (end-of-buffer))
        (beginning-of-line)))
    
    ;; Insert the original category contents
    (let ((insert-point (point)))
      (insert region-text)

      ;; Jump to the original position of the cursor within the category text
      (goto-char (+ insert-point (- original-point category-start)))))
  (read-only-mode 1)
  (unless dont-write (org-connect-write-link-buffer)))

(defvar org-connect-history nil
  "A list of previous notes which have been viewed in the link view buffer.")

(defvar org-connect-history-position 0
  "The currently viewed item in the view history")

(defun org-connect-clear-history ()
  "Clear the history of viewed files."
  (interactive)
  (if org-connect-viewing-file
      (setq org-connect-history (list org-connect-viewing-file))
    (setq org-connect-history nil))
  (setq org-connect-history-position 0))

(defun org-connect-scroll-history (n)
  "Scroll the history forward/backward by n."
  (unless org-connect-history
    (error "There is no history yet"))
  (when (and (> n 0) (eq org-connect-history-position (1- (length org-connect-history))))
    (error "Already at last item in history"))
  (when (and (< n 0) (eq org-connect-history-position 0))
    (error "Already at first item in history"))
  (setq-local org-connect-history-position
              (max 0 (min (1- (length org-connect-history))
                          (+ n org-connect-history-position))))
  (org-connect-open (nth org-connect-history-position org-connect-history)))

(defun org-connect-history-backward ()
  (interactive)
  (org-connect-scroll-history -1))

(defun org-connect-history-forward ()
  (interactive)
  (org-connect-scroll-history 1))

(defvar org-connect-link-buffer nil
  "The buffer in which to display the list of links.")
(defvar org-connect-note-window nil
  "The window for displaying notes.")
(defvar org-connect-viewing-file nil
  "Which file has their links displayed in the link view buffer")

(defun org-connect-view-links (&optional file-or-buffer)
  "Show the link view buffer for FILE-OR-BUFFER."
  (interactive (list (if (eq (current-buffer) org-connect-link-buffer)
                         org-connect-viewing-buffer
                       (current-buffer))))
  (let ((buf (org-connect-check-file-or-buffer file-or-buffer t)))
    (org-connect-update-link-view buf)

    ;; If the link view buffer isn't showing, create a new window layout
    (unless (get-buffer-window org-connect-link-buffer)
      (delete-other-windows)
      (switch-to-buffer org-connect-link-buffer))
    (org-connect-open buf)))

(defun org-connect-update-link-view (file-or-buffer)
  "Background function for displaying the contents of FILE-OR-BUFFER
in the link view buffer. For the user level function, use org-connect-view-links"
  ;; Since this is a background function, reload the same window layout when finished
  (save-window-excursion
    ;; Make sure file-or-buffer is a valid file or buffer, and update org-connect-viewing-file
    (setq org-connect-viewing-file (org-connect-check-file-or-buffer file-or-buffer))

    ;; Parse the text before opening the link buffer, for when file-or-buffer is nil
    (let ((parsed-links (org-connect-parse-links org-connect-viewing-file)))

      ;; Set up a window to show the links in
      (if (buffer-live-p org-connect-link-buffer)
          (if (get-buffer-window org-connect-link-buffer)
              (select-window (get-buffer-window org-connect-link-buffer))
            (switch-to-buffer org-connect-link-buffer))
        (setq org-connect-link-buffer (generate-new-buffer "*Org Connect*.org"))
        (switch-to-buffer org-connect-link-buffer)
        (org-mode))
      (read-only-mode 0)

      ;; Remove previously existing text
      (delete-region (buffer-end -1) (buffer-end 1))
      (remove-overlays 0 (point-max))
      
      ;; Insert a title
      (insert (concat "#+TITLE: " (org-connect-name-from-file org-connect-viewing-file) "\n"))
      (overlay-put (make-overlay 0 9) 'invisible t)

      ;; Insert the categories and links
      (dolist (i parsed-links)
        (end-of-buffer)
        (when (stringp (car i))
          (insert (concat "* " (car i) "\n")))

        (dolist (j (cdr i))
          (end-of-buffer)
          (insert (concat j "\n")))))
    (read-only-mode 1)
    (message org-connect-viewing-file)
    (message (nth org-connect-history-position org-connect-history))
    (unless (string= (nth org-connect-history-position org-connect-history)
                     org-connect-viewing-file)
      (setq-local org-connect-history (append org-connect-history
                                              (list org-connect-viewing-file)))
      (setq-local org-connect-history-position (1- (length org-connect-history)))))

  ;; Return the link view buffer
  org-connect-link-buffer)

(defun org-connect-write-link-buffer ()
  (interactive)
  ;; Return an error if there is not active link view buffer
  (unless (buffer-live-p org-connect-link-buffer)
    (error "No active link view buffer"))
  
  (with-current-buffer org-connect-link-buffer
    (org-connect-write-links
     (replace-regexp-in-string
      "^\\** +" ""
      (replace-regexp-in-string
       "^#\\+TITLE: .*\n" ""
       (buffer-substring-no-properties (buffer-end -1) (buffer-end 1))))
     org-connect-viewing-file)))

(defvar org-connect-directory-list nil
  "A list of directories to prompt for when changing the
org-connect-current-directory.")

(defvar org-connect-current-directory nil
  "The current directory for browsing notes. This will
be used as the default directory to add new notes in.")

(defvar org-connect-add-description t
  "If non-nil, prompt for a description when adding a new note.")

(defvar org-connect-add-date nil
  "If non-nil, insert the date when adding a new note.")

(defun org-connect-add-note (directory name &optional description)
  (interactive 
   (list
    (read-directory-name "Add note in directory: "
                         (when (and (stringp org-connect-current-directory)
                                    (f-directory-p org-connect-current-directory))
                           org-connect-current-directory))
    (read-from-minibuffer "Note name: ")
    (if org-connect-add-description
        (read-from-minibuffer "Description (optional): "))))

  ;; Check to see if the user input a file name or a note name, then parse
  ;; whichever one they didn't input using the naming convention
  (let ((file-name name) (note-name name) (file-path nil))
    (if (string-equal (file-name-extension name) "org")
        (setq note-name (org-connect-name-from-file name))
      (setq file-name (concat (org-connect-file-from-name name) ".org")))
    (setq file-path (concat (directory-file-name directory) "/" file-name))
    (make-empty-file file-path t)
    
    ;; If the current buffer is the link buffer, open the new note in
    ;; a different window
    (when (eq (current-buffer) org-connect-link-buffer)
      (if (and (get-file-buffer org-connect-viewing-file)
               (get-buffer-window (get-file-buffer org-connect-viewing-file)))
          (select-window (get-buffer-window (get-file-buffer org-connect-viewing-file)))
        (other-window)))
    
    ;; Open the new note and insert some header text
    (find-file file-path)
    (insert (concat "#+TITLE: " note-name "\n"))
    (when org-connect-add-date
      (insert (concat "#+DATE: " (current-time-string) "\n")))
    (when (and description (not (string-match-p "^[ 	]*$" description)))
      (insert (concat "#+DESCRIPTION: " description "\n")))
    (insert ":LINKS:\n:END:")
    (save-buffer)
    (org-mode-restart)
    (org-connect-view-links (current-buffer))
    (select-window (get-buffer-window (get-file-buffer org-connect-viewing-file)))))

(setq org-follow-link-hook nil)
(add-hook 'org-follow-link-hook 'org-connect-follow-link-hook)

(defun org-connect-follow-link-hook ()
  "Function to be run after following a link using org-open-at-point"
  (let ((new-buffer (current-buffer))
        (new-file (buffer-file-name (current-buffer))))
    (set-window-configuration org-window-config-before-follow-link)
    (redraw-display)
    (if (file-regular-p new-file)
        (org-connect-open new-buffer)
      (kill-buffer new-buffer)
      (org-connect-open new-file))))

(defun org-connect-open (file-or-buffer)
  ;; Open the buffer in the correct window
  (let ((buf (org-connect-check-file-or-buffer file-or-buffer t))
        (original-window (selected-window)))
    (set-window-buffer
     (if (eq (current-buffer) org-connect-link-buffer)
         (cond ((eq (count-windows) 1) (split-window-right))
               ((and (get-file-buffer org-connect-viewing-file)
                     (get-buffer-window (get-file-buffer org-connect-viewing-file))))
               ((other-window 1)))
       (selected-window))
     buf)
    (ignore-errors (org-connect-update-link-view buf))
    (select-window original-window)))

(defun org-connect-check-file-or-buffer (file-or-buffer &optional return-buffer)
  "Check if FILE-OR-BUFFER is a valid file or buffer, and return
the file associated with it. If FILE-OR-BUFFER is nil, use the
current buffer.
If RETURN-BUFFER is non-nil, create and return the buffer instead."
  (let ((buf nil) (file nil))
    (if (or (not file-or-buffer) (buffer-live-p file-or-buffer))
        (if file-or-buffer
            (setq buf file-or-buffer)
          (setq buf (current-buffer)))

      (if (stringp file-or-buffer)
          (progn
            (when (file-directory-p file-or-buffer)
              (error "File is a directory: %s" file-or-buffer))
            (unless (file-regular-p file-or-buffer)
              (if (y-or-n-p (format "File %s doesn't exist. Create it as a new note?"
                                    file-or-buffer))
                  (save-window-excursion
                    (org-connect-add-note (file-name-directory file-or-buffer)
                                          (concat (file-name-base file-or-buffer) ".org")))
                (error "Not a valid buffer or file path: %s" file-or-buffer)))
            (setq file file-or-buffer))
        (error "Not a valid buffer or file path: %s" file-or-buffer)))

    (if return-buffer
        (or buf
            (save-window-excursion
              (find-file file-or-buffer)
              (current-buffer)))
      (expand-file-name (or file (buffer-file-name buf))))))

(defvar org-connect-showing-separator nil
  "If non-nil, display a horizontal line after the document info.")
(setq-default org-connect-showing-separator nil)

(defvar org-connect-separator-height 0.8
  "A number which determines the height of the face used for the strikethrough.
An integer specifies how many pixels high, and a float represents the height compared
to the surrounding text.")

(defun org-connect-show-separator (&optional state)
  "If STATE is positive, show the separator line.
If STATE is negative, hide the separator line.
If STATE is 0, do not make any change, but make sure
that the separator is being displayed/hidden properly.
Otherwise, toggle the separator line."
  (interactive)

  ;; Set showing the separator to off if it isn't already set
  (unless (ignore-errors (or org-connect-showing-separator t))
    (setq-local org-connect-showing-separator nil))

  ;; Change whether or not to show the separator based on the input
  (setq-local org-connect-showing-separator
              (if (numberp state)
                  (if (eq state 0) org-connect-showing-separator
                    (if (< state 0) nil t))
                (not org-connect-showing-separator)))
  
  ;; Delete the old overlays
  (remove-overlays nil nil 'org-connect-separator t)

  ;; Create an overlay to hide the link drawer, and one to show the line
  (when org-connect-showing-separator
    (save-excursion
      (beginning-of-buffer)
      ;; Strikethrough underline
      (let ((overlay-start
             (if (not (search-forward-regexp "^[ 	]*:LINKS:[ 	]*$" nil t))
                 (error "The separator cannot be drawn without a link drawer.")
               (beginning-of-line)
               (point)))
            (overlay-end 
             (if (not (search-forward-regexp "^[ 	]*:END:[ 	]*$" nil t))
                 (error "The separator cannot be drawn without a link drawer.")
               (end-of-line)
               (point))))

        ;; If the drawer is collapsed, expand it, otherwise it will cause issues
        (remove-overlays overlay-start overlay-end )

        (let ((overlays (list (make-overlay overlay-start overlay-end)
                              (make-overlay overlay-end (1+ overlay-end))
                              (make-overlay overlay-end (1+ overlay-end)))))
          
          ;; Make the link drawer invisible
          (overlay-put (nth 0 overlays) 'org-connect-separator t)
          (overlay-put (nth 0 overlays) 'invisible t)
          (overlay-put (nth 0 overlays)
                       'face `(:height ,org-connect-separator-height))
          
          ;; Make the newline character and the space after it strike-through
          (overlay-put (nth 1 overlays) 'org-connect-separator t)
          (overlay-put (nth 1 overlays)
                       'face `(:height ,org-connect-separator-height
                                       :strike-through t :extend t))

          ;; Make a second overlay on the newline character to make the newline character itself
          ;; not strikethrough, since the strikethrough of actual text and of the space after the
          ;; line don't line up for some reason.
          (overlay-put (nth 2 overlays) 'org-connect-separator t)
          (overlay-put (nth 2 overlays)
                       'face `(:height ,org-connect-separator-height
                                       :strike-through nil :extend nil)))))))

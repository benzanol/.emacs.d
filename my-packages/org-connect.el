;;; Link Manager
;;;; Keybindings
(define-key org-mode-map (kbd "C-d C-h") 'org-connect-history-backward)
(define-key org-mode-map (kbd "C-d C-l") 'org-connect-history-forward)
(define-key org-mode-map (kbd "C-d C-a") 'org-connect-add-file-link)
(define-key org-mode-map (kbd "C-d C-n") 'org-connect-add-note)
(define-key org-mode-map (kbd "C-d C-d") 'org-connect-remove-current-link)
(define-key org-mode-map (kbd "C-d C-S-d") '(org-connect-remove-current-link t))
(define-key org-mode-map (kbd "C-d C-f") 'org-connect-open-file)
(define-key org-mode-map (kbd "C-d C-b") 'org-connect-open-buffer)
(define-key org-mode-map (kbd "C-d C-j") 'org-connect-move-down)
(define-key org-mode-map (kbd "C-d C-k") 'org-connect-move-up)

;;;; Switching Files
;;;;; Opening Files
(defun org-connect-open-file (file)
  (interactive "fOpen File: ")
  (org-connect-open file))

;;;;; Opening Buffers
(defun org-connect-open-buffer (buffer)
  (interactive "bOpen Buffer: ")
  (org-connect-open buffer))

;;;; List of Links
(defcustom org-connect-links nil
  "An alist of links for the current buffer of the form returned by org-connect--parse-links."
  :local t)

;;;; Regexps
(defconst org-connect-link-drawer-regexp
  (concat "\\([^z-a]*?\\(?:^[ 	]*:LINKS:[ 	]*$\\)\\)\n"
          "\\([^z-a]*?\\)\n?"
          "\\(\\(?:^[ 	]*:END:[ 	]*$\\)[^z-a]*\\)$")
  "The regexp used to match the drawer for storing links in a note.")
(defconst org-connect-document-info-regexp
  "\\([^z-a]*#\\+[-_a-zA-Z0-9]: .*\n\\)\\([^z-a]*\\)$"
  "The regexp used to match the beginning and end of the document info")

;;;; Naming
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

;;;; Parsing Links
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

;;;; Writing Links
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

;;;; Adding Links
;;;;; User Defined Categories
(defvar org-connect-preset-categories
  '("Children"
    "Parents"
    "Documentation"
    "Resources"
    "Reference")
  "A list of categories to provide completion for when adding a link.")

;;;;; Adding File Links
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

;;;; Removing Links
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

;;;; Moving Links
;;;;; Settings
(defvar org-connect-dont-write-moves nil
  "If non-nil, dont write the updated link order to file after running
the org-connect-move or org-connect-move-up/down functions")

;;;;; User Level Commands
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

;;;;; Moving Single Lines
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

;;;;; Moving Categories
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

;;;; Link View Buffer
;;;;; Link View History
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

;;;;; Variables
(defvar org-connect-link-buffer nil
  "The buffer in which to display the list of links.")
(defvar org-connect-note-window nil
  "The window for displaying notes.")
(defvar org-connect-viewing-file nil
  "Which file has their links displayed in the link view buffer")

;;;;; Displaying Links
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

;;;;; Write View Buffer Contents
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

;;;; Adding Notes
;;;;; Variables
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

;;;;; Adding a Note
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

;;;; Following Links
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

;;;; Check File or Buffer
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

;;;; Separating Line
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

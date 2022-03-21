(qv/package org-agenda)

;;; Restore Previous Layout
(defun qv/org-agenda (&rest args)
  (interactive)
  (save-window-excursion
    (apply 'org-agenda args))
  (switch-to-buffer "*Org Agenda*"))

;;; Keybindings
;; Setup
(qv/hook org-agenda-mode-hook qv/org-agenda-setup
  (display-line-numbers-mode 0)
  (qv/keys org-agenda-keymap
    :sparse t
    "t" (org-agenda-todo "TODO")
    "f" (org-agenda-todo "DONE")
    "x" (org-agenda-todo "TODAY")
    "X" (org-agenda-todo "NEXT")
    "r" (@ qv/reload-agenda
           (let ((pos-before (point)))
             (org-agenda nil "e")
             (goto-char pos-before)))
    "u" org-agenda-undo
    "U" org-agenda-redo
    "c" qv/agenda-capture))

;; Global Keybindings
(qv/keys *
  "C-x C-a C-a" (qv/org-agenda nil "e")
  "C-x C-a C-l" (qv/org-agenda nil "a")
  "C-x C-a C-m" qv/org-agenda
  "C-x C-a C-c" qv/agenda-capture)

;;; Faces
(qv/face org-agenda-structure :w bold :h 1.2)
(qv/face org-agenda-date line-number)
(qv/face org-agenda-date-today org-agenda-date :fg "#828292"
         :w bold :s italic :u t)
(qv/face org-time-grid :fg gray2)

(qv/face org-scheduled-previously red)
(qv/face org-scheduled-today yellow)
(qv/face org-agenda-done green)
(qv/face org-scheduled nil)

;; Format newlines to be smaller
(defun qv/org-agenda-shrink-newlines ()
  (save-excursion
    (beginning-of-buffer)
    (while (and (search-forward-regexp "^$" nil t) (not (eobp)))
      (put-text-property (point) (1+ (point)) 'face '(:height 0.6))
      (next-line 1))))
(add-hook 'org-agenda-finalize-hook 'qv/org-agenda-shrink-newlines)

;;; Prefix Format
(setq qv/agenda-icons
      '(("Tasks" "" "")
        ("Routine" #(" " 0 2 (display (height 0.9))) " ")
        ("Events" " " " ")
        ("Asap" " " " ")
        ("Habits" " " " ")
        (today " " " ")))
(defun qv/org-agenda-prefix-format ()
  (let* ((category (org-agenda-get-category))
         (category (if (string= category (file-name-base (qv/agenda-today)))
                       'today category))
         (state (ignore-errors (org-get-todo-state)))
         (icon (funcall (if (string= state "DONE") 'cadr 'car)
                        (cdr (assoc category qv/agenda-icons))))
         (time-string (replace-regexp-in-string " " "" time))
         (time-string (if (or (string= time "") (string= category "Habits")) " "
                        (propertize (format " ⟨%s⟩ " time-string) 'display '(height 0.8))))
         (title-lines (split-string txt "--"))
         (title-string (replace-regexp-in-string
                        "TODO \\|TODAY \\|NEXT \\|DONE " ""
                        (concat (propertize (car title-lines) 'display '(height 1.0))
                                (propertize "" 'display '(raise 0.0)))))
         (todo-keyword (car (split-string txt " ")))
         (todo-prefix (cl-case (intern todo-keyword)
                        ((NEXT) '#(" ✸ " 0 1 (display (height 0.6)) 2 3 (display (height 0.6))))
                        ((TODAY) '#(" ✭ " 0 1 (display (height 0.6)) 2 3 (display (height 0.6))))
                        (t " » ")))
         (extra-string ""))

    ;; Add extra lines with bullets to the format string
    (dotimes (i (length (cdr title-lines)))
      (setq extra-string
            (concat extra-string
                    (propertize "\n" 'display '(height 0.5))
                    (propertize (concat " ➢ " (nth i (cdr title-lines)))
                                'display
                                (if (eq i (1- (length (cdr title-lines))))
                                    '((height 0.8) (raise 0.3)) '(height 0.8))))))
    (setq extra-string (propertize extra-string 'extra-line t))

    (setq txt "")
    ;;(format "%s »%s%s%s" icon title-string time-string extra-string)
    ;;(format "%s%s»%s%s" icon time-string title-string extra-string)
    (concat icon todo-prefix title-string time-string extra-string)))
(setq org-agenda-prefix-format '((todo . "%(qv/org-agenda-prefix-format)")
                                 (agenda . "%(qv/org-agenda-prefix-format)")))

;;; Outline Mode
;; For folding the bullets below a particular event
(defun qv/agenda-outline-level ()
  (if (eq (get-text-property (line-beginning-position) 'face) 'org-agenda-structure) 4
    (let* ((line-text (buffer-substring (line-beginning-position) (line-end-position)))
           (match-pos (string-match " ➢ " line-text)))
      (if match-pos (+ 6 match-pos) 5))))

(add-hook 'org-agenda-finalize-hook 'qv/agenda-outline-setup)
(defun qv/agenda-outline-setup ()
  (outline-minor-mode 1)
  (setq-local outline-regexp "")
  (setq-local outline-level #'qv/agenda-outline-level)
  (outline-hide-sublevels 5))

;;; Agenda Settings
(setq org-deadline-past-days 0)
(setq org-scheduled-past-days 0)
(setq org-agenda-past-days 0)
(setq org-deadline-warning-days 0)
(setq org-agenda-start-day nil)
(setq org-agenda-span 'week)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-skip-scheduled-if-done nil)

(setq org-agenda-max-entries 2)
(setq org-agenda-todo-keyword-format "")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-show-all-dates nil)
(setq org-agenda-format-date "%A, %B %d")
(setq org-agenda-block-separator nil)
(setq org-agenda-ignore-properties nil)
(setq org-agenda-move-date-from-past-immediately-to-today nil)
(setq org-agenda-overriding-header "")

(setq org-todo-keywords '((sequence "TODO" "TODAY" "NEXT" "|" "DONE")))

(setq org-agenda-use-time-grid nil)
(setq org-agenda-time-grid
      `((today) (800 1000 1200 1400 1600 1800 2000) ""
        ,(propertize (make-string 40 ?•) 'display '(height 0.7))))
(setq org-agenda-current-time-string "──────────────────────────")

;;; Agenda Files
(setq qv/org-agenda-dir (expand-file-name "~/Notes/Schedule"))
(defun qv/org-agenda-files (&rest files)
  (mapcar (lambda (file-name) (format "%s/%s.org" qv/org-agenda-dir file-name)) files))

(defun qv/agenda-today ()
  (let ((today-file (format "%s/Daily/%s.org" qv/org-agenda-dir
                            (format-time-string "%Y-%m-%d" (current-time)))))
    (unless (file-regular-p today-file)
      (make-empty-file today-file))
    today-file))

(setq org-agenda-files (qv/org-agenda-files "Events" "Tasks"))

;;; Captures
(defun qv/agenda-capture ()
  (interactive)
  (let* ((char (read-char "[E]vent | [T]ask | [A]sap | toda[Y]"))
         (file (cl-case char
                 ((?e) (car (qv/org-agenda-files "Events")))
                 ((?t ?y) (car (qv/org-agenda-files "Tasks")))
                 ((?a) (car (qv/org-agenda-files "Asap")))))
         (name (read-string "Name: "))
         (todo-keyword (cl-case char
                         ((?y) "TODAY")
                         (t "TODO")))
         time-stamp time-string event-string)
    (unwind-protect
        (setq time-stamp (cl-case char
                           ((?y) (format-time-string "%Y-%m-%d %a"))
                           ((?e ?t) (org-read-date))))
      (when time-stamp (setq time-string (format "\nSCHEDULED: <%s>" time-stamp)))
      (setq event-string
            (format "* %s %s%s\n\n" todo-keyword name (or time-string "")))
      (let ((buffer (or (get-file-buffer file) (create-file-buffer file))))
        (with-current-buffer buffer
          (save-excursion (end-of-buffer) (insert event-string))
          (save-buffer)))
      (when (string= (buffer-name) "*Org Agenda*")
        (qv/reload-agenda)))))

;;; Habits
(add-to-list 'org-modules 'org-habit)
(setq org-habit-following-days 0
      org-habit-preceding-days 21)

(defface qv/org-habit '((t :height 0.8))
  "Face for other org habit faces to inherit from")
(qv/face qv/org-habit fixed-pitch :h 1.0 :iv t)

;;;; Default Habit System Faces
(qv/face org-habit-clear-face qv/org-habit :fg "DeepSkyBlue")
(qv/face org-habit-clear-future-face qv/org-habit :fg "DeepSkyBlue")
(qv/face org-habit-alert-future-face qv/org-habit :fg "orange")
(qv/face org-habit-overdue-face qv/org-habit :fg "OrangeRed")
(qv/face org-habit-overdue-future-face :fg qv/org-habit "OrangeRed")
(qv/face org-habit-ready-face qv/org-habit :fg "GreenYellow")
(qv/face org-habit-ready-future-face qv/org-habit :fg "LawnGreen")

;;;; Custom Habit Settings
(defface qv/habit-completed '((t :inherit (qv/org-habit org-agenda-done)))
  "Custom face for habits that are completed")
(defface qv/habit-error '((t :inherit qv/org-habit :foreground "OrangeRed"))
  "Custom face for habits that are error")
(defface qv/habit-warning '((t :inherit qv/org-habit :foreground "Yellow"))
  "Custom face for habits that are warning")
(defface qv/habit-late '((t :inherit qv/org-habit :foreground "Orange"))
  "Custom face for habits that are late")
(defface qv/habit-clear '((t :inherit qv/org-habit :foreground "DeepSkyBlue"))
  "Custom face for habits that are clear")

(setq org-habit-graph-column 22)

;; ▁▂▃▄▅▆▇▓▒▣▢□▨▤▧▩▦⊟■◆◈▴⊠⊞
(progn (setq org-habit-completed-string "▅"
             org-habit-filler-string "▃"
             org-habit-today-string "▁")
       (qv/face qv/org-habit fixed-pitch :h 1.1))

(progn (setq org-habit-completed-string "▣"
             org-habit-filler-string "□"
             org-habit-today-string "▨")
       (qv/face qv/org-habit fixed-pitch :h 1.2))

;;;; Custom Habit Graph Function
(setq qv/displayed-habits nil)
(add-hook 'org-agenda-mode-hook (lambda () (setq qv/displayed-habits nil)))

(defun org-habit-build-graph (habit starting current ending)
  (if (memq habit qv/displayed-habits) ""
    (push habit qv/displayed-habits)
    (let* ((completed-dates (org-habit-done-dates habit))
           (start (time-to-days starting))
           (repeat (org-habit-scheduled-repeat habit))
           (habit-start (apply 'min (or completed-dates '(-1))))
           (now (time-to-days current))
           (end (time-to-days ending))
           (last-completed -1)
           (graph "")
           (space (propertize " " 'display `(space :align-to ,org-habit-graph-column)))
           completed status face string)
      (dolist (date (number-sequence start end))
        (setq completed (memq date completed-dates)
              last-completed (if completed date last-completed)
              status (if (eq last-completed -1) 'clear
                       (if (>= (- date last-completed) repeat)
                           (if (= date (org-today)) 'warning
                             (if (= (- date last-completed) repeat)
                                 'late 'error))
                         'completed))
              face (intern (format "qv/habit-%s" status))
              string (if completed org-habit-completed-string
                       (if (eq date now) org-habit-today-string
                         org-habit-filler-string))
              graph (concat graph (propertize string 'face face))))
      (concat space graph))))

;;;; Don't insert on bullet lines
(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t)
	    (buffer-invisibility-spec '(org-link))
	    (moment (org-time-subtract nil (* 3600 org-extend-today-until))))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	    (let ((habit (get-text-property (point) 'org-habit-p))
              (extra-line (get-text-property (point) 'extra-line)))
	      (when (and habit (not extra-line))
	        (move-to-column org-habit-graph-column t)
	        (delete-char (min (+ 1 org-habit-preceding-days
				                 org-habit-following-days)
			                  (- (line-end-position) (point))))
	        (insert-before-markers
	         (org-habit-build-graph
	          habit
	          (time-subtract moment (days-to-time org-habit-preceding-days))
	          moment
	          (time-add moment (days-to-time org-habit-following-days))))))
	    (forward-line)))))

;;; Highlight One Time Events
(qv/hook org-agenda-finalize-hook qv/highlight-one-time-events
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "^Upcoming$" nil t)
    (search-forward-regexp
     (format "^%s$" (regexp-quote (format-time-string
                                   org-agenda-format-date (current-time))))
     nil t)
    (search-forward-regexp "^[MTWFS]" nil t)
    (while (search-forward-regexp "^" nil t)
      (put-text-property (line-beginning-position)
                         (1- (search-forward-regexp "^[^ ]" nil t))
                         'face `(:foreground ,qv/blue-color)))))

;;; Custom Agenda View
(setq qv/org-agenda-spacer
      '(agenda "Space" ((org-agenda-files nil)
                        (org-agenda-span 'day)
                        (org-agenda-start-day nil)
                        (org-agenda-format-date "")
                        (org-agenda-show-all-dates t))))

(setq org-agenda-custom-commands
      `(("e" "Multi Agenda View"
         ;; Today's schedule
         ((agenda "" ((org-agenda-files (qv/org-agenda-files "Events" "Routine"))
                      (org-agenda-day-face-function
                       (lambda (day) 'org-agenda-structure))
                      (org-agenda-use-time-grid t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 'day)
                      (org-agenda-show-all-dates t)
                      (org-agenda-prefix-format
                       '((agenda . "  %?-6t")))
                      ))

          ;; Today
          ,qv/org-agenda-spacer
          (todo "NEXT" ((org-agenda-files (qv/org-agenda-files "Tasks" "Asap" "Habits"))))
          (todo "TODAY" ((org-agenda-files (qv/org-agenda-files "Tasks" "Asap" "Habits"))))

          ;; Habits
          ,qv/org-agenda-spacer
          ;; This has to be an agenda to get highlighting for completed and uncompleted today
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Habits"))
                      (org-agenda-day-face-function (lambda (day) 'org-agenda-structure))
                      (org-agenda-format-date "Habits")
                      (org-agenda-sorting-strategy '((agenda category-keep)))
                      (org-agenda-span 'day)))

          ;; Tasks
          ,qv/org-agenda-spacer
          (todo "" ((org-agenda-files (qv/org-agenda-files "Asap"))
                    (org-agenda-overriding-header "Tasks")))
          ,qv/org-agenda-spacer
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Tasks"))
                      (org-agenda-start-day "-100d")
                      (org-agenda-span 100)
                      (org-agenda-skip-scheduled-if-done t)))
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Tasks"))
                      (org-agenda-show-all-dates t)
                      (org-agenda-start-day nil)
                      (org-agenda-span 'day)))
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Tasks"))
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 'month)
                      (org-agenda-max-entries 10)))


          ;; Future events
          ,qv/org-agenda-spacer
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Events" "Routine"))
                      (org-agenda-overriding-header "Upcoming")
                      (org-agenda-start-day "-100d")
                      (org-agenda-span 100)
                      (org-agenda-skip-scheduled-if-done t)))
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Events" "Routine"))
                      (org-agenda-show-all-dates t)
                      (org-agenda-start-day nil)
                      (org-agenda-span 'day)))
          (agenda "" ((org-agenda-files (qv/org-agenda-files "Events" "Routine"))
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 'month)
                      (org-agenda-max-entries 10)))
          ))))

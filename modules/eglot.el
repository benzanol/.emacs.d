;; -*- lexical-binding: t; -*-

(require 'bz-base)
(require 'bz-flycheck)
(require 'bz-functions)

(require 'eglot)
(require 'flymake)
(require 'flymake-popon)
(require 'jsonrpc)
(require 's)


;;; Configuration

(bz/hook eglot-managed-mode-hook bz/eglot-setup
  (when (eglot-managed-p)
    (company-mode 1)
    (flymake-popon-mode 1)

    ;; No type annotations displayed inline
    (eglot-inlay-hints-mode 0)
    ;; (eldoc-mode 0)

    (setq eldoc-echo-area-use-multiline-p nil)))

;; Stupid hints won't go away
(bz/advise :override eglot--update-hints bz/disable-inlay-hints (&rest _args)
  (when eglot-inlay-hints-mode (eglot-inlay-hints-mode 0)))

(bz/advise :override doom-modeline-update-eglot ignore)


;;; Flymake

(setq flymake-popon-diagnostic-formatter
      (defun bz/flymake-popon-formatter (diag)
        (setq diag (copy-flymake--diag diag))
        (mapconcat
         (lambda (line)
           (setf (flymake--diag-text diag) (truncate-string-to-width line 200))
           (replace-regexp-in-string "[\n\r ]+" " " (flymake-popon-format-diagnostic diag)))
         (split-string (flymake-diagnostic-text diag) "[\n\r]")
         "\n")))

(bz/face flymake-note :u "yellow green")
(bz/face flymake-error :u "Red1")
(bz/face flymake-warning :u "DarkOrange2")

(bz/face flymake-note-echo :fg "YellowGreen" :w normal :s italic)
(bz/face flymake-error-echo :fg "#ff5454" :w normal :s italic)
(bz/face flymake-error-warning :fg "DarkOrange2" :w normal :s italic)

(bz/face eglot-diagnostic-tag-unnecessary-face flymake-note)

(setq flymake-popon-delay 99999999999999999999999)
(setq flymake-popon-delay 0.1)
(setq flymake-popon-posframe-extra-arguments
      (list
       ;; :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
       :poshandler #'posframe-poshandler-bz/window-bottom-right-corner
       :background-color (bz/color bg2)
       :border-color (bz/color gray3)
       :border-width 1
       :left-fringe 8
       :right-fringe 8
       :height 10
       :width 110
       ))
(setq flymake-popon-posframe-border-width 0)
(setq flymake-popon-width 110)


;;; Keybinds

(bz/keys eglot-mode-map
  :sparse t
  ;; Diagnostics (eglot uses flymake by default)
  "C-c C-e" (@ bz/flymake-copy-error
               (let ((diagnostics (flymake-diagnostics (point))))
                 (if diagnostics
                     (let ((msg (mapconcat
                                 (lambda (d) (flymake-diagnostic-text d))
                                 diagnostics
                                 "\n")))
                       (kill-new msg)
                       (message "Copied: %s" msg))
                   (message "No Flymake diagnostic at point."))))

  [remap bz/Q]
  (@ bz/flymake-show-error (unless flymake-popon-mode (flymake-popon-mode 1)) (flymake-popon--show))

  "C-c C-b" (@ bz/eglot-refresh-buffer (eglot--signal-textDocument/didSave))
  "C-j" flymake-goto-next-error
  "C-k" flymake-goto-prev-error

  ;; Save and kill all unsaved buffers (useful after a big rename which opened a ton of buffers)
  "C-c C-s"
  (@ bz/eglot-save-all
     (let* ((root (or (project-root (eglot--project (eglot-current-server)))
                      (error "No project root"))))
       (when buffer-file-name (save-buffer))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (and buffer-file-name (file-in-directory-p buffer-file-name root)
                (buffer-modified-p)
                (progn (save-buffer) (kill-buffer buf)))))))
  ;; Workspace
  "C-c C-w C-s" eglot-shutdown
  "C-c C-w C-r" eglot-reconnect

  ;; LSP actions
  "C-c C-r" eglot-rename
  "C-c C-d" xref-find-definitions
  "C-c C-f" xref-find-references
  "C-c C-l" flymake-show-buffer-diagnostics
  "C-c C-a" eglot-code-actions

  "C-c C-y" (@ bz/copy-flymake-diagnostic arg
               (cl-loop for diag in (flymake-diagnostics (unless arg (point)))
                        collect (save-excursion
                                  (goto-char (flymake-diagnostic-beg diag))
                                  (format "[%s:%s] %s" (line-number-at-pos) (current-column)
                                          (flymake-diagnostic-text diag)))
                        into strs
                        finally do (kill-new (string-join strs "\n"))))

  ;; Fix all diagnostics
  "C-c C-c" (@ bz/eglot-act-all
               (save-excursion
                 (goto-char (point-min))
                 (while (ignore-errors (flymake-goto-next-error) t)
                   (bz/eglot-act)
                   (ignore-errors (forward-char -1)))))

  ;; Documentation popup (using eldoc)
  [remap bz/q] bz/eldoc-posframe-toggle


  "C-c C-x"
  (@ bz/eglot-act
     (bz/eglot-attempt-autofix
      (lambda (diag)
        (let ((range (plist-get diag :range)))
          (and (bz/eglot-diag-position-is-before-point (plist-get range :start))
               (not (bz/eglot-diag-position-is-before-point (plist-get range :end))))))
      (lambda (_fix) t)))

  ;; Organize imports
  "C-c C-u"
  (@ bz/eglot-fix-unused-imports
     (bz/eglot-attempt-autofix
      (lambda (diag) (equal "unused_imports" (plist-get diag :code)))
      (lambda (fix) (member (plist-get fix :title)
                            '("Remove all unused imports" "remove unnecessary import")))
      0.5))
  "C-c C-i"
  (@ bz/eglot-fix-missing-imports
     (bz/eglot-attempt-autofix
      (lambda (diag)
        (string-match-p "^\\(consider importing\\|cannot find name\\)"
                        (downcase (plist-get diag :message))))
      (lambda (fix)
        (string-match-p "^\\(consider importing\\|update import\\|add import\\)" (downcase (plist-get fix :title))))
      0.5)))


;;; Autofixes

(defun bz/eglot-diag-position-is-before-point (diag-pos)
  (let ((pl (line-number-at-pos))
        (dl (1+ (plist-get diag-pos :line))) ; Line is 0-indexed
        (pc (current-column))
        (dc (plist-get diag-pos :character)))
    (or (< dl pl) (and (eq dl pl) (<= dc pc)))))

(defun bz/eglot-attempt-autofix (diag-pred fix-pred &optional repeat-delay)
  (bz/save-position
   (let* ((server (eglot--current-server-or-lose))
          (diags (--filter (let* ((data (eglot--diag-data it))
                                  (diag (cdr (assoc 'eglot-lsp-diag data))))
                             (funcall diag-pred diag))
                           (flymake-diagnostics))))
     (if (null diags)
         (message "No matching diagnostic detected.")
       (when (and (bz/eglot-autofix-first-diag server diags fix-pred)
                  repeat-delay)
         (run-with-timer
          repeat-delay nil
          (lambda () (bz/eglot-attempt-autofix diag-pred fix-pred repeat-delay))))))))

(defun bz/eglot-autofix-first-diag (server diags fix-pred)
  "Perform an autofix for the first diagnostic in DIAGS with a valid fix.

Returns t if any fix was applied."
  (when diags
    (let* ((diag (car diags))
           (beg (flymake-diagnostic-beg diag))
           (end (flymake-diagnostic-end diag))
           (_ (goto-char beg))
           (actions (jsonrpc-request
                     server
                     :textDocument/codeAction
                     (list :textDocument (eglot--TextDocumentIdentifier)
                           :range (list :start (eglot--pos-to-lsp-position beg)
                                        :end (eglot--pos-to-lsp-position end))
                           :context `(:diagnostics
                                      [,(cdr (assoc 'eglot-lsp-diag
                                                    (eglot--diag-data diag)))]
                                      :only ["quickfix"]))
                     :deferred t))
           (fix (cl-find-if fix-pred actions)))
      (if (null fix)
          ;; If this one could not be fixed, try the next one
          (bz/eglot-autofix-first-diag server (cdr diags) fix-pred)
        (ignore-errors (eglot-execute server fix))
        t))))


;;; Request with callback

;; Send a save request, and then when the new diagnostics come in for
;; THAT SAVE (not some arbitrary modification), run a callback

(defun bz/eglot-save-and-wait-for-diagnostics (callback)
  "Send didSave, then run CALLBACK when all requests are finished.

Specifically, the callback is run when a diagnostics request is recieved
whose version matches the current version of the document."
  (let* ((server (eglot-current-server))
         (buffer (current-buffer))
         (file (or buffer-file-name (error "Not in a file")))
         ;; Grab the version eglot is currently tracking for this buffer
         (old-version (plist-get (eglot--VersionedTextDocumentIdentifier) :version))
         (advice-sym (make-symbol "bz/diag-advice")))

    ;; Save the buffer — this triggers eglot's didSave notification
    (save-buffer)
    (eglot--signal-textDocument/didSave)

    ;; Add a one-shot advice on the jsonrpc receive handler
    (advice-add
     #'jsonrpc-connection-receive
     :after
     (lambda (conn message)
       (when-let*
           ((_ (eq conn server))
            (_ (equal (plist-get message :method) "textDocument/publishDiagnostics"))
            (params (plist-get message :params))
            (incoming-version (plist-get params :version))
            (_ (equal (plist-get params :uri) (eglot-path-to-uri file)))
            (_ (> incoming-version old-version))
            (_ (eq incoming-version
                   (with-current-buffer buffer
                     (plist-get (eglot--VersionedTextDocumentIdentifier) :version)))))
         ;; Remove ourselves immediately — one-shot
         (advice-remove #'jsonrpc-connection-receive advice-sym)
         ;; Run after eglot has actually processed the notification,
         ;; so flymake state is updated
         (run-at-time 0.1 nil
                      (lambda ()
                        (with-current-buffer buffer
                          (funcall callback))))))
     `((name . ,advice-sym)))))


;;; Posframe

(dolist (f (frame-list))
  (when (frame-parameter f 'posframe-buffer)
    (delete-frame f)))

(defvar bz/eldoc-posframe-buffer " *eldoc-posframe*")

(bz/keys bz/eldoc-posframe-focused-map
  :doc "Keymap for when the eglot posframe is focused."
  :sparse t
  "q" bz/eldoc-posframe-hide
  [remap bz/q] bz/eldoc-posframe-hide)

(defun bz/eldoc-posframe-show ()
  "Show eldoc documentation in a posframe."
  (interactive)
  (let ((content (with-current-buffer (eldoc-doc-buffer) (buffer-string))))
    (if (string-empty-p (string-trim content))
        (message "No documentation available")
      (let ((frame
             (posframe-show
              bz/eldoc-posframe-buffer
              :string content
              :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
              :border-width 1
              :border-color (face-foreground 'shadow nil t)
              :background-color (bz/color bg2)
              :left-fringe 8
              :right-fringe 8
              :max-width 80
              :max-height 20)))
        (select-frame frame)
        (setq truncate-lines t)
        (use-local-map bz/eldoc-posframe-focused-map)))))

(defun bz/eldoc-posframe-hide ()
  "Hide the eldoc posframe."
  (interactive)
  (let ((buffer-or-name bz/eldoc-posframe-buffer)
        (success nil))
    (dolist (frame (frame-list))
      (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
        (when (and (or (equal buffer-or-name (car buffer-info))
                       (equal buffer-or-name (cdr buffer-info)))
                   (frame-visible-p frame))
          (setq success t)
          (when (eq frame (selected-frame))
            (select-frame-set-input-focus (frame-parameter nil 'parent-frame)))
          (posframe--make-frame-invisible frame))))
    success))

(defun bz/eldoc-posframe-toggle ()
  "Toggle eldoc posframe visibility."
  (interactive)
  (unless (bz/eldoc-posframe-hide)
    (bz/eldoc-posframe-show)))


;;; Rename

;; Include the old name in the minibuffer when renaming
(bz/advise :override eglot-rename bz/eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          (thing-at-point 'symbol t) nil nil nil
          (symbol-name (symbol-at-point)))))
  (eglot-server-capable-or-lose :renameProvider)
  (eglot--apply-workspace-edit
   (eglot--request (eglot--current-server-or-lose)
                   :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                          :newName ,newname))
   this-command))


;;; Typescript

(setf
 (alist-get '(web-mode) eglot-server-programs nil nil #'equal)
 '("typescript-language-server" "--stdio"))

;; json mode is derived from js mode so json mode ends up using the js server
(dolist (entry eglot-server-programs)
  (when (and (listp (car entry)) (assq 'js-mode (car entry)))
    (setcar entry (cl-remove 'js-mode (car entry) :key #'car))))


;;; Provide

(provide 'bz-eglot)

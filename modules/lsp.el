(bz/package lsp)
(bz/package lsp-mode)
(bz/package lsp-ui)
(bz/package lsp-dired)
(bz/package dash)
(bz/package subr-x)
(bz/package s)

(bz/require flycheck)
(bz/require company)
(bz/require svelte)
(bz/require jsx)

(bz/require lsp-signature)
(bz/require lsp-popup)
(bz/require lsp-dired)

;;; Execute action by name
(defun bz/lsp-action-by-name (&rest names)
  (let* ((actions (lsp-code-actions-at-point))
         (action (--find (member (gethash "title" it) names) actions)))
    (if action (lsp-execute-code-action action)
      (error "Action%s \"%s\" not found" (if (eq (length names) 1) "" "s") (s-join "\"/\"" names)))))

;;; Override Warn

;; Instead of annoyingly switching to the warning buffer, message the warning
(bz/advise :override lsp-warn bz/message-warning (message &rest args)
  (display-warning 'lsp-mode (apply #'format-message message args) nil "*lsp-log*")
  (message "LSP WARNING: %s ..." (substring message 0 (min 200 (length message) (s-index-of "\n" message)))))

(setq lsp-log-io t)

;;; Header line
(bz/face lsp-headerline-breadcrumb-path-face nil :fg gray1 :w bold)
(bz/face lsp-headerline-breadcrumb-symbols-warning-face
  lsp-headerline-breadcrumb-symbols-face :u (:color "DarkOrange"))
(bz/face lsp-headerline-breadcrumb-path-warning-face
  lsp-headerline-breadcrumb-path-face :u (:color "DarkOrange"))
(bz/face lsp-headerline-breadcrumb-symbols-error-face
  lsp-headerline-breadcrumb-symbols-face :u (:color "Red1"))
(bz/face lsp-headerline-breadcrumb-path-error-face
  lsp-headerline-breadcrumb-path-face :u (:color "Red1"))

;; Otherwise posframes are black
(bz/face markdown-code-face :fg fg)
(bz/face flycheck-posframe-face :fg red)

;;; Don't auto-select action
(bz/advise :override lsp--select-action bz/lsp--select-action-noauto (actions)
  "Select an action to execute from ACTIONS."
  (if (seq-empty-p actions) (signal 'lsp-no-code-actions nil)
    (let ((completion-ignore-case t))
      (lsp--completing-read "Select code action: "
                            (seq-into actions 'list)
                            (-compose (lsp--create-unique-string-fn)
                                      #'lsp:code-action-title)
                            nil t))))

;;; Settings

(setq lsp-eldoc-enable-hover nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-position 'at-point)

;; Don't highlight other instances of the thing under the cursor
(setq lsp-enable-symbol-highlighting nil)

;; Use the default major mode indentation function
(setq lsp-enable-indentation nil)


(bz/face rust-question-mark error)

;; (bz/face lsp-flycheck-warning-unnecessary
;;   lsp-lsp-flycheck-warning-unnecessary-face)

;; Disable all on-type formatting
(bz/advise :override lsp--on-type-formatting ignore)

;; Make wavy underlines straight
(dolist (f (face-list))
  (when-let* ((u (face-attribute f :underline))
              (s (and (listp u) (plist-get u :style))))
    (when (or (s-contains-p "lsp" (symbol-name f))
              (s-contains-p "flycheck" (symbol-name f)))
      (plist-put u :style 'line)
      (set-face-attribute f t :underline nil)
      (set-face-attribute f t :underline u))))


;; Completely disable lens (displaying big blocks of documentation in the echo area)
(bz/advise :override lsp-lens-refresh bz/lsp-lens-disable (&rest args) (lsp-lens-mode 0))

;;; Keymap

(bz/keys lsp-mode-map
  :sparse t
  ;; "C-x C-f" (@ bz/lsp-save (if (buffer-modified-p) (save-buffer) (lsp-on-save)))

  "C-c C-e" flycheck-explain-error-at-point
  "C-c C-b" flycheck-buffer

  ;; "C-j" flycheck-next-error
  "C-c C-j" flycheck-next-error
  ;; "C-k" flycheck-previous-error
  "C-c C-k" flycheck-previous-error

  "C-c C-w C-s" lsp-workspace-shutdown
  "C-c C-w C-r" lsp-workspace-restart

  "C-c C-r" lsp-rename
  "C-c C-d" lsp-find-definition
  "C-c C-f" lsp-find-references
  "C-c C-l" lsp-ui-flycheck-list

  "C-c C-a" lsp-execute-code-action
  "C-c C-x" (@ bz/lsp-act (lsp-execute-code-action
                           (or (lsp-seq-first (lsp-code-actions-at-point))
                               (error "No code actions"))))
  "C-c C-m" bz/lsp-refactor-file
  "C-c C-i" (@ bz/lsp-organize-imports
               (ignore-errors
                 (lsp-execute-code-action
                  (or (lsp-seq-first (lsp-get-or-calculate-code-actions "source.organizeImports"))
                      (error "Organizing imports is not supported")))))
  "C-c C-c" (@ bz/lsp-act-all
               (save-excursion
                 (goto-char (point-min))
                 (while (ignore-errors (flycheck-next-error) t)
                   (bz/lsp-act) (ignore-errors (forward-char -1)))))

  [remap bz/q]
  (@ bz/lsp-popup-show
     (if (lsp-ui-doc--frame-visible-p)
         (lsp-ui-doc-focus-frame)
       (bz/lsp-popup))))



;;; Setup

(bz/hook lsp-mode-hook bz/lsp-setup

  ;; Use my own signature by default
  (when (memq major-mode '(rust-mode scala-mode))
    (bz/lsp-signature-mode 1))

  ;; So weird "$0"s don't show up in company output
  (yas-minor-mode 1)

  ;; Enable company with immediate autocompleting
  (bz/company-autocomplete-mode 1)

  ;; Organize imports before save
  ;; (add-hook 'before-save-hook #'bz/lsp-organize-imports nil 'local)
  (add-hook 'before-save-hook #'whitespace-cleanup nil 'local))

(when lsp-mode (bz/lsp-setup))


;;; Disable post command hook

;; For js/ts language server, disabling post command hook makes it not
;; run super slow like it does otherwise

(bz/advise :override lsp--post-command ignore)



;;; Languages
;;;; Python

;; Install python package 'python-lsp-server' for lsp support
;; Install python package 'flake8' for syntax and style checking
;; Install python package 'mypy' for type checking
(defun bz/python-lsp-setup ()
  (interactive)
  (bz/package flycheck-pycheckers)
  (setq flycheck-pycheckers-checkers '(flake8 mypy3))
  (flycheck-pycheckers-setup))


;;;; Javascript/Typescript

;; Use double quotes
(setq lsp-typescript-preferences-quote-style "double")
(setq lsp-javascript-preferences-quote-style "double")
(setq typescript-indent-level 4)

(setq js-indent-level 4)

;; (setq lsp-clients-typescript-server-args '("--stdio"))
;; (setq lsp-clients-typescript-server-command "typescript-language-server")

;; Otherwise the tsserver takes forever to catch up
;; (setq lsp-typescript-tsc-auto-detect "on")
;; (setq lsp-typescript-tsc-auto-detect "off")


;;;; Rust
;; Add cargo to path
(setenv "PATH" (format "%s:%s" (getenv "PATH") "~/.cargo/bin"))
;; (lsp-install-server nil 'rust-analyzer)

;; This is necessary for wasm files
(setq lsp-rust-analyzer-diagnostics-disabled ["missing-unsafe"])

;;;;; How rust should organize imports

(defun bz/fix-rust-imports ()
  (interactive)

  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp "^[ \t]*use " nil t)
      (let ((prefix (buffer-substring (line-beginning-position) (point)))
            (organized
             (->> (buffer-substring (point) (1- (search-forward ";")))
                  (bz/parse-rust-import)
                  (bz/organize-rust-imports))))
        (when (> (length organized) 1)
          (delete-region (1+ (line-end-position)) (line-beginning-position))
          (dolist (im organized)
            (insert (format "%s%s;\n" prefix (bz/format-rust-import im)))))))))

(defun bz/organize-rust-imports (imports)
  "IMPORTS is a list of lists of symbols.
Each list of symbols represents one import.  The output is a list
of lines, each represented by a list.  The first element of the
list is the base path, and the remaining items are the imports
from that path."

  (let (lines)
    (dolist (im (--sort (> (length it) (length other)) (-uniq imports)))
      (if-let ((line (assoc im lines)))
          (setcdr line (cons 'self (cdr line)))
        (if-let ((line (assoc (butlast im 1) lines)))
            (nconc line (list (car (last im))))
          (push (list (butlast im 1) (-last-item im)) lines))))
    ;; Longest first
    (reverse lines)))

(defun bz/format-rust-import (import)
  (concat (string-join (mapcar 'symbol-name (car import)) "::") "::"
          (if (= (length import) 2) (symbol-name (cadr import))
            (format "{%s}" (string-join (mapcar 'symbol-name (cdr import)) ", ")))))

(defun bz/parse-rust-import (str &optional start return-idx)
  "Returns a cons of a list of imports and an index"
  (setq start (or start 0))
  (while (memq (aref str start) '(?\s ?,)) (setq start (1+ start)))
  (let ((idx start) imports)
    (if (not (eq (aref str start) ?\{))
        (while (null imports)
          (pcase (when (< idx (length str)) (aref str idx))
            ((or 'nil ?, ?} ?\;)
             (setq imports (list (pcase (intern (substring str start idx))
                                   ('self nil) (sym (list sym))))))
            (?: (let ((prefix (intern (substring str start idx)))
                      (part (bz/parse-rust-import str (+ idx 2) t)))
                  (setq imports (--map (cons prefix it) (car part)) idx (cdr part))))
            (_ (setq idx (1+ idx)))))
      ;; If braces
      (setq idx (1+ idx))
      (while (not (eq (aref str idx) ?}))
        (let ((part (bz/parse-rust-import str idx t)))
          (setq imports (append imports (car part))
                idx (cdr part)))))

    (if return-idx (cons imports idx) imports)))

;;;;; Make rust struct multiline
(bz/hook company-after-completion-hook bz/rust-struct-expand :remove
         (bz/delay 0
                   (line-end-position)
                   (when (and (eq major-mode 'rust-mode)
                              (string= ": () }" (buffer-substring (- (point) 6) (point))))
                     (let ((m (set-marker (make-marker) (point))))
                       (beginning-of-line-text)
                       (save-excursion
                         (replace-string "{ " "{\n" nil (point) (marker-position m))
                         (replace-string ": ()," ",\n" nil (point) (marker-position m))
                         (replace-string ": () }" "\n}" nil (point) (marker-position m))
                         (bz/indent-buffer))
                       (forward-line) (end-of-line) (backward-char)))))
;;; Rename file and imports

(setq bz/lsp-import-format-alist
      '((dart-mode :srcdir "lib" :format "package:%r/%f")))

(defun bz/lsp-import-string (root file mode)
  (let* ((spec (or (alist-get mode bz/lsp-import-format-alist)
                   (error "No import format for %s" mode)))
         (relative (f-relative file (f-join root (plist-get spec :srcdir)))))
    (->> (plist-get spec :format)
         (replace-regexp-in-string "%r" (file-name-base root))
         (replace-regexp-in-string "%f" relative))))

(defun bz/lsp-refactor-file (newname)
  (interactive "FMove To: ")
  (let* ((oldname (or (buffer-file-name) (error "Not a file")))
         (root (or (lsp-workspace-root) (error "No lsp root")))
         (old-str (bz/lsp-import-string root oldname major-mode))
         (new-str (bz/lsp-import-string root newname major-mode))
         (_ (bz/move-buffer-file newname))
         (files (--> ($$ "grep -R %s -e %s" root (regexp-quote old-str))
                     (split-string it "\n")
                     (prog1 it (message ">>>%s" (prin1-to-string it)))
                     (--map (expand-file-name (car (split-string it ":"))) it)
                     (-uniq it))))

    (dolist (file files)
      (when (file-regular-p file)
        (find-file file)
        (beginning-of-buffer)
        (replace-string old-str new-str)
        (save-buffer)))))

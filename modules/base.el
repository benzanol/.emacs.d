;; -*- lexical-binding: t; -*-

(require 'benchmark)
(require 'package)


;;; Module Macros

(defvar bz/loaded-modules nil
  "List of modules that have been loaded.")

(defun bz/load (module)
  (interactive
   (list (completing-read
          "Load Module: "
          (mapcar 'file-name-base
                  (let ((default-directory "/"))
                    (split-string (shell-command-to-string "ls ~/.emacs.d/modules")))))))

  (condition-case result
      (benchmark-elapse (load (format "~/.emacs.d/modules/%s.el" module) nil t))
    (:success
     (add-to-list 'bz/loaded-modules (intern (format "%s" module)))
     (message "Module %s loaded in %s seconds" module result))
    (t (message "Error loading module %s: '%s'" module result))))

(defmacro bz/required (module)
  `(not (not (memq ',module bz/loaded-modules))))

(defmacro bz/require (module)
  `(unless (bz/required ,module)
     (ignore-errors (bz/load ',module))))

(defmacro bz/after (package &optional module)
  `(eval-after-load ',package
     (lambda () (bz/require ,(or module package)))))


;;; Face Macro

(defvar bz/face-property-abbrevs
  (list :i :inherit
        :fg :foreground
        :bg :background
        :w :weight
        :s :slant
        :iv :inverse-video
        :f :family
        :h :height
        :u :underline
        :o :overline
        :b :box
        :st :strikethrough
        :x :extend)
  "Plist of face property abbreviations and what they should map to.")

(defvar bz/face-display-condition-abbrevs
  '(dark ((background dark)) light ((background light))
         color ((class color)) mono ((class mono))
         x ((type x)) tty ((type tty)) macos ((type ns)) windows ((type w32)))
  "Plist of display condition abbreviations.")

(defvar bz/face-alist nil)

(defmacro bz/face (face &rest props)
  (declare (indent 1))
  `(let ((props ,(list '\` props)))
     (setf (alist-get ',face bz/face-alist) props)
     (face-spec-set ',face (bz/face-spec props))))

(defun bz/face-spec (props)
  ;; Common contains properties for all display conditions
  ;; Specific is an alist of display conditions to properties for that specific condition
  (let (common specific)
    ;; Use the beginning of the spec as inherited faces
    (unless (keywordp (car props)) (push :inherit props))

    ;; Add the additional props to the spec
    (while props
      (let* ((prop-abbrev (pop props))
             ;; Allow the syntax :dark:fg to set a property for a particular display condition
             (display-cond
              (let* ((str (symbol-name prop-abbrev)))
                (when (string-match "^:\\(.+\\)\\(:[^:]+\\)$" str)
                  (setq prop-abbrev (intern (match-string 2 str)))
                  (or (plist-get bz/face-display-condition-abbrevs (intern (match-string 1 str)))
                      (error "Unkown display condition abbreviation: %s" (match-string 1 str))))))
             ;; Expand abbreviations
             (prop (or (plist-get bz/face-property-abbrevs prop-abbrev) prop-abbrev))
             (val (pop props)))
        ;; For properties whose value can be a color, convert symbol to (bz/color sym)
        (when (memq prop '(:overline :underline :foreground :background))
          (setq val (or (bz/get-color val)
                        (and (stringp val) val)
                        (when (memq prop '(:foreground :background)) 'unspecified)
                        val)))
        ;; For box property, apply the above transformation to the box color
        (when (and (memq prop '(:box)) (listp val) (symbolp (plist-get val :color)))
          (setq val (apply #'list val))
          (plist-put val :color (bz/get-color (plist-get val :color))))

        (if (not display-cond) (setq common (cons prop (cons val common)))
          (setf (alist-get display-cond specific nil nil #'equal)
                (cons prop (cons val (alist-get display-cond specific nil nil #'equal)))))))

    ;; Add common properties to all specific specs
    (dolist (s specific) (setcdr s (append common (cdr s))))
    (append specific (list (cons t common)))))


;;; Colors

(defvar bz/color-plist nil
  "Plist of symbols and valid emacs color strings.")

(defmacro bz/color (color) `(bz/get-color ',color))
(defun bz/get-color (color) (plist-get bz/color-plist color))

(defmacro bz/set-colors (&rest args)
  `(progn
     ,@(mapcar
        (lambda (n)
          (let ((face (nth n args)) (color (nth (1+ n) args)))
            `(set 'bz/color-plist (plist-put bz/color-plist ',face ,color))))
        (number-sequence 0 (- (length args) 2) 2))
     (set-background-color (bz/color bg))
     (set-foreground-color (bz/color fg))
     (set-cursor-color (bz/color fg))
     (dolist (entry bz/face-alist)
       (face-spec-set (car entry) (bz/face-spec (cdr entry))))))


;;; Hook Macro

(defmacro bz/hook (hooks name &rest body)
  "Define a function NAME with BODY and add it to HOOKS.
HOOK can be a single hook, or a list of hooks.
If BODY starts with :remove, remove the hook."
  (declare (indent 2))

  (let* ((keys (cl-loop while (keywordp (car body)) collect (pop body)))
         (local (if (memq :local keys) :local nil))
         (hook-name (if (listp hooks) 'hook (list 'quote hooks)))
         (inner (if (memq :remove keys) `(remove-hook ,hook-name #',name ,local)
                  `(add-hook ,hook-name #',name nil ,local)))
         (loop (if (listp hooks) `(dolist (hook ',hooks) ,inner) inner))
         (fn-body `(with-demoted-errors ,(format "Error in Hook %s: %%s" name) ,@body)))
    (if body `(progn (defun ,name (&rest _) ,fn-body) ,loop) loop)))


;;; Advice Macro

(defmacro bz/advise (where func name &optional args &rest body)
  "Define advice for FUNC with function name NAME as defined by BODY.
If BODY starts with :remove, remove the advice."
  (declare (indent 4) (doc-string 5))

  (setq name (symbol-name name))
  (setq name (replace-regexp-in-string "^\\*:" (format "advice:%s:" func) name))
  (setq name (replace-regexp-in-string "\\*" (symbol-name func) name))
  (setq name (intern name))

  (let* ((expr (if (and (null args) (null body)) `',name
                 `(defun ,name ,args . ,body))))
    (if (eq where :remove)
        `(advice-remove ',func ,expr)
      `(advice-add ',func ,where ,expr))))


;;; Keybinding Macros
;;;; Key

(defmacro bz/key (map key binding)
  (declare (indent 1))

  (setq map (if (eq map '*) 'global-map map)
        key (cond ((stringp key) (kbd key))
                  ((numberp key) (vector key))
                  (t key)))

  (pcase key
    (:doc `(defvar ,map nil ,binding))
    (:sparse `(if (and (boundp ',map) (keymapp ,map))
                  (setcdr ,map nil) (setq ,map (make-sparse-keymap))))
    (:full `(if (and (boundp ',map) (keymapp ,map))
                (setcdr ,map (cdr (make-keymap))) (setq ,map (make-keymap))))
    (:parent `(set-keymap-parent ,map ,(if (keymapp binding) (list 'quote binding) binding)))
    (:prefix `(progn
                (defvar ,(cadr binding) nil ,(format "Extension of `%s' with \"%s\"" map (car binding)))
                (setq ,(cadr binding) (bz/add-keymap-prefix ,map ,(car binding)))))
    (_ (let* ((pre nil)
              (bind-val
               (pcase binding
                 ((guard (and (listp binding) (eq (car binding) '\,))) (cadr binding))
                 ((or (pred atom) (pred functionp) (pred keymapp)) `#',binding)
                 (`(,(or 'defun 'defmacro 'lambda) . ,_) binding)
                 (`(@ ,name ,(pred symbolp)) (setq pre `(defalias ',name ',(caddr binding))) `#',name)
                 (`(@ ,name ,(and (pred symbolp) arg) . ,body)
                  (setq pre `(defun ,name (&optional ,arg) (interactive "P") . ,body)) `#',name)
                 (`(@ ,name . ,body) (setq pre `(defun ,name () (interactive) . ,body)) `#',name)
                 (`(,(pred listp) . ,_) `(lambda () (interactive) . ,binding))
                 (_ (eval `(lambda () (interactive) ,binding)))))
              (define-expr
               (if (not (listp key)) (list #'define-key map key bind-val)
                 `(dolist (i (number-sequence ,(car key) ,(cadr key)))
                    (define-key ,map (vector i) ,bind-val)))))
         (if pre `(progn ,pre ,define-expr) define-expr)))))


;;;; Keys

(defmacro bz/keys (map &rest forms)
  "Define keys as specified by FORMS in MAP.

FORMS can start with various properties:
:parent PARENT sets the keymap PARENT as the parent of MAP
:sparse t clears MAP, making it a new sparse keymap
:full t clears MAP, making it a new full (chartable) keymap
:prefix PREFIX adds PREFIX before every binding"
  (declare (indent 1) (doc-string 3))

  (let ((keyword-order '(:doc -2 :sparse -1 :full -1 :prefix 1)))
    `(prog1 nil
       ,@(mapcar (lambda (n) `(bz/key ,map ,(nth n forms) ,(nth (1+ n) forms)))
                 (sort (number-sequence 0 (1- (length forms)) 2)
                       :key (lambda (n) (or (plist-get keyword-order (nth n forms)) 0)))))))


;;;; Keymap prefix

(defun bz/add-keymap-prefix (keymap prefix &optional recursive)
  "Return a new keymap containing each of the keys in KEYMAP
modified by PREFIX.

PREFIX should be a string to add before the string representation of
a keymap to modify it, for example \"M-\" would add the meta modifier
to a key sequence."

  (if (or (not (stringp prefix)) (equal prefix "")) keymap
    (when (symbolp keymap) (setq keymap (symbol-value keymap)))
    (unless (keymapp keymap) (error "Not a valid keymap: %s" keymap))
    ;; Make the new keymap the same type as the input keymap
    (let ((new-map (if (char-table-p (cadr keymap))
                       (make-keymap)
                     (make-sparse-keymap)))
          new-key)
      ;; If `PREFIX` doesn't end in a space or dash, add a space
      (setq prefix (replace-regexp-in-string "[^ -]$" "\\& " prefix))
      ;; Add each modified binding to the new keymap
      (map-keymap
       (lambda (key binding)
         (setq new-key
               (pcase key
                 (`[remap ,_] key)
                 ((or (pred arrayp) (pred numberp) (pred symbolp))
                  (kbd (concat prefix (key-description (if (vectorp key) key (vector key))))))
                 (`(,_ . ,_) nil)
                 (_ (vector key))))
         (when new-key
           (define-key new-map new-key
                       (if (and recursive (keymapp binding))
                           (bz/add-keymap-prefix binding prefix t)
                         binding))))

       keymap)
      new-map)))


;;; Font Lock

(defvar bz/custom-font-lock-keywords-alist nil)

(defun bz/font-lock-add-keywords (mode keywords &optional how)
  "Same as `font-lock-add-keywords', but remove the old ones first."
  (let ((existing (alist-get mode bz/custom-font-lock-keywords-alist nil nil #'equal)))
    (cond ((symbolp mode)
           (font-lock-remove-keywords mode existing)
           (font-lock-add-keywords mode keywords how))

          ((and (listp mode) (eq (car mode) 'var))
           (set (cadr mode)
                (nconc (cl-loop for kwd in (symbol-value (cadr mode))
                                unless (member kwd existing) collect kwd)
                       (apply #'list keywords)))))

    (setf (alist-get mode bz/custom-font-lock-keywords-alist nil nil #'equal) keywords)))


;;; Shell functions

(defmacro bz/fix-default-directory (&rest body)
  `(if (and default-directory (not (file-directory-p default-directory)))
       (let* ((default-directory (expand-file-name "~"))) ,@body)
     ,@body))

(bz/advise :around shell-command-to-string bz/shell-command-to-string-advice (&rest args)
  (bz/fix-default-directory (apply args)))

(defun $ (cmd &rest args)
  (interactive "sCommand: ")
  (bz/fix-default-directory
   (let* ((arg-strs (cl-loop for arg in args collect (shell-quote-argument (format "%s" arg)))))
     (start-process-shell-command "command" "*Shell*" (apply #'format cmd arg-strs)))))

(defun $$ (cmd &rest args)
  (bz/fix-default-directory
   (let* ((arg-strs (cl-loop for arg in args collect (shell-quote-argument (format "%s" arg)))))
     (shell-command-to-string (apply #'format cmd arg-strs)))))

(defun bz/shell-command-async (cmd &optional success-cb error-cb)
  (let* ((out-buff (generate-new-buffer "*shell-stdout*"))
         (err-buff (generate-new-buffer "*shell-stderr*"))
         (proc (make-process
                :name "shell-cmd"
                :command (list "sh" "-c" cmd)
                :buffer out-buff
                :stderr err-buff
                :noquery t
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((exit (process-exit-status proc))
                          (stdout (with-current-buffer out-buff (buffer-string)))
                          (stderr (with-current-buffer err-buff (buffer-string))))
                      (kill-buffer out-buff)
                      (kill-buffer err-buff)
                      (if (= exit 0) (funcall success-cb stdout stderr)
                        (if error-cb (funcall error-cb stdout stderr exit) (error stderr)))))))))
    proc))

(defmacro $& (arg cmd &rest body)
  (declare (indent 1))
  (when (vectorp cmd)
    (setq cmd (append cmd nil)
          cmd `(format ,(car cmd) ,@(cl-loop for arg in (cdr cmd)
                                             collect (list #'shell-quote-argument arg)))))
  `(bz/shell-command-async ,cmd (lambda ,(cons arg '(&rest _)) ,@body)))


;;; Package Management

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(defmacro bz/package (name)
  `(or (require ',name nil t)
       (package-install ',name)))

(bz/advise :remove require bz/require-advice (func package &optional file &rest _args)
  (or (funcall func package file t) (package-install package)))

(let ((module-link-dir "~/.emacs.d/.modules"))
  (unless (file-directory-p module-link-dir) (mkdir module-link-dir))
  (add-to-list 'load-path module-link-dir)

  (dolist (f (directory-files "~/.emacs.d/modules"))
    (let ((newf (format "%s/bz-%s" module-link-dir (file-name-nondirectory f))))
      (unless (file-symlink-p newf)
        (make-symbolic-link (expand-file-name f) newf)))))


;;; Better Timer

(defmacro bz/timer (time &rest body)
  (declare (indent 1))
  (let* ((args (if (vectorp (car body))
                   (append (pop body) nil)))
         (arglist (cl-loop for _ in args
                           for idx upfrom 1
                           collect (intern (format "@%s" idx)))))
    `(run-with-timer
      ,time nil
      (lambda (=buf= ,@arglist) (with-current-buffer =buf= ,@body))
      (current-buffer) ,@args)))


;;; Provide

(provide 'bz-base)

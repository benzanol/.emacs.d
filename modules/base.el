;;; Module Macros

(require 'benchmark)

(defvar bz/loaded-modules nil
  "List of modules that have been loaded.")

(defun bz/load (module)
  (interactive
   (list (completing-read
          "Load Module: "
          (mapcar 'file-name-base
                  (split-string (shell-command-to-string "ls ~/.emacs.d/modules"))))))
  (condition-case result (benchmark-elapse (load (format "~/.emacs.d/modules/%s.el" module) nil t))
    (:success (message "Module %s loaded in %s seconds" module result))
    (t (message "Error loading module %s: '%s'" module result)))
  (add-to-list 'bz/loaded-modules (intern (format "%s" module))))

(global-set-key (kbd "C-x C-l") 'bz/load)

(defmacro bz/required (module)
  `(not (not (memq ',module bz/loaded-modules))))

(defmacro bz/require (module)
  `(unless (bz/required ,module)
     (ignore-errors (bz/load ',module))))

(defmacro bz/after (package &optional module)
  `(eval-after-load ',package
     (lambda () (bz/require ,(or module package)))))


;;; Package Management

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;; ("org" . "https://orgmode.org/elpa/")
;; ("elpa" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)

(defmacro bz/package (name)
  `(or (require ',name nil t)
       (package-install ',name)))


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

(setq bz/face-alist nil)

(defmacro bz/face (face &rest props)
  (declare (indent 1))
  `(progn (setf (alist-get ',face bz/face-alist) ',props)
          (apply #'bz/apply-face-props ',face ',props)))

(defun bz/apply-face-props (face &rest props)
  (let (prop val spec)
    ;; Use the beginning of the spec as inherited faces
    (unless (keywordp (car props)) (push :inherit props))

    ;; Add the additional props to the spec
    (while props
      (let* ((prop-abbrev (pop props))
             (prop (or (plist-get bz/face-property-abbrevs prop-abbrev) prop-abbrev))
             (val (pop props))
             (iscolor (and val (symbolp val) (not (eq val t)))))
        (when (and iscolor (memq prop '(:overline :underline :foreground :background)))
          (setq val (bz/get-color val)))
        (setq spec (cons prop (cons val spec)))))

    ;; Wrap the spec in a backquote, and evaluate it
    (face-spec-set face (eval (list '\` (list (cons t spec)))))))


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
     (dolist (face-spec bz/face-alist)
       (apply #'bz/apply-face-props face-spec))))


;;; Hook Macro

(defmacro bz/hook (hooks name &rest body)
  "Define a function NAME with BODY and add it to HOOKS.
HOOK can be a single hook, or a list of hooks.
If BODY starts with :remove, remove the hook."
  (declare (indent 2) (doc-string 3))

  (let ((hooks (if (listp hooks) hooks (list hooks)))
        (remove (when (eq (car body) :remove) (pop body)))
        (local (when (eq (car body) :local) (pop body)))
        (expr (if (null body) `',name
                (if (null name) `(lambda (&rest args) . ,body)
                  `(defun ,name (&rest args) . ,body)))))
    (if remove
        (cons #'progn (mapcar (lambda (h) `(remove-hook ',h ,expr ,local)) hooks))
      (cons #'progn (mapcar (lambda (h) `(add-hook ',h ,expr nil ,local)) hooks)))))


;;; Advice Macro

(defmacro bz/advise (where func name &optional args &rest body)
  "Define advice for FUNC with function name NAME as defined by BODY.
If BODY starts with :remove, remove the advice."
  (declare (indent 4) (doc-string 5))

  (let* ((newname (intern (replace-regexp-in-string "\\*" (symbol-name func) (symbol-name name))))
         (expr (if (and (null args) (null body)) `',name
                 `(defun ,newname ,args . ,body))))
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
    (:sparse `(if (and (boundp ',map) (keymapp ,map))
                  (setcdr ,map nil) (setq ,map (make-sparse-keymap))))
    (:full `(if (and (boundp ',map) (keymapp ,map))
                (setcdr ,map (cdr (make-keymap))) (setq ,map (make-keymap))))
    (:parent `(set-keymap-parent ,map ,(if (keymapp binding) (list 'quote binding) binding)))
    (:prefix `(setq ,(cadr binding) (bz/add-keymap-prefix ,map ,(car binding))))
    (_ (let ((bind-val
              (pcase binding
                ((guard (and (listp binding) (eq (car binding) '\,))) (cadr binding))
                ((or (pred atom) (pred functionp) (pred keymapp)) (list 'quote binding))
                (`(,(or 'defun 'defmacro 'lambda) . ,_) binding)
                (`(@ ,name ,(pred symbolp)) `(defalias ',name ',(caddr binding)))
                (`(@ ,name . ,body) `(defun ,name (&optional =arg=) (interactive "P") . ,body))
                (`(,(pred listp) . ,_) `(lambda (&optional =arg=) (interactive "P") . ,binding))
                (_ (eval `(lambda (&optional =arg=) (interactive "P") ,binding))))))
         (if (not (listp key)) (list #'define-key map key bind-val)
           `(dolist (i (number-sequence ,(car key) ,(cadr key)))
              (define-key ,map (vector i) ,bind-val)))))))

;;;; Keys
(require 'seq)

(defmacro bz/keys (map &rest forms)
  "Define keys as specified by FORMS in MAP.

FORMS can start with various properties:
:parent PARENT sets the keymap PARENT as the parent of MAP
:sparse t clears MAP, making it a new sparse keymap
:full t clears MAP, making it a new full (chartable) keymap
:prefix PREFIX adds PREFIX before every binding"
  (declare (indent 1))

  (let ((keyword-order '(:sparse -1 :full -1 :prefix 1)))
    `(prog1 nil
       ,@(mapcar (lambda (n) `(bz/key ,map ,(nth n forms) ,(nth (1+ n) forms)))
                 (seq-sort-by (lambda (n) (or (plist-get keyword-order (nth n forms)) 0))
                              '< (number-sequence 0 (1- (length forms)) 2))))))

;;;; Keymap prefix
(defun bz/add-keymap-prefix (keymap prefix &optional recursive)
  "Return a new keymap containing each of the keys in KEYMAP
modified by PREFIX.

PREFIX should be a string to add before the string representation of
a keymap to modify it, for example \"M-\" would add the meta modifier
to a key sequence."

  (if (or (not (stringp prefix)) (eq prefix "")) keymap
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
         (setq new-key (pcase key
                         (`[remap ,_] key)
                         ((or (pred arrayp) (pred numberp))
                          (kbd (concat prefix (key-description
                                               (if (vectorp key)
                                                   key (vector key))))))
                         (`(,_ . ,_) nil)
                         (_ (vector key))))
         (when new-key
           (define-key new-map new-key
             (if (and recursive (keymapp binding))
                 (qvk-add-keymap-prefix binding prefix t)
               binding))))

       keymap)
      new-map)))

;;; Background Shell Commands
(defun qv/run-in-background (command)
  (interactive "MSystem Command: ")
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;; Module Macros
(require 'benchmark)

(defvar qv/loaded-modules nil
  "List of modules that have been loaded.")

(defun qv/load (module)
  (interactive
   (list (completing-read
          "Load Module: "
          (mapcar 'file-name-base
                  (split-string (shell-command-to-string
                                 "ls ~/.emacs.d/modules"))))))
  (let ((file (format "~/.emacs.d/modules/%s.el" module)))
    (if-let ((time (benchmark-elapse (ignore-errors (load file)))))
        (progn (message "Module `%s` loaded in %s seconds" module time)
               (add-to-list 'qv/loaded-modules (intern (format "%s" module))))
      (message "Error loading `%s`" file))))

(global-set-key (kbd "C-x C-l") 'qv/load)

(defmacro qv/required (module)
  `(memq ',module qv/loaded-modules))

(defmacro qv/require (module)
  `(unless (qv/required ,module)
     (qv/load ',module)))

(defmacro qv/after (package &optional module)
  `(eval-after-load ',package
     (lambda nil (qv/require ,(or module package)))))

;;; Package Management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defmacro qv/package (name)
  `(or (require ',name nil t)
       (package-install ',name)))

;;; Face Macro
(defvar qv/face-property-abbrevs
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

(defmacro qv/face (face &rest props)
  (declare (indent 1))
  (let (prop val spec)
    ;; Set the beginning of the spec as inherited faces
    (when (not (keywordp (car props)))
      (setq spec (list (pop props) :inherit)))
    ;; Add the additional props to the spec
    (while props
      (setq prop (pop props)
            prop (or (plist-get qv/face-property-abbrevs prop) prop)
            val (pop props)
            val (if (and (or (eq prop :foreground) (eq prop :background)) (symbolp val))
                    (list '\, (list 'qv/color val)) val)
            spec `(,val ,prop . ,spec)))
    ;; Wrap the spec in a backquote, and run it into `face-spec-set`
    (list 'face-spec-set (list '\` face) (list '\` (list (cons t (reverse spec)))))))

;;; Colors
(defvar qv/color-plist nil
  "Plist of symbols and valid emacs color strings.")

(defmacro qv/color (color)
  `(plist-get qv/color-plist ',color))

(defmacro qv/set-colors (&rest args)
  (cons 'progn
        (mapcar (lambda (n)
                  `(set 'qv/color-plist (plist-put qv/color-plist ',(nth n args) ,(nth (1+ n) args))))
                (number-sequence 0 (- (length args) 2) 2))))

;;; Hook Macro
(defmacro qv/hook (hook name &rest body)
  "Define a function NAME with BODY and add it to HOOK.
HOOK can also be a list of hooks."
  (declare (indent 2))
  (unless (listp hook) (setq hook (list hook)))
  `(let ((func ,(if (and (symbolp name) name)
                    `(defun ,name () ,@body)
                  `(lambda () ,@body))))
     (mapcar (lambda (h) (add-hook h func)) ',hook)))

;;; Keybinding Macros
(defvar qv/keybinding-abbrevs
  '(* (global-set-key)
      ~ (local-set-key)
      exwm (exwm-input-set-key))
  "Plist of abbreviations mapped to forms that will bind a key when 2 arguments are added.")

(defmacro qv/key (map key binding)
  (declare (indent 1))
  (cond ((eq key :parent) `(set-keymap-parent ,map ,(if (keymapp binding) (list 'quote binding) binding)))
        ((eq key :sparse) `(if (and (boundp ',map) (keymapp ,map))
                               (setcdr ,map nil) (setq ,map (make-sparse-keymap))))
        ((eq key :full) `(if (and (boundp ',map) (keymapp ,map))
                             (setcdr ,map (cdr (make-keymap))) (setq ,map (make-keymap))))
        ((eq key :prefix) `(let ((new (qv/add-keymap-prefix ,map ,(car binding) ,(nth 2 binding))))
                             (if (and (boundp ',(cadr binding)) (keymapp ,(cadr binding)))
                                 (setcdr ,(cadr binding) (cdr new)) (setq ,(cadr binding) new))))
        (t (setq key (cond ((stringp key) (kbd key)) ((numberp key) (vector key)) (t key)))
           `(,@(or (plist-get qv/keybinding-abbrevs map) (list 'define-key map)) ,key
             ,(cond ((and (listp binding) (eq (car binding) '\,)) (cadr binding))
                    ((or (atom binding) (functionp binding) (keymapp binding)) (list 'quote binding))
                    ((memq (car binding) '(defun defmacro lambda)) binding)
                    ((eq (car binding) '@) `(defun ,(cadr binding) () (interactive) . ,(cddr binding)))
                    ((listp (car binding)) `(lambda () (interactive) . ,binding))
                    (t (eval `(lambda () (interactive) ,binding))))))))

(defmacro qv/keys (map &rest forms)
  (declare (indent 1))

  ;; Move prefix definitions to the end
  (when (memq :prefix forms)
    (let ((rev (reverse forms)))
      (setq forms nil)
      (while rev
        (if (eq (cadr rev) :prefix)
            (setq forms (append forms (list (cadr rev) (car rev)))
                  rev (cddr rev))
          (push (pop rev) forms) (push (pop rev) forms)))))

  `(progn ,@(mapcar (lambda (n) `(qv/key ,map ,(nth n forms) ,(nth (1+ n) forms)))
                    (number-sequence 0 (1- (length forms)) 2))
          ',map))

;;; Keymap prefix
(defun qv/add-keymap-prefix (keymap prefix &optional recursive)
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
                     (make-sparse-keymap))))
      ;; If `PREFIX` doesn't end in a space or dash, add a space
      (setq prefix (replace-regexp-in-string "[^ -]$" "\\& " prefix))
      ;; Add each modified binding to the new keymap
      (map-keymap
       (lambda (key binding)
         (define-key new-map
           (if (and (vectorp key) (eq (aref key 0) 'remap)) key
             (kbd (concat prefix (key-description
                                  (if (vectorp key)
                                      key (vector key))))))
           (if (and recursive (keymapp binding))
               (qvk-add-keymap-prefix binding prefix t)
             binding)))
       keymap)
      new-map)))

;;; Apply
(defmacro qv/apply (func &rest applications)
  (declare (indent 1))
  (cons 'progn (mapcar (lambda (x) (cons func x)) applications)))

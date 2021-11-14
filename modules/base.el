;;; Background Shell Commands
(defun qv/run-in-background (command)
  (interactive "MSystem Command: ")
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;; Module Macro
(defvar qv/loaded-modules nil
  "List of modules that have been loaded.")

(defun qv/load (module)
  (interactive
   (list (completing-read
          "Load Module:"
          (mapcar 'file-name-base
                  (split-string (shell-command-to-string
                                 "ls ~/.emacs.d/modules"))))))
  (load-file (format "~/.emacs.d/modules/%s.el" module)))

(defmacro qv/module (module)
  `(qv/load ',module))

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
            spec (cons (pop props) (cons prop spec))))
    ;; Wrap the spec in a backquote, and run it into `face-spec-set`
    (list 'face-spec-set (list '\` face) (list '\` (list (cons t (reverse spec)))))))

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
  (declare (indent 2))
  (cond
   ((eq key :parent) `(set-keymap-parent ,map ,(if (keymapp binding) (list 'quote binding) binding)))
   ((eq key :sparse) (if (and (boundp map) (keymapp (eval map)))
                         `(setcdr ,map nil) `(setq ,map (make-sparse-keymap))))
   ((eq key :full) (if (and (boundp map) (keymapp (eval map)))
                       `(setcdr ,map (cdr (make-keymap))) `(setq ,map (make-sparse-keymap))))
   (t (setq key (cond ((stringp key) (kbd key)) ((numberp key) (vector key)) (t key)))
      (if (null binding) `(unbind-key ,key ,map)
        `(,@(or (plist-get qv/keybinding-abbrevs map) (list 'define-key map)) ,key
          ,(cond ((or (atom binding) (functionp binding) (keymapp binding)) (list 'quote binding))
                 ((memq (car binding) '(defun defmacro lambda)) binding)
                 ((eq (car binding) '@) `(defun ,(cadr binding) () (interactive) . ,(cddr binding)))
                 ((listp (car binding)) `(lambda () (interactive) . ,binding))
                 (t (eval `(lambda () (interactive) ,binding)))))))))

(defmacro qv/keys (map &rest forms)
  (declare (indent 1))
  (cons 'progn (mapcar (lambda (n) (macroexpand `(qv/key ,map ,(nth n forms) ,(nth (1+ n) forms))))
                       (number-sequence 0 (1- (length forms)) 2))))

;;; Apply
(defmacro qv/apply (func &rest applications)
  (declare (indent 1))
  (cons 'progn (mapcar (lambda (x) (cons func x)) applications)))

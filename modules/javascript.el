;; -*- lexical-binding: t; -*-

(require 'bz-apheleia)
(require 'bz-base)
(require 'bz-functions)

(require 'jtsx)
(require 'typescript-mode)
(require 'web-mode)


(bz/face typescript-jsdoc-value :fg gray2 :s italic)

(bz/hook typescript-mode-hook bz/typescript-setup
  (hs-minor-mode 1)
  (display-line-numbers-mode 1)

  (setq fci-rule-column 100)
  (fci-mode)

  ;; (call-interactively #'eglot)
  (flymake-eslint-enable)

  (setq-local eldoc-idle-delay 1.0))

(bz/hook (typescript-mode-hook rust-mode-hook) bz/outline-prefix-equal-signs
  (setq-local outline-regexp "// =+" outline-level (lambda () 1)))


;;; Typescript Keywords

(bz/face bz/font-lock-public bold)

;; Don't colorize function calls
(font-lock-remove-keywords
 'typescript-mode
 `((,typescript--function-call-re (1 font-lock-function-name-face))))


(dolist (mode '(typescript-mode (var web-mode-javascript-font-lock-keywords)))
  (bz/font-lock-add-keywords
   mode
   `(
     (,(let ((p "\\(?: +async\\| +abstract\\| +static\\)?"))
         (format "^[ \t]*\\(public%s%s\\(?: +get\\)? +\\)\\([a-zA-Z0-9]+\\) *[(<]" p p))
      (1 '(bz/font-lock-public font-lock-keyword-face) t)
      (2 '(bz/font-lock-public font-lock-function-name-face) t))
     ("^[ \t]*\\(public\\(?: +readonly\\)? +\\)\\([a-zA-Z0-9]+\\) *[:+]"
      (1 '(bz/font-lock-public font-lock-keyword-face) t)
      (2 'bz/font-lock-public t))
     ("^[ \t]*\\(export\\(?: +default\\)?\\(?: +async\\)? +function +\\)\\([a-zA-Z0-9]+\\)"
      (1 '(bz/font-lock-public font-lock-keyword-face) t)
      (2 '(bz/font-lock-public font-lock-function-name-face) t))
     ("^[ \t]*\\(export\\(?: +default\\)?\\(?:\\(?: +abstract\\)? +class\\| +type\\) +\\)\\([a-zA-Z0-9]+\\)"
      (1 '(bz/font-lock-public font-lock-keyword-face) t)
      (2 '(bz/font-lock-public font-lock-type-face) t))
     ("^[ \t]*\\(export\\(?: +default\\)? +const +\\)\\([a-zA-Z0-9]+\\)"
      (1 '(bz/font-lock-public font-lock-keyword-face) t)
      (2 'bz/font-lock-public t))
     (,(let ((p "\\(?:export +\\|default +\\|async +\\)?"))
         (format "^[ \t]*%s%s%sfunction *\\([a-zA-Z0-9_]+\\)" p p p))
      1 font-lock-function-name-face)
     (,(let ((p "\\(?:public +\\|private +\\|protected +\\|async +\\|abstract +\\)"))
         (format "^[ \t]*%s%s? *\\([a-zA-Z0-9_]+\\) *\\(?:(\\|<\\)" p p))
      1 font-lock-function-name-face)
     ("\\_<\\(function\\|if\\|for\\|while\\|import\\|const\\|let\\|type\\|async\\|await\\|protected\\)\\_>"
      1 font-lock-keyword-face)
     ("\\_<\\(number\\|boolean\\|string\\)\\_>"
      1 font-lock-keyword-face)
     ("\\_<\\([A-Z][a-zA-Z0-9_]*\\)\\_>"
      1 font-lock-type-face)
     ("\\_<\\(null\\|undefined\\|true\\|false\\)\\_>"
      1 font-lock-constant-face)
     ("\\_<\\(as any\\)\\_>" 1 '(error))
     ("\\_<\\(as\\)\\_>" 1 '(error))
     ("[a-zA-Z0-9)]\\(!\\)" 1 '(error))
     )))

;; ("\\_<\\([a-zA-Z0-9_]*\\)[(<]"
;;  1 font-lock-constant-face)


;;; Web mode stuff

(setq web-mode-enable-auto-quoting nil)
(setq web-mode-enable-auto-indentation nil)
(bz/face web-mode-html-tag-face :fg orange :w bold)
(bz/face web-mode-html-tag-bracket-face :fg orange :w bold)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(bz/keys web-mode-map
  :sparse t
  [remap bz/comment-line] web-mode-comment-or-uncomment
  [remap bz/paren-delete] bz/web-mode-delete-pair
  [remap bz/paren-replace] bz/web-mode-replace-pair
  )

(defun bz/web-typescript-fill-paragraph (&optional arg)
  (when-let*
      ((comment (web-mode-comment-context))
       (_ (equal (plist-get comment :format) "/*"))
       (beg (plist-get comment :beg))
       (pos (- (point) beg))
       (str (save-excursion (goto-char beg) (and (looking-at "[^9-0]+?\\*/") (match-string 0))))
       (new-str t))

    (with-temp-buffer
      (let ((typescript-mode-hook nil))
        (typescript-mode)
        (insert str)
        (goto-char (- pos (point-min)))
        (typescript-c-fill-paragraph arg)
        (setq new-str (buffer-string))))
    (delete-region beg (+ beg (length str)))
    (insert new-str)
    (goto-char (+ beg pos))))

(bz/hook web-mode-hook bz/web-mode-setup
  (setq-local forward-sexp-function 'bz/jsx-forward-sexp)
  (when (string-match-p "\\.\\(tsx?\\|jsx?\\)$" buffer-file-name)
    (setq-local fill-paragraph-function 'bz/web-typescript-fill-paragraph)))

(defun bz/web-mode-delete-pair ()
  (interactive)
  (if (not (looking-at-p "<")) (bz/paren-delete)
    (atomic-change-group
      (save-excursion
        (bz/jsx-forward-sexp)
        (delete-region (point) (progn (search-forward-regexp "\\=</[a-zA-Z0-9.]*>") (point)))
        (goto-char (pos-bol))
        (when (looking-at-p " *$") (delete-region (point) (1+ (pos-eol)))))
      (delete-region (point) (or (search-forward ">" (pos-eol) t) (1+ (pos-eol)))))))

(defun bz/web-mode-replace-pair ()
  (interactive)
  (if (not (looking-at-p "<")) (call-interactively #'bz/paren-replace)
    (save-excursion
      (bz/jsx-forward-sexp)
      (search-forward-regexp "\\=</\\([a-zA-Z0-9.]*\\)>")
      (goto-char (match-beginning 1))
      (delete-region (point) (match-end 1))
      (mc2-add-cursor))
    (forward-char)
    (search-forward-regexp "\\=[a-zA-Z0-9.]*")
    (delete-region (match-beginning 0) (point))
    (mc2-add-cursor)
    (mc2-all)
    (bz/insert)))


;;; Snippets

(defun bz/javascript-make-doc-comment ()
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "^[ \t]*//+ *"))
      (bz/insert-snippet "/**\n * <<>>\n */")

    (end-of-line)
    (save-excursion
      ;; Replace current line
      (beginning-of-line)
      (delete-region (match-beginning 0) (match-end 0))
      (insert " * ")
      ;; Search back
      (save-excursion
        (beginning-of-line)
        (while (looking-back "^\\([ \t]*//+ *\\).*\n")
          (forward-line -1)
          (delete-region (match-beginning 1) (match-end 1))
          (insert " * ")
          (beginning-of-line))
        (insert "/**\n"))
      ;; Search forward
      (while (looking-at ".*\n\\([ \t]*//+ *\\)")
        (forward-line 1)
        (delete-region (match-beginning 1) (match-end 1))
        (insert " * "))
      (end-of-line)
      (insert "\n */"))))

(defun bz/javascript-make-line-comment ()
  (end-of-line)
  (if (not (search-backward-regexp "^[ \t]*/\\*.*\\(\n[ \t]*\\*.*\\)*\\=" nil t))
      (progn (beginning-of-line-text) (insert "/**  */") (forward-char -3))

    (delete-line)
    (save-excursion
      (while
          (cond ((looking-at-p "[ \t]*\\*/$") (delete-line) nil)
                ((search-forward-regexp "\\=[ \t]*?\\( ?\\*\\) *" nil t)
                 (delete-region (match-beginning 1) (point))
                 (insert "// ")
                 (forward-line)))))
    (end-of-line)))

(setf (alist-get '(js-mode js-jsx-mode typescript-mode web-mode) bz/snippet-mode-alist nil nil #'equal)
      '(
        ;; ("/" "/*\n *<<>>\n*/")
        ("/" bz/javascript-make-line-comment)
        ("?" bz/javascript-make-doc-comment)
        ))
(setf (alist-get 'go-mode bz/snippet-mode-alist)
      '(
        ;; ("/" "/*\n *<<>>\n*/")
        ("/" bz/javascript-make-line-comment)
        ("?" bz/javascript-make-doc-comment)
        ))


;;; Provide

(provide 'bz-javascript)

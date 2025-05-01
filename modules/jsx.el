(bz/require prettier)

;;; Basic Settings

(setq bz/jsx-major-mode 'js-jsx-mode)
;; (setq bz/jsx-major-mode 'rjsx-mode)

(add-to-list 'auto-mode-alist (cons "\\.tsx\\'" bz/jsx-major-mode))


(setq js-indent-level 4)

;; Disable tree sitter because it freaks out in tsx files
(setq js-mode-hook nil)


;;; Define Snippets

(setf (alist-get bz/jsx-major-mode bz/snippet-mode-alist)
      '(("C" (lambda ()
               (insert "export default function ")
               (insert (file-name-base buffer-file-name))
               (insert "(ps: {}) {\nreturn (\n\n);\n}")))
        ("S" (lambda ()
               (insert "export default function ")
               (insert (file-name-base buffer-file-name))
               (insert "({ navigation, route }: ScreenProps<\"")
               (insert (file-name-base buffer-file-name))
               (insert "\">) {\nreturn (\n<<>>\n);\n}")))

        ("s" " style={}" -1)
        ("S" "const styles = N.StyleSheet.create({\n\n});" -4)

        ("v c" "<View flex={1}></View>" -7)
        ("v r" "<View flex={1} flexDirection=\"row\"></View>" -7)

        ("i m" "import * as M from \"@mui/material\";")
        ("i a" "import * as A from \"../theme/\";")
        ("i n" "import * as N from \"react-native\";")
        ("i p" "import * as P from \"react-native-paper\";")

        ("c" "className=\"<<>>\"")

        ("r d" "<div>\n<<>>\n</div>")

        ("m b" "<M.Button onClick={() => <<>>}></M.Button>")
        ("m t" "<M.Typography><<>></M.Typography>")
        ("m f" "<M.TextField variant=\"outlined\" label="<<>>" onChange={}>")

        ("n b" "<N.Button title=\"\" onPress={() => } />" 17)
        ("n i" "<N.TextInput value={} onChangeText={(text) => } />" 20)
        ("n l" "<N.FlatList\ndata={}\nrenderItem={({ item }) => }\n>\n</N.FlatList>" 18)
        ("n L" "<N.SectionList\nsections={}\nrenderSectionHeader={({ section }) => }\nrenderItem={({ item }) => }\n>\n</N.SectionList>" 25)
        ("n m" "<N.Modal\ntransparent\nvisible={}\nonRequestClose={() => }\nanimationType={slide}\n>\n</N.Modal>" 30)
        ("n p" "<N.Pressable onPress={() => }></N.Pressable>" -14)
        ("n s" "<N.Switch value={} onChangeValue={(val) => } />" 17)
        ("n t" "<N.Text></N.Text>" 8)
        ("n v" "<N.View></N.View>" 8)
        ("n V" "<N.ScrollView></N.ScrollView>" 14)

        ("p b" "<P.Button title=\"\" onPress={() => } />" 17)
        ("p B" "<P.IconButton icon=\"\" onPress={() => } />" 20)
        ("p i" "<P.TextInput value={} onChangeValue={(text) => } />" 20)
        ("p m" "<P.Modal\nvisible={}\nonRequestClose={() => }\nanimationType={slide}\n>\n</P.Modal>" 18)
        ("p s" "<P.Switch value={} onChangeValue={(val) => } />" 17)
        ("p t" "<P.Text variant=\"\"></P.Text>" 17)
        ))


;;; Jsx Sexps

(bz/hook js-jsx-mode-hook bz/enable-jsx-sexps
  (setq forward-sexp-function #'bz/jsx-forward-sexp))

(defun bz/jsx-forward-sexp (&optional arg interactive)
  (let* ((start (point))
         (func (if (and (numberp arg) (< arg 0))
                   #'bz/jsx-backward-tag #'bz/jsx-forward-tag))
         (err (funcall func)))

    (when err
      (goto-char start)
      (let ((forward-sexp-function nil))
        (forward-sexp arg interactive)))))

(defun bz/jsx-forward-tag ()
  (catch 'exit
    (let (tag-name)
      (when (looking-at "[ \t]*<") (goto-char (match-end 0)))
      (unless (looking-back "<[a-z]*>?") (throw 'exit t))
      (goto-char (match-beginning 0))

      (unless (looking-at "<\\([a-z]+\\)") (throw 'exit t))
      (setq tag-name (match-string 1))

      ;; If there is a match on the same line
      (when (search-forward-regexp (concat "</" (match-string 1)) (point-at-eol) 'noerror)
        (goto-char (match-beginning 0))
        (throw 'exit nil))

      (beginning-of-line)
      (unless (looking-at (concat "\\([ \t]*\\)<" tag-name)) (throw 'exit t))
      (forward-line 1)
      (if (search-forward-regexp (concat "^" (match-string 1) "</" tag-name) nil 'noerror)
          (beginning-of-line-text)
        (throw 'exit t)))))

(defun bz/jsx-backward-tag ()
  (catch 'exit
    (let (tag-name)
      (when (looking-at "/\\|[ \t]*</") (goto-char (match-end 0)))
      (unless (looking-back "</[a-z]*>?") (throw 'exit t))
      (goto-char (match-beginning 0))

      (unless (looking-at "</\\([a-z]+\\)") (throw 'exit t))
      (setq tag-name (match-string 1))

      ;; If there is a match on the same line
      (when (search-backward-regexp (concat "<" (match-string 1)) (point-at-bol) 'noerror)
        (goto-char (match-beginning 0))
        (throw 'exit nil))

      (beginning-of-line)
      (unless (looking-at (concat "\\([ \t]*\\)</" tag-name)) (throw 'exit t))
      (if (search-backward-regexp (concat "^" (match-string 1) "<" tag-name) nil 'noerror)
          (beginning-of-line-text)
        (throw 'exit t)))))

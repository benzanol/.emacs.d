;;; Snippets  -*- lexical-binding: t; -*-

(require 'bz-camel-to-snake)


(bz/keys rust-mode-map
  :sparse t)

(bz/face rust-question-mark error)
(bz/face rust-ampersand-face :inherit nil)

(bz/hook rust-mode-hook bz/rust-mode-setup
  (setq-local outline-regexp "// ==")
  (setq-local outline-heading-alist '(("// ==" . 1)))

  (apheleia-mode 1))

(setf (alist-get 'rust-mode bz/snippet-mode-alist)
      '(
        ("a" "'a")
        ("S" "'static")
        ("p 1" "println!(\"{:?}\", <<>>);")
        ("p 2" "println!(\"{:?} {:?}\", <<>>);")
        ("p 3" "println!(\"{:?} {:?} {:?}\", <<>>);")
        ("P" "println!(\"{}\", <<>>);")
        ("D" "println!(\"{:?}\", <<>>);")
        ;; Print multiline string
        ("p m s" "println!(\"{}\", <<>>.iter().map(|line| line.iter().join(\"\")).join(\"\\n\"));")
        ))


;;; Cargo install

(setq bz/cargo-packages nil)

(defun bz/cargo-add ()
  (interactive)

  (let ((path (split-string (expand-file-name buffer-file-name) "/"))
        cargo crate pkg-line cargo-line)
    (while path
      (setq cargo (format "/%s/Cargo.toml" (s-join "/" path)))
      (if (file-exists-p cargo)
          (save-window-excursion
            (setq path nil)
            (find-file cargo)

            ;; Go to last line with text, then create a newline
            (goto-char (point-max))
            (search-backward-regexp ".")
            (end-of-line)
            (newline)

            ;; Add here
            (setq crate (read-string "Crate: "))

            (setq pkg-line ($$ "cargo search --limit 1 %s" crate))

            (string-match "^\\([^ ]+\\) = \"\\([0-9]+\\.[0-9]+\\)" pkg-line)
            (setq cargo-line (format "%s = \"%s\"" (match-string 1 pkg-line) (match-string 2 pkg-line)))

            (insert cargo-line)
            (save-buffer)
            (message cargo-line))

        (setq path (butlast path))
        (unless path (error "No cargo file found!"))))))


;;; Font lock

(bz/font-lock-add-keywords
 'rust-mode
 '(("\\<\\(impl\\)\\>"
    (1 '(font-lock-function-name-face bold)))))


;;; Provide

(provide 'bz-rust)

;;; Snippets

(setf (alist-get 'rust-mode bz/snippet-mode-alist)
      '(
        ("a" "'a")
        ("S" "'static")
        ("p 1" "println!(\"{:?}\", );" -2)
        ("p 2" "println!(\"{:?} {:?}\", );" -2)
        ("p 3" "println!(\"{:?} {:?} {:?}\", );" -2)
        ("P" "println!(\"{}\", );" -2)
        ("D" "println!(\"{:?}\", );" -2)
        ;; Print multiline string
        ("p m s" "println!(\"{}\", .iter().map(|line| line.iter().join(\"\")).join(\"\\n\"));" 15)
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

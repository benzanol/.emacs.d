;; -*- lexical-binding:t -*-

(bz/require magit)
(bz/require dired)
(bz/require vterm)
(bz/require activities)
(bz/require lsp)

(defvar bz/lsp-directories nil)

(bz/hook (rust-mode-hook typescript-mode-hook js-jsx-mode-hook svelte-mode-hook dart-mode-hook) bz/prj-enable-lsp
  (when buffer-file-name
    (let ((buf-file (expand-file-name buffer-file-name)))
      (when (--any (s-starts-with-p (expand-file-name it) buf-file) bz/lsp-directories)
        (lsp)))))

;; Tree sitter errors when opening jsx files
(require 'js)
(require 'tree-sitter)
(remove-hook 'js-mode-hook 'tree-sitter-mode)
(remove-hook 'js-mode-hook 'tree-sitter-hl-mode)

(setq bz/prj-langs
      '((##)

        (python :extension "py" :lsp nil :run "python3 %s.py")
        (lua :extension "lua" :lsp nil :run "lua %s.lua")
        (swift :extension "swift" :lsp nil :run "swift %s.swift")
        (flutter :dir "Flutter" :lsp t :run ?r :alt-run ?R
                 :init "flutter create ." :eventually "./lib/main.dart" )
        (dart :dir "Dart" :init "dart create -t console-simple '%s'" :lsp t :run "dart 'bin/%s.dart'")
        (go :dir "Go" :lsp t :extension "go")

        (rust :lsp t :run "cargo run" :eventually "src/main.rs"  :init "cargo init")
        (serenity :dir "Rust" :lsp t :run "cargo run" :eventually "src/main.rs"  :template "serenity")
        (wasm :dir "Wasm" :lsp t :init "cargo init --lib" :eventually "src/lib.rs"  :template "wasm"
              :run "wasm-pack build --target web ; python3 -m http.server 8080")

        (web :dir "Node" :template "web" :run "python3 -m http.server 8080")
        (javascript :dir "Node" :extension "js" :lsp nil :run "node %s.js")
        (typescript :dir "Node" :lsp t :run "ts-node ./src/main.ts"
                    :template "typescript" :eventually "src/main.ts")
        (reactnative :dir "ReactNative" :lsp t :run "echo 'npx react-native start' | db enter debian1"
                     :init "echo '%s' | npx react-native init --template react-native-template-typescript && mv */{*,.*} . ; rm -rf '%1$s'"
                     :eventually "./App.tsx")
        (discord-ts :dir "Node/DiscordBots" :lsp t :run "tsc && node ./build/main.js"
                    :template "discord-ts" :eventually "src/main.ts")
        (svelte :dir "Web" :template "svelte" :init "npm init vite@latest" :lsp t :run "ns run android")
        (sveltenative :dir "SvelteNative" :template "sveltenative" :lsp t :run "ns run android")

        ;; (vite :dir "Web" :template "svelte" :init "npm init vite@latest" :lsp t :run "ns run android")
        ))

(defun bz/prj-open (lang test path)
  (interactive
   (let* ((lang-str (completing-read "Language: " (mapcar 'car bz/prj-langs)))
          (test (if (string= lang-str "") t (not (y-or-n-p "Real project?"))))
          (dir-path (concat (if test "~/Test/" "~/Documents/Programming/")
                            (or (plist-get (alist-get (intern lang-str) bz/prj-langs) :dir)
                                (upcase-initials lang-str))
                            "/"))
          (path (read-directory-name "Project: " (if (string= lang-str "") "~/" dir-path))))
     (list (intern lang-str) test path)))

  (let* ((proj-name (car (last (split-string path "/" t))))
         (windows (list nil nil nil))
         (activity-name (format "%s:%s" lang proj-name))
         (term-name (format "*prj-term %s*" activity-name))
         (template-dir "~/Documents/Programming/Templates/")
         (needs-init nil)

         (lang-plist (alist-get lang bz/prj-langs))
         (p-init (plist-get lang-plist :init))
         (p-template (plist-get lang-plist :template))
         (p-extension (plist-get lang-plist :extension))
         (p-eventually (plist-get lang-plist :eventually))
         (p-lsp (plist-get lang-plist :lsp))
         (eventually-file (when p-eventually (format "%s/%s" path p-eventually)))
         (def-file (when p-extension (format "%s/%s.%s" path proj-name p-extension)))

         (act-plist (list :path path :name proj-name
                          :run (plist-get lang-plist :run) :alt-run (plist-get lang-plist :alt-run))))

    ;; Create the folder if it doesn't already exist
    (unless (file-exists-p path)
      (mkdir path t)
      (setq needs-init t))

    ;; If init is a function, call it
    (when (and needs-init (functionp p-init))
      (funcall p-init))

    ;; If a template directory is specified, copy the contents
    (when (and needs-init p-template)
      (f-copy-contents (concat template-dir p-template) path))

    ;; Add the folder to lsp directories
    (when (and p-lsp (not (member (expand-file-name path) bz/lsp-directories)))
      (push (expand-file-name path) bz/lsp-directories))

    ;; Create the default file
    (when def-file (write-region "" nil def-file))

    ;; Create a new activity
    (if (assoc (upcase-initials activity-name) bz/activities)
        (bz/switch-to-activity activity-name)
      (bz/add-activity activity-name))

    ;; Create a window layout so that `windows` contains the left,
    ;; middle, and right window in that order
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (setf (nth 0 windows) (selected-window))
    (setf (nth 2 windows) (split-window-right 30))
    (select-window (nth 2 windows))
    (setf (nth 1 windows) (or (ignore-errors (split-window nil 90 'left))
                              (split-window nil nil 'left)))


    ;; Create dired and make it fixed width
    (select-window (nth 0 windows))
    (dired path)
    (plist-put act-plist :dired (current-buffer))
    (setq-local window-size-fixed 'width)
    (when p-lsp (ignore-errors (bz/lsp-dired-mode 1)))

    ;; Create the terminal
    (select-window (nth 2 windows))
    (if (get-buffer term-name)
        (switch-to-buffer term-name)
      ;; So that it opens in the correct directory
      (set-buffer (window-buffer (nth 0 windows)))
      (multi-vterm)
      (rename-buffer term-name))
    (plist-put act-plist :terminal (current-buffer))

    ;; Send an init command to the terminal
    (when (and needs-init (stringp p-init))
      (process-send-string (get-buffer-process (current-buffer))
                           (concat (format p-init proj-name) "")))

    ;; Open magit if not in a test
    (unless test
      (select-window (nth 0 windows))
      (select-window (split-window-below))
      (magit-init path))


    ;; Go back to dired
    (select-window (nth 0 windows))
    (setq bz/last-window (nth 1 windows))

    ;; If there is a default file, open it
    (when def-file (bz/dired-open def-file))

    (when eventually-file
      (bz/dired-eventually-open-file
       eventually-file (plist-get act-plist :dired)))

    ;; Put the plist into the activity
    (setcdr (assoc 'plist bz/current-activity) act-plist)))

(bz/keys *
  "C-x C-o" bz/prj-open
  "<C-return>" bz/prj-run
  "<C-M-return>" (@ bz/prj-alt-run (bz/prj-run t)))

(defun bz/prj-run (&optional alt)
  (interactive)
  (if-let* ((plist (alist-get 'plist (cdr bz/current-activity)))
            (run (plist-get plist (if alt :alt-run :run)))
            (process (get-buffer-process (or (and alt (plist-get plist :alt-terminal)) (plist-get plist :terminal)))))

      (cond ((stringp run)
             (let* ((name (plist-get plist :name))
                    (cmd (format run name)))
               (process-send-string process (format "%s" cmd))))

            ((functionp run) (funcall run))

            ((numberp run) (process-send-string process (string run)))

            (t (message "No run command found")))))


(setq bz/max-wait-for-eventually-file 60)
(defun bz/dired-eventually-open-file (file dired-buf &optional depth)
  (setq depth (or depth 1))
  (when (< depth (* 2 bz/max-wait-for-eventually-file))
    (if (file-exists-p file)
        (with-current-buffer dired-buf
          (dired-revert) (bz/dired-open file))
      (run-with-timer 0.5 nil 'bz/dired-eventually-open-file
                      file dired-buf (1+ depth)))))


(defun bz/prj-web-run ()
  (interactive)

  (let ((win (selected-window)))
    (select-window (get-buffer-window (format ":%s:browser:" (downcase (car bz/current-activity)))))
    ($ "sleep 0.15; xdotool key Control+r")
    (run-with-timer 0.4 nil #'select-window win)))

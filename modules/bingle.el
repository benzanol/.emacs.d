;; -*- lexical-binding: t; -*-

(require 'bz-base)


(add-to-list 'load-path "~/.emacs.d/my-packages/bingle" t)
(require 'bingle)

;; (cancel-function-timers #'bz/abort-recursive-edit)
;; (run-with-timer nil 10
;;                 (defun bz/abort-recursive-edit ()
;;                   (unless (active-minibuffer-window)
;;                     (ignore-errors (abort-recursive-edit)))))

(bz/keys bingle-mode-map
  [remap bz/q] bingle-info-posframe
  [remap bz/Q] bingle-info-posframe-focus
  "C-c C-i" bingle-auto-import
  )

(bz/keys bingle-posframe-map
  [remap bz/q] bingle-info-posframe-hide)

(bz/hook (typescript-mode-hook web-mode-hook) bz/bingle-enable-maybe
  :remove
  (when (and buffer-file-name (s-starts-with-p "ts" (file-name-extension buffer-file-name)))
    (let ((sess (bingle-session-at-path buffer-file-name)))
      (if sess (bingle-mode sess) (run-with-timer 1 nil #'call-interactively #'bingle-mode)))))

(bz/hook after-save-hook bz/bingle-after-save-hook :remove
         (when (bingle-ensure :active :noerror)
           (bingle-reload-file)))

(defvar bz/tsserver "/nix/store/9b483mcq6f0p9xicik806cgvv7jf4qs0-typescript-5.6.3/lib/node_modules/typescript/./bin/tsserver")

(defun bz/use-npx-tsserver (path)
  (when-let* ((pkg (ignore-errors (f-read (f-join path "package.json")))))
    (when (string-match-p "\"typescript\":" pkg)
      t)))

(setf (alist-get 'tsserver bingle-configurations)
      (list :command ;; (list "node" "--max-old-space-size=1024" bz/tsserver)
            (list "tsserver")
            :modes '(typescript-mode web-mode)
            :dir-pred (lambda (path) (and (member "package.json" (directory-files path))
                                          (not (bz/use-npx-tsserver path))))))

(setf (alist-get 'npx-tsserver bingle-configurations)
      (list :command ;; (list "node" "--max-old-space-size=1024" bz/tsserver)
            (list "npx" "tsserver")
            :modes '(typescript-mode web-mode)
            :dir-pred (lambda (path) (and (member "package.json" (directory-files path))
                                          (bz/use-npx-tsserver path)))
            :preferences '(:autoImportFileExcludePatterns
                           [
                            ;; "**/src/ui/core/**"
                            ;; "**/src/ui/layout/**"
                            ;; "**/src/ui/logical/**"
                            ;; "lucide-react"
                            ;; "**/node_modules/lucide-react/**"
                            ])))


;;; Testing
;;;; Testing the partial evaluation of messages

;; (bingle-shutdown mybingle)
;; (setq mybingle (bingle--init "." 'typescript))

;; (bingle--process-filter (plist-get mybingle :process) "Content-Length: ")
;; (bingle--process-filter (plist-get mybingle :process) "11\r\n\r\n")
;; (bingle--process-filter (plist-get mybingle :process) "{\"abc\": 0}")
;; (bingle--process-filter (plist-get mybingle :process) "\nContent-")
;; (bingle--process-filter (plist-get mybingle :process) "Length: 10\r\n\r\n")
;; (bingle--process-filter (plist-get mybingle :process) "{\"a\": 44}\n")


;;; Provide

(provide 'bz-bingle)

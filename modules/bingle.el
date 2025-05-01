(load-file "~/.emacs.d/my-packages/bingle/bingle.el")


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

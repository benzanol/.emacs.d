;; Some comment!!!!!!
(bz/key * "<f6>" lens-remove)

(bz/key *
  "<f8>"
  (lens-create
   (mark) (point)
   `(ui :dummy ,bz/dummy
        :state-fn (lambda (str) `(:text ,(string-trim (upcase str)) :exclaims 1))
        :text-fn (lambda (s) (concat "\n" (downcase (plist-get s :text)) "\n"))
        :ui-fn (lambda (state)
                 `((columns ((string "This is the title!")
                             (button ,(format "Add Exclaim%s" (make-string (plist-get state :exclaims) ?!))
                                     :onclick (lambda (s) (plist-put s :exclaims (1+ (plist-get s :exclaims)))))))
                   (box ,(string-trim (plist-get state :text)) :onchange (lambda (s str) (plist-put s :text (upcase str)))))
                 )
        )))


(bz/advise :remove flycheck-report-buffer-checker-status bz/redisplay-lenses (check status &optional data)
  (when (eq status 'finished)
    (mapc #'lens--redisplay-lens lens--buffer-lenses)))


(bz/key *
  "<f5>" (lens-create (mark) (point) `(buffer "./temp1.el")))

(bz/key *
  "<f7>" (lens-create (mark) (point) `(buffer "~/.emacs.d/my-packages/lens/test.org"
                                              :face variable-pitch)))


(lens-generate-bar-graph '("a" "b" "cccccccc" "x" "y" "z") '(1.1 1.2 7.3 1.4 1.5 1.6) :bar-width 2 :width 4 :spacing 0)

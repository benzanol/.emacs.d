;; JK JSON!!

(
 {
 style: bold,
 content:
 [
  "Whaa!"
  ]
 },
 "hello"
 )

(defun fmt-insert (fmt &optional face)
  (dolist (node fmt)
    (if (stringp node) (insert (propertize node 'face face))
      (fmt-insert (plist-get node :content)
                  (append (plist-get node :style) face)))))

(fmt-insert
 '("hello "
   (:style (:weight bold) :content
           ("th"
            (:style (:height 20.0) :content ("e"))
            "re"))
   " friend")
 )

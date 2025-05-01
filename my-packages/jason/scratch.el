(st--intern
 '(:type
   rows
   :prefix "-- "
   :divider "<<--->>"
   :children
   ((:type text :content "Hello there")
    (:type text :content "Hi again"))
   ))


(887756893108999158
 (:type rows :prefix "-- " :divider "<<--->>" :children
        (86248359977093067 2254941752706946473))
 2254941752706946473
 (:type text :content "Hi again")
 86248359977093067
 (:type text :content "Hellooasdflkj\n tasflkj here"))


(st-deftype note
  :title
  [:text :default ""]
  [:margin :default 5]

  )

(st-deftype event
  :title
  [:text :default ""]
  [:margin :default 5]

  )

st-type-alist

(st-deftype note
  :dom
  (st:div (st:label (plist-get =state= :title) :weight bold :underline t)
          (st:text (plist-get =state= :text))))


(defun st-create:note (title text)
  (interactive "sTitle: \nsText: ")
  (st-new-state 'note (list :title title :text text)))


(st-deftype group
  :dom
  (-let* (((&plist :children cs :divider div) =state=)
          (div-dom (list :type 'const :content div)))
    (list :type 'div :children
          (if (null div) (mapcar #'st-dom cs)
            (--reduce-from (cons div-dom (cons (st-dom it) acc))
                           (list div-dom) cs)))))

(defun st-create:group (children &optional divider)
  (st-new-state 'group (list :children children :divider divider)))



(let ((f (plist-get (alist-get 'note st-type-alist) :update)))
  (funcall f nil :state 123))

(setq a nil)
(setf (plist-get a :text) "abc")




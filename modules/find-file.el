(defun qv/with-home-directory (func &rest args)
  (let ((default-directory (expand-file-name "~/")))
    (apply func args)))

(advice-add 'read-file-name :around 'qv/with-home-directory)

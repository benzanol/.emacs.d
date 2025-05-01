(qv/advise :around read-file-name qv/with-home-directory (func &rest args)
  (let ((default-directory (expand-file-name "~/")))
    (apply func args)))

(qv/package highlight-indent-guides)
;;(setq highlight-indent-guides-method 'character
;;      highlight-indent-guides-character "│")
(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-bitmap-function
      'highlight-indent-guides--bitmap-line)

(highlight-indent-guides-mode)


(qv/package ivy)

(ivy-mode 0)
(qv/face ivy-current-match highlight)

(qv/keys ivy-minibuffer-map
  [remap qv/down] ivy-next-line
  [remap qv/up] ivy-previous-line)

(qv/package ivy-posframe)

(ivy-posframe-mode 0)
(setq ivy-posframe-style "frame-top-center")

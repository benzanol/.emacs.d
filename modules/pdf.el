(qv/package pdf-view)
(qv/package pdf-tools)

(qv/keys pdf-view-mode-map
  [remap qv/down] pdf-view-next-line-or-next-page
  [remap qv/up] pdf-view-previous-line-or-previous-page
  [remap qv/down4] (pdf-view-next-line-or-next-page 4)
  [remap qv/up4] (pdf-view-previous-line-or-previous-page 4))

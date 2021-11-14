(qv/package emms)

(setq emms-directory (expand-file-name "~/.emacs/cache/emms"))

(emms-standard)
(emms-default-players)
(emms-mode-line-mode 0)

(qv/hook emms-playlist-mode-hook nil
  (display-line-numbers-mode 0))

(qv/face emms-playlist-track-face)
(qv/face emms-playlist-selected-face highlight)

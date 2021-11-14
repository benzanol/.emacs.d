;;;; Disable Browsers
(start-process-shell-command "" nil (expand-file-name "~/.bin/disable-browsers.sh"))

;;;; Caps Lock as Control
(shell-command "xmodmap ~/.Xmodmap")

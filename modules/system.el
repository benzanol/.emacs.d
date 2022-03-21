;;; Caps Lock as Control
(shell-command "xmodmap ~/.Xmodmap")

;;; Key Repeat
(shell-command "xset r rate 250 20")

;;; Battery saving mode
;;(shell-command "sudo tlp bat")
;;(shell-command "echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo")
;;(shell-command "sudo cpupower frequency-set -u 400MHz")

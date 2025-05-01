#!/bin/sh
# exec dbus-launch --exit-with-session emacs -mm --debug-init
emacs --debug-init -e bz/load-window-manager

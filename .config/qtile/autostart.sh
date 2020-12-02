#!/usr/bin/env bash

# Appearance
picom &
feh --no-fehbg --bg-fill "$HOME/.wallpapers/cyan aurora.jpg"

# Daemons
emacs --daemon &
onedrive --monitor &
thunar --daemon &
urxvtd &
clipcatd

# Misc
lxsession &
setxkbmap -option caps:swapescape -option compose:rctrl &
# xset m 0/0 0 &
# xinput set-prop 8 295 0, 0 &
# xinput set-prop 8 292 1 &
# xset s off

sleep 2

#!/usr/bin/env bash

export QT_STYLE_OVERRIDE=breeze
picom &
setxkbmap -option caps:swapescape -option compose:rctrl &
lxsession &
feh --no-fehbg --bg-fill "$HOME/.wallpapers/cyan aurora.jpg" &
emacs --daemon &
urxvtd &
onedrive --monitor &
# xset m 0/0 0 &
# xinput set-prop 8 295 0, 0 &
# xinput set-prop 8 292 1 &
xset s off &
clipcatd &

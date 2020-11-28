#!/usr/bin/env bash

export QT_STYLE_OVERRIDE=breeze
picom --no-fading-openclose --active-opacity .99 --inactive-opacity .90 --vsync &
setxkbmap -option caps:swapescape -option compose:rctrl &
lxsession &
feh --no-fehbg --bg-fill "$HOME/.wallpapers/cyan aurora.jpg" &
emacs --daemon &
urxvtd &

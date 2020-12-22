#!/usr/bin/env sh

sleep 2

# Appearance
picom --daemon
feh --no-fehbg --bg-fill "$HOME/.wallpapers/snowy mountains.jpg"
redshift-gtk -l geoclue2 &

# Daemons
emacs --daemon &
thunar --daemon &
urxvtd -o -f
#clipcatd --no-daemon &
greenclip daemon &
twmnd &

# Misc
nm-applet &
lxsession --de=Qtile &
setxkbmap -option caps:swapescape -option compose:rctrl
# xset m 0/0 0 &
# xinput set-prop 8 295 0, 0 &
# xinput set-prop 8 292 1 &
xset s off
xset dpms 0 0 0
numlockx on

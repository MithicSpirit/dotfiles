#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00

sleep 1.5

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
checkupdates --download &

sleep .5
notify-send "Welcome"

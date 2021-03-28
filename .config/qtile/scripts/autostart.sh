#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00

sleep 1.5

# Appearance
picom --daemon &
feh --no-fehbg --bg-fill "$HOME/.wallpapers/snowy mountains.jpg" &
#redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
xrdb ~/.config/xresources/Xresources &

# Daemons
/usr/lib/geoclue-2.0/demos/agent &
#emacs --daemon &
thunar --daemon &
#urxvtd -o -f &
#greenclip daemon &
#twmnd &
play-with-mpv &
flameshot &

# Misc
nm-applet &
lxsession --de=Qtile &
setxkbmap -option caps:swapescape -option compose:rctrl &
#xset m 0/0 0 &
#xinput set-prop 8 295 0, 0 &
#xinput set-prop 8 292 1 &
xset s 0 0 &
xset dpms 0 0 0 &
numlockx on &
radeon-profile &

sleep .5
notify-send "Welcome"

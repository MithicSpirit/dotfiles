#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
#xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00

{
	. ~/.profile
	
	echo -n "Qtile startup at "
	date
	
	# Appearance
	sleep 1
	picom --experimental-backends &
	feh --no-fehbg --bg-fill "$HOME/.wallpapers/snowy mountains.jpg" &
	redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
	xrdb ~/.config/xresources/Xresources &

	# Daemons
	sleep 4
	/usr/lib/geoclue-2.0/demos/agent &
	greenclip daemon >/tmp/greenclip.log &
	flameshot &
	/usr/lib/xfce-polkit/xfce-polkit &
	[ "$REAL_GPU" != "none" ] && replay-sorcery &
	sxhkd &

	# Misc
	sleep 1
	nm-applet &
	xset m 0/0 0 &
	xset s 0 0 &
	xset +dpms dpms 0 0 0 &
	numlockx on &
	wacom margin &
	mailspring --background &
	
	sleep 4
	notify-send "Welcome" &
} >>/tmp/qtile-autostart.log 2>&1 &

#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
#xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00

{
	. ~/.profile
	
	echo -n "Qtile startup at "
	date

	sleep 2
	
	# Appearance
	picom --experimental-backends &
	feh --no-fehbg --bg-fill "$HOME/.local/share/backgrounds/selected" &
	redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
	xrdb ~/.config/xresources/Xresources &

	# Daemons
	/usr/lib/geoclue-2.0/demos/agent &
	greenclip daemon >/tmp/greenclip.log &
	flameshot &
	/usr/lib/xfce-polkit/xfce-polkit &
	[ "$REAL_GPU" != "none" ] && replay-sorcery &
	sxhkd -t 1000 -r /tmp/sxhkd.log &
	/usr/bin/kdeconnect-indicator &
	xautolock &

	# Misc
	nm-applet &
	xset m 0/0 0 &
	xset s 0 0 &
	xset +dpms dpms 0 0 0 &
	numlockx on &
	#wacom margin &
	nice -n5 evolution &
	
	sleep 2
	systemctl --user restart dunst.service
	notify-send "Welcome" &
} >>/tmp/qtile-autostart.log 2>&1 &

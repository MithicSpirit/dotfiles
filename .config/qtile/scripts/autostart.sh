#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
#xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00


startup() {
	echo -n "Qtile startup at "
	date
	sleep 1
	# Appearance
	picom --experimental-backends &
	feh --no-fehbg --bg-fill "$HOME/.wallpapers/snowy mountains.jpg" &
	redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
	xrdb ~/.config/xresources/Xresources &

	sleep 1.5
	# Daemons
	/usr/lib/geoclue-2.0/demos/agent &
	#emacs --daemon &
	#nice -n1 thunar --daemon &
	#urxvtd -o -f &
	greenclip daemon >/tmp/greenclip.log &
	#dunst &
	#play-with-mpv &
	flameshot &
	/usr/lib/xfce-polkit/xfce-polkit &
	#start-pulseaudio-x11 &
	#nice -n2 /opt/LBRY/lbry --hidden &
	[ "$NO_REPLAY_SORCERY" != "true" ] && replay-sorcery &

	# Misc
	sxhkd &
	nm-applet &
	#lxsession --de=Qtile &
	setxkbmap -option caps:escape_shifted_capslock -option compose:rctrl &
	xset m 0/0 0 &
	#xinput set-prop 8 295 0, 0 &
	#xinput set-prop 8 292 1 &
	xset s 0 0 &
	xset +dpms dpms 0 0 0 &
	numlockx on &
	wacom margin &
	#radeon-profile &
	mailspring --background &
}

greet() {
	sleep 4
	notify-send "Welcome" &
}

full() {
	startup
	greet
}

sleep 0.5
full >>/tmp/qtile-autostart.log 2>&1 &

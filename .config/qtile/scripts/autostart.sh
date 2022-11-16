#!/usr/bin/env sh

# Xrandr (I plan on moving this to system config in /etc soon™)
#xrandr --output HDMI-A-0 --mode 1920x1080 --rate 60.00

{
	. ~/.profile
	
	echo -n "Qtile startup at "
	date

	sleep 2
	
	# Appearance
	picom &
	feh --no-fehbg --bg-fill "$HOME/.local/share/backgrounds/selected" &
	redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
	xrdb ~/.config/xresources/Xresources &
	#xsettingsd &

	# Daemons
	/usr/lib/geoclue-2.0/demos/agent &
	greenclip daemon >>/tmp/greenclip.log &
	XDG_CURRENT_DESKTOP="" QT_QPA_PLATFORMTHEME="gtk2" flameshot &
	#/usr/lib/xfce-polkit/xfce-polkit &
	/usr/lib/polkit-kde-authentication-agent-1 &
	kwalletd5 &
	(sleep 1; /usr/lib/pam_kwallet_init) &
	[ "$REAL_GPU" != "none" ] && replay-sorcery >>/tmp/replay-sorcery.log &
	sxhkd -t 1000 -r /tmp/sxhkd.log &
	XDG_CURRENT_DESKTOP="" QT_QPA_PLATFORMTHEME="gtk2" kdeconnect-indicator &
	xautolock &
	systemctl --user restart emacs.service &
	kded5 &
	{
		sleep 1
		for module in \
			gtkconfig \
			ktimezoned \
			kded_accounts \
			networkmanagement
		do
			echo -n "Loading module $module: "
			qdbus org.kde.kded5 /kded org.kde.kded5.loadModule "$module"
		done
	}&

	# Misc
	nm-applet &
	xset m 0/0 0 &
	xset s 0 0 &
	xset +dpms dpms 0 0 0 &
	numlockx on &
	# wacom margin &
	nice -n5 evolution &
	
	sleep 2
	#systemctl --user restart dunst.service
	dunst -print >>/tmp/dunst.log &
	notify-send "Welcome" &
} >>/tmp/qtile-autostart.log 2>&1 &

#!/usr/bin/env sh

{

# shellcheck disable=SC1090
. ~/.profile
printf "Qtile startup at "
date

xrdb ~/.config/xresources/Xresources &
/usr/lib/polkit-kde-authentication-agent-1 &
sxhkd -t 1500 -r /tmp/sxhkd.log &
{
	if pgrep -u "$USER" kwalletd5
	then echo "Starting kwallet"; unkrun kwalletd5
	else echo "Starting pam_kwallet_init"; unkrun /usr/lib/pam_kwallet_init
	fi
} &
/usr/lib/geoclue-2.0/demos/agent &
xset m 0/0 0 &
xset s 0 0 &
xset +dpms dpms 0 0 0 &
numlockx on &
picom &
setbackground &

sleep 0.5
kded5 &
#systemctl --user restart emacs.service &
xautolock &
greenclip daemon >>/tmp/greenclip.log &
redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
unkrun kdeconnect-indicator &
unkrun flameshot &
[ "$REAL_GPU" != "none" ] && replay-sorcery >>/tmp/replay-sorcery.log &

sleep 0.5
for module in \
	gtkconfig \
	ktimezoned \
	kded_accounts \
	networkmanagement
do
	qdbus org.kde.kded5 /kded org.kde.kded5.loadModule "$module" &
done
nm-applet &
nice -n5 discord-canary >>/tmp/discord.log 2>&1 &
nice -n5 signal-desktop >>/tmp/signal.log 2>&1 &
nice -n5 element-desktop >>/tmp/element.log 2>&1 &

#nice -n5 evolution >>/tmp/evolution.log 2>&1 &
nice -n5 kmail >>/tmp/kmail.log 2>&1 &
nice -n5 kalendar >>/tmp/kalendar.log 2>&1 &
#{
#	{
#		echo "RMing"
#		rm -rf "$XDG_DATA_HOME/akonadi/file_db_data"
#		rm -rf "$XDG_DATA_HOME/akonadi/file_lost+found"
#		echo "Starting akonadi"
#		akonadictl start
#		sleep 1.0
#		echo "Running vacuum"
#		akonadictl vacuum
#		echo "Running fsck"
#		akonadictl fsck
#	} >>/tmp/akonadi.log 2>&1
#
#	nice -n5 kmail >>/tmp/kmail.log 2>&1 &
#	nice -n5 kalendar >>/tmp/kalendar.log 2>&1 &
#} &
nice -n5 lbry >>/tmp/lbry.log 2>&1 &
# shellcheck disable=SC2086
nice -n5 $TERMINAL --class btop-spawn -t btop -e btop \
	>>/tmp/alacritty-btop.log 2>&1 &

sleep 1.0
akonadictl fsck &
dunst -print >>/tmp/dunst.log &
sleep 0.1
notify-send "Welcome, $USER!" &

} >>/tmp/qtile-autostart.log 2>&1 &

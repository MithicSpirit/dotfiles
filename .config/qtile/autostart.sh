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
tmux -D &
redshift-gtk -l geoclue2 -b 0.95:0.9 -t 6400K:4000K &
unkrun kdeconnect-indicator &
unkrun flameshot &
[ "$REAL_GPU" != "none" ] && replay-sorcery >>/tmp/replay-sorcery.log &
#easyeffects --gapplication-service &

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

nice -n5 kmail >>/tmp/akonadi.log 2>&1 &
nice -n5 korganizer >>/tmp/akonadi.log 2>&1 &
#nice -n5 lbry >>/tmp/lbry.log 2>&1 &
# shellcheck disable=SC2086
nice -n5 $TERMINAL --class btop-spawn -t btop -e btop \
	>>/tmp/alacritty-btop.log 2>&1 &

sleep 2.0
{
	akonadictl vacuum
	akonadictl fsck
} >>/tmp/akonadi.log 2>&1 &

sleep 3.0
killall dunst &
sleep 0.1
dunst -print >>/tmp/dunst.log &
sleep 0.1
notify-send "Welcome, $USER!" &


} >>/tmp/qtile-autostart.log 2>&1 &

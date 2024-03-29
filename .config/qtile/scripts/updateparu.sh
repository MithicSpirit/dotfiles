#!/usr/bin/env zsh

unset LINES COLUMNS

clear_input() {
	while true; do
		read -t 0 -sk 1 || break
	done
	return 0
}

success() {
	clear_input
	notify-send "Update done" "Success"
	read -rsk 1 '?Done [Press any key to exit]'
	exit 0
}

fail() {
	local errorcode=$?
	clear_input
	notify-send "Update done" "Failed"
	echo -n "Error: Exit code $errorcode "
	while true; do
		[[ "$(read -re "?[Type \`exit\` to exit] ")" =~ ".*[Ee][Xx][Ii][Tt].*" ]] && 
			break
	done
	exit $errorcode
}

echo "Main:"
checkupdates
echo "\nAUR:"
paru -Qua --color=always | grep --color=never -v '\[ignored\]'
echo ""
clear_input
read -rk 1 'OPTION?Proceed? [y/r/a/N] '
echo ""
if [[ $OPTION == "y" || $OPTION == "Y" ]]; then
	sudo -v &&
		time nice -n20 paru -Syu --noconfirm --repo &&
		( (sleep 7 ; notify-send "Repo update done" "Please accept AUR update") &
			clear_input
			time nice -n20 paru -Su --aur 
			) && success || fail
elif [[ $OPTION == "r" || $OPTION == "R" ]]; then
	sudo -v &&
		time nice -n20 paru -Syyu --noconfirm --repo &&
		success || fail
elif [[ $OPTION == "a" || $OPTION == "A" ]]; then
	sudo -v &&
		time nice -n20 paru -Su --aur &&
		success || fail
else
	echo -n "Aborted"
	sleep 1
	exit 1
fi

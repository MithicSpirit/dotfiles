#!/usr/bin/env zsh

echo "Main:"
sleep 1
checkupdates
echo "\nAUR:"
checkupdates-aur
echo ""
if [[ $(read -eq '?Proceed? [y/N] ') == "y" ]]; then
	echo ""
	yay -Syu --noconfirm --color=always &&
	read -sk 1 '?Done [Press any key to exit]' ||
	read -sk 1 "?Error: Exit code $? [Press any key to exit]"
else
	echo "\nAborted"
	sleep 1
fi

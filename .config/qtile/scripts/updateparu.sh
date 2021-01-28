#!/usr/bin/env zsh

echo "Main:"
checkupdates
echo "\nAUR:"
#checkupdates-aur
paru -Qua
echo ""
if [[ $(read -req '?Proceed? [y/N] ') == "y" ]]; then
	echo ""
	paru -Syu --noconfirm --nodevel &&
	read -rsk 1 '?Done [Press any key to exit]' ||
	read -rsk 1 "?Error: Exit code $? [Press any key to exit]"
else
	echo "\nAborted"
	sleep 1
fi

#!/usr/bin/env zsh

echo "Main:"
checkupdates
echo "\nAUR:"
paru -Qua --color=always
echo ""
read -rk 1 'OPTION?Proceed? [y/m/r/N] '
echo ""
if [[ $OPTION == "y" || $OPTION == "Y" ]]; then
	paru -Syu --noconfirm &&
	read -rsk 1 '?Done [Press any key to exit]' ||
	read -rsk 1 "?Error: Exit code $? [Press any key to exit]"
elif [[ $OPTION == "m" || $OPTION == "M" ]]; then
	nice -n20 paru -Syu &&
	read -rsk 1 '?Done [Press any key to exit]' ||
	read -rsk 1 "?Error: Exit code $? [Press any key to exit]"
elif [[ $OPTION == "r" || $OPTION == "R" ]]; then
	paru -Syu --noconfirm --repo &&
	read -rsk 1 '?Done [Press any key to exit]' ||
	read -rsk 1 "?Error: Exit code $? [Press any key to exit]"
else
	echo -n "Aborted"
	sleep 1
	exit 1
fi

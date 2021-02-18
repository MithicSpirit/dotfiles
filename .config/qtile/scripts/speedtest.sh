#!/usr/bin/env zsh

speedtest-cli --secure &&
	read -rsk 1 '?[Press any key to exit]' ||
	read -rsk 1 "?Error: Exit code $? [Press any key to exit]"

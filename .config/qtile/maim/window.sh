#!/usr/bin/env bash

maim -ui "$(xdotool getactivewindow)" | xclip -sel clip -t image/png -i

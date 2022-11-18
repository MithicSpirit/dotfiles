#!/usr/bin/env sh
nvim -es -u "$HOME/.config/nvim/init.vim" -i NONE -c "PlugInstall" -c "qa"

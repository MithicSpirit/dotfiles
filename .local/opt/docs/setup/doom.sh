#!/usr/bin/env sh
git clone --depth=1 'https://github.com/hlissner/doom-emacs.git' "$HOME/.config/emacs"
"$HOME/.config/emacs/bin/doom" install

# Skip sourcing these files if powerlevel10k is not available
functions p10k &>/dev/null || return 1

local themesdir file default
themesdir="${0:a:h}"
file="$themesdir/p10k-term-$TERM.zsh"
default="$themesdir/p10k-default.zsh"

if [[ -r "$file" ]]; then
	source "$file"
else
	file="$themesdir/p10k-term-dumb.zsh"
	safesource "$file"
fi

safesource "$default"
safesource "${file%.zsh}-override.zsh"

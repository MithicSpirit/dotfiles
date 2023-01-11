__expand_alias_custom () {
	# Get last word to the left of the cursor:
	# (z) splits into words using shell parsing
	# (A) makes it an array even if there's only one element
	local word=${${(Az)LBUFFER}[-1]}
	if [[ "${LBUFFER[-1]}" != " "  &&
		"${word[1]}" != '\' &&
		"$GLOBALIAS_IGNORE[(Ie)$word]" = 0
		]]; then
		zle _expand_alias
		#zle expand-word # don't want non-aliases
	fi
}; zle -N expand-alias-custom __expand_alias_custom
GLOBALIAS_IGNORE=()

__globalias () {
	zle expand-alias-custom
	zle self-insert
	zle autosuggest-fetch
}; zle -N globalias __globalias

__globalias_newline() {
	zle expand-alias-custom
	zle accept-line
}; zle -N globalias-newline __globalias_newline
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(globalias-newline)


# space expands all aliases, including global
bindkey -M emacs " " globalias
bindkey -M viins " " globalias

# control-space to make a normal space
bindkey -M emacs "^ " magic-space
bindkey -M viins "^ " magic-space

# normal space during searches
bindkey -M isearch " " magic-space

# also apply to RET
bindkey -M emacs "^M" globalias-newline
bindkey -M viins "^M" globalias-newline

# Escape aliases to prevent double-expansion
zmodload zsh/parameter
__escape_aliases() {
	for name expansion in "${(@kv)aliases}"; do
		local word=${${(Az)expansion}[1]}
		[[ -n ${aliases[$word]} ]] && aliases[$name]="\\$expansion"
	done
}

__expandalias() {
    # Get last word to the left of the cursor:
    # (z) splits into words using shell parsing
    # (A) makes it an array even if there's only one element
    local word=${${(Az)LBUFFER}[-1]}
    if [[ ${LBUFFER[-1]} != ' '  &&
        $GLOBALIAS_FILTER_VALUES[(Ie)$word] -eq 0
        ]]; then
        zle _expand_alias
        #zle expand-word # don't want non-aliases
    fi
}
zle -N __expandalias
__globalias() {
    zle __expandalias
    zle self-insert
    zle autosuggest-fetch
}
zle -N __globalias
__globalias_newline() {
    zle __expandalias
    zle accept-line
}
zle -N __globalias_newline

# space expands all aliases, including global
bindkey -M emacs " " __globalias
bindkey -M viins " " __globalias

# control-space to make a normal space
bindkey -M emacs "^ " magic-space
bindkey -M viins "^ " magic-space

# normal space during searches
bindkey -M isearch " " magic-space

# also apply to RET
bindkey -M emacs "^M" __globalias_newline
bindkey -M viins "^M" __globalias_newline

# Escape aliases to prevent double-expansion
__escape_aliases() {
    for name expansion in "${(@kv)aliases}"; do
        local word=${${(Az)expansion}[1]}
        [[ -n ${aliases[$word]} ]] && aliases[$name]="\\$expansion"
    done
}

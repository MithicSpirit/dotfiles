__fg_widget () {
    if [[ "${#BUFFER}" = 0 ]]; then
        BUFFER="fg"
        zle accept-line
    else
        zle push-input
        zle clear-screen
    fi
}
zle -N fg __fg_widget

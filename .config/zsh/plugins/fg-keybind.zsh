__fg_widget () {
	[[ "${#BUFFER}" != "0" ]] && zle push-input
	BUFFER="fg"
	zle accept-line
}
zle -N fg __fg_widget

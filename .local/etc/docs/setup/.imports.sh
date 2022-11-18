#!/hint/sh

safeclone() {
	TARGET="$1"
	shift

	if [ ! -e "$TARGET" ]; then
		git clone --depth=1 "$@" "$1"
	else
		echo "Directory at \`$TARGET\' already exists." \
			"Skipping clone command \`$*\'."
	fi
}

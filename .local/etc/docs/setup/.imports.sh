#!/hint/sh

safeclone() {
	TARGET="$1"
	shift

	if [ ! -e "$TARGET" ]; then
		git clone --depth=1 "$@" "$TARGET"
	else
		echo "Directory at \`$TARGET' already exists." \
			"Skipping clone \`$*'."
	fi
}

#!/hint/sh

export EDITOR="nvim"
export LANG="en_US.UTF-8"
export TERMINAL="alacritty"
export BROWSER="dmenu-browser"
export REAL_BROWSER="librewolf"

export XDG_CURRENT_DESKTOP="KDE"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_BIN_HOME="$HOME/.local/bin"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
[ -r "$XDG_CONFIG_HOME/user-dirs.dirs" ] && . "$XDG_CONFIG_HOME/user-dirs.dirs"
#export XDG_{DOWNLOAD,DOCUMENTS,MUSIC,PICTURES,VIDEOS,PUBLICSHARE,DESKTOP,TEMPLATES}_DIR

export LC_MEASUREMENT="C"
export LC_TIME="C"

export NUM_BUILD_PROCS="10"

export _FASD_DATA="$XDG_DATA_HOME/fasd"
export AGDA_DIR="$XDG_CONFIG_HOME/agda"
export AMD_VULKAN_ICD="RADV"
export BATDIFF_USE_DELTA="true"
export CABAL_CONFIG="$XDG_CONFIG_HOME/cabal/config"
export CABAL_DIR="$XDG_DATA_HOME/cabal"
export CARGO_BUILD_JOBS="$NUM_BUILD_PROCS"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export CHKTEXRC="/home/mithic/.config"
export DIALOGRC="$XDG_CONFIG_HOME/dialogrc"
export GHCUP_USE_XDG_DIRS=1
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$HOME/.local/share/go"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export GTK_THEME=Arc-Dark
export GTK_THEME_VARIANT=dark
export GPG_TTY="$(tty)"
#export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export KDE_SESSION_VERSION=5
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export MOZ_USE_XINPUT2=1
export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"
export PYTHONINTMAXSTRDIGITS=0
export PYTHONTRACEMALLOC=1
#export QT_QPA_PLATFORMTHEME="qt6ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export RIPGREP_CONFIG_PATH="$HOME/.config/rg.conf"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export RXVT_SOCKET="$XDG_RUNTIME_DIR/urxvtd"
export DOT_SAGE="$XDG_CONFIG_HOME/sage"
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SPACEMACSDIR="$HOME/.config/spacemacs"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export STACK_ROOT="$XDG_DATA_HOME/stack"
export STARSHIP_LOG="error"
export VK_ICD_FILENAMES="/usr/share/vulkan/icd.d/radeon_icd.i686.json:/usr/share/vulkan/icd.d/radeon_icd.x86_64.json"
export WINEPREFIX="$XDG_DATA_HOME/wine"
export WINIT_X11_SCALE_FACTOR=1
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
#export XCURSOR_THEME="breeze_cursors"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export PAGER="bat -p --paging=always --theme=ansi"
export MANPAGER="less"
#export DOOMPAGER="$PAGER"

export LESS_TERMCAP_md="$(printf '\033[1;35m')"
export LESS_TERMCAP_so="$(printf '\033[3;33m')"
export LESS_TERMCAP_us="$(printf '\033[4;34m')"
export LESS_TERMCAP_me="$(printf '\033[0m')"
export LESS_TERMCAP_se="$(printf '\033[0m')"
export LESS_TERMCAP_ue="$(printf '\033[0m')"

unset LINES COLUMNS

if [ -z "$__MITHIC_RECURSE_INITIALIZED" ]; then
	export __MITHIC_RECURSE_INITIALIZED="true"

	export PATH="$XDG_CONFIG_HOME/emacs/bin:$XDG_BIN_HOME:$XDG_BIN_HOME/flatpak:$XDG_DATA_HOME/cabal/bin:$PATH"
	export INFOPATH="$HOME/.local/info:$INFOPATH"
	export MANPATH="$HOME/.local/man:/usr/local/man:/usr/share/man:$MANPATH"
	export XDG_DATA_DIRS="$XDG_DATA_DIRS:/usr/local/share:/usr/share"
	export XDG_CONFIG_DIRS="$XDG_CONFIG_DIRS:/etc/xdg"

	export DEBUGINFOD_URLS="https://debuginfod.elfutils.org/ https://debuginfod.archlinux.org/ $DEBUFINFOD_URLS"
	export GRADLE_OPTS=" $GRADLE_OPTS -Dorg.gradle.daemon=false
		-Dorg.gradle.caching=false -Dorg.gradle.parallel=true
		-Dorg.gradle.workers.max=$NUM_BUILD_PROCS "
	export GTK3_MODULES="$GTK3_MODULES:gtk-vector-screenshot"

	export MAKEFLAGS=" $MAKEFLAGS -j$NUM_BUILD_PROCS "
	export CARGO_MAKEFLAGS=" $CARGO_MAKEFLAGS $MAKEFLAGS "
fi

[ -r "$HOME/.profile_custom" ] && . "$HOME/.profile_custom"

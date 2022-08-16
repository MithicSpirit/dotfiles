export EDITOR="nvim"
export LANG="en_US.UTF-8"
export TERMINAL="alacritty"
export BROWSER="dmenu-browser"
export REAL_BROWSER="librewolf"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
. "$XDG_CONFIG_HOME/user-dirs.dirs"
#export XDG_{DOWNLOAD,DOCUMENTS,MUSIC,PICTURES,VIDEOS,PUBLICSHARE,DESKTOP,TEMPLATES}_DIR

export AMD_VULKAN_ICD="RADV"
export BATDIFF_USE_DELTA="true"
export CARGO_BUILD_JOBS="8"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export CHKTEXRC="/home/mithic/.config"
export DEBUGINFOD_URLS="https://debuginfod.elfutils.org/"
export DIALOGRC="$XDG_CONFIG_HOME/dialogrc"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$HOME/.local/share/go"
export GRADLE_OPTS=" $GRADLE_OPTS -Dorg.gradle.daemon=false "
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export GTK3_MODULES="$GTK3_MODULES:gtk-vector-screenshot"
export GTK_THEME=Arc-Dark
export GPG_TTY="$(tty)"
#export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"
export PYTHONTRACEMALLOC=1
export QT_QPA_PLATFORMTHEME="qt5ct"
export RIPGREP_CONFIG_PATH="$HOME/.config/rg.conf"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export RXVT_SOCKET="$XDG_RUNTIME_DIR/urxvtd"
export DOT_SAGE="$XDG_CONFIG_HOME/sage"
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SPACEMACSDIR="$HOME/.config/spacemacs"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export STARSHIP_LOG="error"
export VK_ICD_FILENAMES="/usr/share/vulkan/icd.d/radeon_icd.i686.json:/usr/share/vulkan/icd.d/radeon_icd.x86_64.json"
export WINEPREFIX="$XDG_DATA_HOME/wine"
export WINIT_X11_SCALE_FACTOR=1
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export XCURSOR_THEME="breeze_cursors"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export MAKEFLAGS="-j10"
export CARGO_MAKEFLAGS="$MAKEFLAGS"

export PAGER="bat -p --paging=always --theme=ansi"
export MANPAGER="less"
#export DOOMPAGER="$PAGER"

export LESS_TERMCAP_md="$(echo '\033[1;35m')"
export LESS_TERMCAP_so="$(echo '\033[3;33m')"
export LESS_TERMCAP_us="$(echo '\033[4;34m')"
export LESS_TERMCAP_me="$(echo '\033[0m')"
export LESS_TERMCAP_se="$(echo '\033[0m')"
export LESS_TERMCAP_ue="$(echo '\033[0m')"

unset LINES COLUMNS

if [ -z "$__MITHIC_PATHS_INITALIZED" ]; then
	export __MITHIC_PATHS_INITIALIZED=1
	export PATH="$XDG_CONFIG_HOME/emacs/bin:$HOME/.local/bin:$HOME/.local/bin/flatpak:$PATH"
	export INFOPATH="$HOME/.local/info:$INFOPATH"
	export MANPATH="$HOME/.local/man:/usr/local/man:/usr/share/man:$MANPATH"
	export XDG_DATA_DIRS="$XDG_DATA_DIRS:/usr/local/share:/usr/share"
	export XDG_CONFIG_DIRS="$XDG_CONFIG_DIRS:/etc/xdg"
fi

[ -f "$HOME/.profile_custom" ] && . "$HOME/.profile_custom"

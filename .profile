export PATH="$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/lib/ccache/bin:$PATH"
export EDITOR="vis"
export LANG="en_US.UTF-8"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share:$XDG_DATA_DIRS"
export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"

export BATDIFF_USE_DELTA="true"
export CARGO_BUILD_JOBS="8"
export CHKTEXRC="/home/mithic/.config"
export GOPATH="$HOME/.local/share/go"
export GTK_MODULES="$GTK_MODULES:gtk-vector-screenshot"
export INFOPATH="$HOME/.local/info:$INFOPATH"
export MAKEFLAGS="-j10" ; export CARGO_MAKEFLAGS="$MAKEFLAGS"
export MANPAGER="less"
export MANPATH="$HOME/.local/man:/usr/local/man:/usr/share/man:$MANPATH"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export PAGER="bat -p"
export PYGMENTIZE_STYLE="vim"
export PYTHONTRACEMALLOC=1
export QT_QPA_PLATFORMTHEME="qt5ct"
export RUSTC_WRAPPER="sccache"
export RXVT_SOCKET="$XDG_RUNTIME_DIR/urxvtd"
export SCCACHE_CACHE_SIZE="6G"
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

LESS_TERMCAP_md="$(echo -en '\e[1;35m')"; export LESS_TERMCAP_md
LESS_TERMCAP_so="$(echo -en '\e[3;33m')"; export LESS_TERMCAP_so
LESS_TERMCAP_us="$(echo -en '\e[4;34m')"; export LESS_TERMCAP_us
LESS_TERMCAP_me="$(echo -en '\e[0m')"; export LESS_TERMCAP_me
LESS_TERMCAP_se="$(echo -en '\e[0m')"; export LESS_TERMCAP_se
LESS_TERMCAP_ue="$(echo -en '\e[0m')"; export LESS_TERMCAP_ue

unset LINES COLUMNS

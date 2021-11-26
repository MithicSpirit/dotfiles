export PATH="$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/lib/ccache/bin:$PATH"
export EDITOR="vis"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share:$XDG_DATA_DIRS"
export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"

export QT_QPA_PLATFORMTHEME="qt5ct"
export PYTHONTRACEMALLOC=1
export RXVT_SOCKET="$XDG_RUNTIME_DIR/urxvtd"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export PYGMENTIZE_STYLE="vim"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export MAKEFLAGS="-j10"
export CARGO_MAKEFLAGS="$MAKEFLAGS"
export RUSTC_WRAPPER="sccache"
export SCCACHE_CACHE_SIZE="6G"
export GOPATH="$HOME/.local/share/go"
export BATDIFF_USE_DELTA=true
export GTK_MODULES="$GTK_MODULES:gtk-vector-screenshot"

export PASSWORD_STORE_DIR="$HOME/.local/share/password-store"

unset LINES COLUMNS

export PATH="$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/lib/ccache/bin:$PATH"
export EDITOR="nvim"
export TERMINAL="alacritty"

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

unset LINES COLUMNS

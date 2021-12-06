export PATH="$HOME/.local/bin:/usr/lib/ccache/bin:$PATH"
export EDITOR="vis"
export LANG="en_US.UTF-8"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"

export XDG_CONFIG_HOME="$HOME/.config"; PATH="$XDG_CONFIG_HOME/emacs/bin:$PATH"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share:$XDG_DATA_DIRS"
export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"

export AMD_VULKAN_ICD=RADV
export BATDIFF_USE_DELTA="true"
export CARGO_BUILD_JOBS="8"
export CHKTEXRC="/home/mithic/.config"
export GOPATH="$HOME/.local/share/go"
export GTK_MODULES="$GTK_MODULES:gtk-vector-screenshot"
export GPG_TTY="$(tty)"
export INFOPATH="$HOME/.local/info:$INFOPATH"
export MAKEFLAGS="-j10"; export CARGO_MAKEFLAGS="$MAKEFLAGS"
export MANPAGER="less"
export MANPATH="$HOME/.local/man:/usr/local/man:/usr/share/man:$MANPATH"
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export PAGER="bat -p"
export PYGMENTIZE_STYLE="vim"
export PYTHONTRACEMALLOC=1
export QT_QPA_PLATFORMTHEME="qt5ct"
export RIPGREP_CONFIG_PATH="$HOME/.config/rg.conf"
export RUSTC_WRAPPER="sccache"
export RXVT_SOCKET="$XDG_RUNTIME_DIR/urxvtd"
export SCCACHE_CACHE_SIZE="6G"
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export STARSHIP_LOG="error"
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/radeon_icd.i686.json:/usr/share/vulkan/icd.d/radeon_icd.x86_64.json
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

LESS_TERMCAP_md="$(echo '\033[1;35m')"; export LESS_TERMCAP_md
LESS_TERMCAP_so="$(echo '\033[3;33m')"; export LESS_TERMCAP_so
LESS_TERMCAP_us="$(echo '\033[4;34m')"; export LESS_TERMCAP_us
LESS_TERMCAP_me="$(echo '\033[0m')"; export LESS_TERMCAP_me
LESS_TERMCAP_se="$(echo '\033[0m')"; export LESS_TERMCAP_se
LESS_TERMCAP_ue="$(echo '\033[0m')"; export LESS_TERMCAP_ue

unset LINES COLUMNS

[ -f "$HOME/.profile_custom" ] && source "$HOME/.profile_custom"
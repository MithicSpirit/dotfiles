# Basic variables
set -pgx PATH $HOME/.local/bin $HOME/.emacs.d/bin $PATH
set -g fish_greeting

set -gx LANG en_US.UTF-8
set -pgx MANPATH $HOME/.local/man /usr/local/man $MANPATH
set -pgx INFOPATH $HOME/.local/info $INFOPATH

set -gx EDITOR nvim
set -gx VISUAL visual
set -gx SUDO_EDITOR $EDITOR

set -gx CHKTEXRC $HOME
set -gx MAKEFLAGS -j 10

# Externals
source /usr/share/autojump/autojump.fish # j <dir> to jump
source ~/.config/fish/aliases.fish # Many, many, many aliases
# Cool `cd` thing
functions -c cd __wrapped_cd
functions -e cd
functions -c __plugin_cd cd
source ~/.config/fish/completions/cd.fish # Not working 😳

# TODO:
# - ez-extract function
# - C-z toggle

# Keybinds
bind "\cz" fg
bind "\cq" cancel
bind "&&" 'commandline -i "; and"'
bind "||" 'commandline -i "; or"'

function bind_bang
    switch (commandline -t)
        case "!"
            commandline -t $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end

bind ! bind_bang

# Colored manpages
set -gx LESS_TERMCAP_md (printf "\e[01;38;5;74m")
set -gx LESS_TERMCAP_mb (printf "\033[01;31m")
set -gx LESS_TERMCAP_me (printf "\033[0m")

set -gx LESS_TERMCAP_ue (printf "\e[0m")
set -gx LESS_TERMCAP_us (printf "\e[04;38;5;146m")

set -gx LESS_TERMCAP_se (printf "\033[0m")
set -gx LESS_TERMCAP_so (printf "\033[01;44;33m")

# Prompt
starship init fish | source

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

# TODO:
# - ez-extract function
# - colored man pages
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

# Clipcat
if type clipcat-menu >/dev/null 2>&1
    alias clipedit 'clipcat-menu --finder=builtin edit'
    alias clipdel 'clipcat-menu --finder=builtin remove'

    bind -s '\c\\' "^Q clipcat-menu --finder=builtin insert ^J"
    bind -s '\e]' "^Q clipcat-menu --finder=builtin remove ^J"
end

# Prompt
starship init fish | source

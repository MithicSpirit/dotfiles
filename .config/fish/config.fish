# Basic variables
set -px PATH "$HOME/.local/bin" "$HOME/.emacs.d/bin" /usr/lib/ccache/bin
set -px MANPATH "$HOME/.local/man" /usr/local/man /usr/share/man
set -px INFOPATH "$HOME/.local/info"

# Externals
[ -e "/usr/share/autojump/autojump.fish" ] &&
    source "/usr/share/autojump/autojump.fish"
# Cool `cd` thing
functions -c cd __wrapped_cd
functions -e cd
functions -c __plugin_cd cd

# Keybinds
bind \cd exit
bind \cz fg "commandline -f repaint"
if command -v xclip &>/dev/null
    bind \cq "commandline | xclip -i -r -sel clip" "commandline -r ''"
else if command -v termux-clipboard-set &>/dev/null
    bind \cq "commandline | termux-clipboard-set" "commandline -r ''"
else
    bind \cq "commandline -r ''"
end

function bind_bang
    switch (commandline -t)
        case "!"
            commandline -t $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end
bind "!" bind_bang

set --unexport LINES
set --unexport COLUMNS

# Prompt
starship init fish | source

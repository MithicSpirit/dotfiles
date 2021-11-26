# Basic variables
set -px PATH "$HOME/.local/bin" "$HOME/.emacs.d/bin" /usr/lib/ccache/bin
set -px MANPATH "$HOME/.local/man" /usr/local/man /usr/share/man
set -px INFOPATH "$HOME/.local/info"
set -x CARGO_MAKEFLAGS "$MAKEFLAGS"
set --unpath -x GOPATH "$HOME/.local/share/go"

# Externals
if test -f "/usr/share/autojump/autojump.fish"
    source "/usr/share/autojump/autojump.fish"
else if test -f "~/.autojump/share/autojump/autojump.fish"
    source "~/.autojump/share/autojump/autojump.fish"
end

# Keybinds
bind \cd exit
bind \cz fg "commandline -f repaint"
## If `xclip' exists _and_ on X11
if test -n "$(command -v xclip)" -a -n "$XAUTHORITY"
    bind \cq "commandline | xclip -i -r -sel clip" "commandline -r ''"
# If on termux
else if test -n "$(command -v termux-clipboard-set)"
    bind \cq "commandline | termux-clipboard-set" "commandline -r ''"
# Otherwise just blank it
else
    bind \cq "commandline -r ''"
end # TODO: wayland?

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

# Misc
set --unexport LINES
set --unexport COLUMNS
complete docker -e  # Requires `docker` group, which is unsafe
starship init fish | source  # Prompt

# Emacs vterm
if [ "$INSIDE_EMACS" = vterm ]
    function vterm-printf
        if begin
                [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
            end
            # tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
        else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
        else
            printf "\e]%s\e\\" "$argv"
        end
    end

    function clear
        vterm-printf "51;Evterm-clear-scrollback"
        tput clear
    end

    function vterm-cmd --description 'Run an Emacs command among the ones been defined in vterm-eval-cmds.'
        set -l vterm_elisp ()
        for arg in $argv
            set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
        end
        vterm-printf '51;E'(string join '' $vterm_elisp)
    end

    function find-file --description 'Open the specified file (or current directory) in Emacs when using vterm.'
        set -q argv[1]; or set argv[1] "."
        vterm-cmd find-file (realpath "$argv")
    end

    function vterm_prompt_end
        vterm-printf '51;A'(whoami)'@'(hostname)':'(pwd)
    end

    functions --copy fish_prompt vterm_old_fish_prompt
    function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
        # Remove the trailing newline from the original prompt. This is done
        # using the string builtin from fish, but to make sure any escape codes
        # are correctly interpreted, use %b for printf.
        printf "%b" (string join "\n" (vterm_old_fish_prompt))
        vterm_prompt_end
    end
end

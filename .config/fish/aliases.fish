#!/usr/bin/env fish

# Misc
alias grep "grep --color=auto"
alias fishrc "$EDITOR ~/.config/fish/config.fish"
abbr h "history"
abbr -g xrdb "xrdb ~/.Xresources"

# Common switches
abbr -g rmrm "rm -i"
abbr -g mv "mv -i"
abbr -g cp "cp -ir"
abbr -g mkdir "mkdir -p"
abbr -g ln "ln -si"
abbr -g sl "sl -ae"
abbr -g df "df -h"
abbr -g matrix "cmatrix -ab"

# Prepends
abbr -g su "sudo -i"
abbr -g xsu "exec sudo -i"
abbr -g ufw "sudo ufw"
abbr -g visudo "sudo --preserve-env=EDITOR visudo"
abbr -g svi "sudoedit"
abbr -g radeontop "sudo radeontop -cT"
abbr -g xsh "exec fish"
abbr -g xssh "exec ssh"

# Replacements
abbr -g rm "trash"
abbr -g du "dust"
abbr -g top "btm -b"
abbr -g htop "btm -b"
abbr -g emacs "visual"
abbr -g eterm "visual -t"

# Shortcuts
abbr -g md "mkdir"
abbr -g bot "btm"
abbr -g scim "sc-im"
abbr -g vi "nvim"
abbr -g vim "nvim"
abbr -g copy "xclip -i -sel clip <"


# LONG long unsigned int (scripts/many switches)
abbr -g dooms "doom sync ; doom doctor"
abbr -g doomup "doom upgrade && doom sync -p ; doom doctor"
abbr -g f "fortune | cowsay"
alias torrium 'chromium --proxy-server="socks5://localhost:9050" --incognito \
--host-resolver-rules="MAP * ~NOTFOUND , EXCLUDE localhost"'
alias reflect "sudo reflector --protocol https --latest 200 --sort rate \
--save /etc/pacman.d/mirrorlist --verbose"

alias homegit 'git --git-dir=$HOME/.homegit --work-tree=$HOME'
abbr -g hg "homegit"
abbr -g hga "homegit add --update"
abbr -g hgs "homegit status"
abbr -g hgd "homegit diff"
abbr -g hgdd "homegit diff --cached"
abbr -g hgc "homegit commit -S -m"
abbr -g hgp "read -rs && homegit push origin && homegit push backup"

abbr -g py "python"
abbr -g ipy "ipython"
abbr -g pip "python -m pip"
abbr -g pip3 "python3 -m pip"

abbr -g tsc "npx tsc"
abbr -g eslint "npx eslint"

abbr -g cpp "g++ -Wall -ansi -DMITHIC"
abbr -g cppa "g++ -Wall -ansi -DMITHIC -S -fverbose-asm"

# ls/exa
alias exa "exa --color=automatic"
abbr -g ls "exa"
abbr -g l "exa -lFbg"
abbr -g lS "exa -lFbs size --color-scale"
abbr -g la "exa -laFb"
abbr -g ldot "ls -ld .*"

# yay
alias yay "yay --color=auto"
abbr -g yaconf 'yay -Pg'
abbr -g yaupg 'yay -Syu'
abbr -g yasu 'yay -Syu --noconfirm'
abbr -g yains 'yay -S'
abbr -g yainsd 'yay -S --asdeps'
abbr -g yainse 'yay -S --asexplicit'
abbr -g yarem 'yay -Rns'
abbr -g yainf 'yay -Si'
abbr -g yasear 'yay -Ss'
abbr -g yalin 'yay -Qi'
abbr -g yalsea 'yay -Qs'
abbr -g yalst 'yay -Qe'
abbr -g yaorph 'yay -Qtd'

# pacman
abbr -g pacupg 'sudo pacman -Syu'
abbr -g pacsu 'sudo pacman -Syu --noconfirm'
abbr -g pacins 'sudo pacman -S'
abbr -g pacinsd 'sudo pacman -S --asdeps'
abbr -g pacinse 'sudo pacman -S --asexplicit'
abbr -g pacrem 'sudo pacman -Rns'
abbr -g pacinf 'pacman -Si'
abbr -g pacsear 'pacman -Ss'
abbr -g paclin 'pacman -Qi'
abbr -g paclsea 'pacman -Qs'
abbr -g paclst 'pacman -Qe'
abbr -g pacorph 'pacman -Qtd'

# git
abbr -g gs "git status"
abbr -g gd "git diff"
abbr -g gdd "git diff --cached"
abbr -g gdl "git diff HEAD~"
abbr -g ga "git add"
abbr -g gaa "git add --all"
abbr -g gau "git add --update"
abbr -g gb "git branch"
abbr -g gc "git commit"
abbr -g gsta "git stash apply"
abbr -g gst "git stash"
abbr -g gl "git pull"
abbr -g gp "git push"

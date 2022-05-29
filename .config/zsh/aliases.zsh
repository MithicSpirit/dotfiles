alias -g \
    C='| tee >(xclip -i -sel c) | bat -pp' \
    G='| rg' \
    P="| $PAGER" \
    NOP='&>/dev/null'

alias \
    c='cal -3' \
    copy='xclip -i -sel c' \
    cp='cp -ir' \
    df='duf' \
    diff='batdiff' \
    doomd='doom doctor' \
    dooms='doom sync; doom doctor' \
    doomup='doom upgrade; doom sync -p; doom doctor' \
    du='dust' \
    eemacs='emacs' \
    emacs='devour visual' \
    eterm='emacs -nw' \
    ffupload='ffsend upload -vd 20 -e 7d' \
    ghc='ghc -dynamic -Wall' \
    gpp='g++' \
    h='history 1' \
    l='exa -lFbg --git' \
    lS='exa -laFbgs size --color-scale --group-directories-first' \
    la='exa -laFbg --git' \
    ldot='exa -laFbgd .*' \
    ln='ln -si' \
    lr='exa -laFbgR' \
    ls='exa' \
    matrix='cmatrix -ab' \
    md='mkdir -p' \
    mv='mv -i' \
    open='xdg-open' \
    pacdiff='sudo DIFFPROG="nvim -d" pacdiff' \
    paste='xclip -o -sel c' \
    pping='prettyping' \
    pip='python -m pip' \
    ppy='pypy3' \
    ps='procs' \
    radeontop='sudo radeontop -cT' \
    rm='trash' \
    rmrm='rm -i' \
    sc='systemctl' \
    scim='sc-im' \
    scu='systemctl --user' \
    sl='sl -ae' \
    ssc='sudo systemctl' \
    su='sudo -i' \
    svi='sudoedit' \
    ufw='sudo ufw' \
    updb='sudo updatedb' \
    v='bat' \
    venv='source venv/bin/activate' \
    vi="$EDITOR" \
    vidiff='nvim -d' \
    visudo='sudo --preserve-env=EDITOR visudo' \
    xalarm='exec alarm' \
    xfish='exec fish' \
    xrdb='xrdb ~/.config/xresources/Xresources' \
    xsh='exec zsh' \
    xssh='exec ssh' \
    xsu='exec sudo -i' \
    yt='yt-dlp'

command -v git &>/dev/null && alias \
    g='git' \
    ga='git add' \
    gaa='git add --all' \
    gau='git add --update' \
    gb='git branch' \
    gba='git branch --all' \
    gc='git commit' \
    gcl='git clone' \
    gco='git checkout' \
    gd='git diff' \
    gdd='git diff --cached' \
    gdl='git diff HEAD~' \
    gf='git fetch' \
    gid='git rev-parse HEAD' \
    gl='git pull' \
    gld='git diff HEAD..@{u}' \
    glg='git log' \
    gllg='git log HEAD..@{u}' \
    gp='git push' \
    gr='git remote -v' \
    grm='git restore --staged' \
    grmrm='git restore' \
    gsh='git stash' \
    gst='git status' \
    hg='homegit' \
    hga='homegit add --update' \
    hgc='homegit commit -m' \
    hgco='homegit checkout --force' \
    hgd='homegit diff' \
    hgdd='homegit diff --cached' \
    hgl='homegit pull --ff' \
    hgp='read -rs && homegit push origin && homegit push backup' \
    hgrm='homegit restore --staged' \
    hgst='homegit status'

command -v git &>/dev/null && homegit () {
        git --git-dir=$HOME/.homegit --work-tree=$HOME $@
    }

command -v devour &>/dev/null && alias \
    dev='devour' \
    dopen='devour xdg-open' \
    emacs='devour visual'

command -v pacman &>/dev/null && alias \
    pac='pacman' \
    paccl='sudo pacman -Sc' \
    pacin='sudo pacman -S' \
    pacind='sudo pacman -S --asdeps' \
    pacli='pacman -Qi' \
    pacll='pacman -Q' \
    pacls='pacman -Qs' \
    pacmd='sudo pacman -D --asdeps' \
    pacme='sudo pacman -D --asexplicit' \
    pacorph='pacman -Qtd' \
    pacorphrm='sudo pacman -Rs $(pacman -Qtdq)' \
    pacown='pacman -Qo' \
    pacownl='pacman -Ql' \
    pacrem='sudo pacman -Rs' \
    pacri='pacman -Si' \
    pacrl='pacman -Sl' \
    pacrs='pacman -Ss' \
    pacupg='sudo pacman -Syyu'

command -v paru &>/dev/null && alias \
    pa='paru' \
    pacl='paru -Sc' \
    pain='paru -S' \
    paind='paru -S --asdeps' \
    pali='paru -Qi' \
    pall='paru -Q' \
    palrl='paru -Ll' \
    palrrm='paru -Ld' \
    palrun="paru -Ll | rg -v '\[installed\]'" \
    palrunrm="paru -Ll | rg -v '\[installed\]' | awk '{print \$2}' | xargs paru -Ld" \
    pals='paru -Qs' \
    pamd='paru -D --asdeps' \
    pame='paru -D --asexplicit' \
    paorph='paru -Qtd' \
    paorphrm='paru -c' \
    paown='paru -Qo' \
    paownl='paru -Ql' \
    parem='paru -Rs' \
    pari='paru -Si' \
    parl='paru -Sl' \
    pars='paru -Ss' \
    pary='paru' \
    paupg='paru -Syyu'


command -v python &>/dev/null && command -v ipython &>/dev/null &&
    py () {
        if [[ $# = 0 ]]; then
            ipython
        else
            python $@
        fi
    }

command -v xdg-open &>/dev/null && 
    xopen () { 
        xdg-open "$@"& 
        disown && exit 0 
    }

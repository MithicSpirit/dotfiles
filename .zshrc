# Basic variables
export PATH="$HOME/.local/bin:$HOME/.emacs.d/bin:$PATH"
export LANG=en_US.UTF-8
export MANPATH="$MANPATH:$HOME/.local/man:/usr/local/man"
export INFOPATH="$INFOPATH:$HOME/.local/info:/usr/share/info"
export N_PREFIX="$HOME/.local"
export CHKTEXRC="$HOME"
export EDITOR="nvim"
export VISUAL="visual"
export SUDO_EDITOR=$EDITOR
export MAKEFLAGS="-j 10"

# Zsh/ohmyzsh variables
export ZSH="$HOME/.oh-my-zsh"
ZSH_CUSTOM="$HOME/.oh-my-zsh-custom"
ZSH_THEME="powerlevel10k/powerlevel10k"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
HIST_STAMPS="yyyy-mm-dd"

plugins=(
	archlinux
	autojump
	autopep8 
	colored-man-pages
	command-not-found 
	common-aliases 
	copyfile 
	dirhistory 
	emoji
	extract
	fancy-ctrl-z
	git 
	git-auto-fetch
	gitfast
	github
	gitignore
	# globalias
	gnu-utils
	node
	npm 
	npx
	pip 
	pyenv
	python 
	ripgrep
	sudo
	systemd
	ubuntu
	yarn
	zsh-autosuggestions
	zsh-completions
	zsh-syntax-highlighting 
	zsh-history-substring-search
)
source /home/mithic/.config/broot/launcher/bash/br

# Init stuff
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]
then
	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source $ZSH/oh-my-zsh.sh

# Custom globalias
# (I only want aliases, not other expressions)
globalias() {
	local word=${${(Az)LBUFFER}[-1]}
	if [[ $GLOBALIAS_FILTER_VALUES[(Ie)$word] -eq 0 ]]; then
		zle _expand_alias  # aliases
		# zle expand-word  # non-alias expressions
	fi
	zle self-insert
}
zle -N globalias
bindkey -M emacs " " globalias
bindkey -M emacs "^ " magic-space
bindkey -M isearch " " magic-space

# Aliases

alias -g C="| xclip -i -sel clip"

alias ln="ln -si"
alias mv="mv -i"
alias cp="cp -ir"
alias md="\\mkdir"
alias mkdir="\\mkdir -p"
alias sl="sl -ae"
alias df="df -h"
alias rmrm="\\rm -i"
alias gs="git status"
alias gdd="git diff --cached"
alias xrdb="xrdb ~/.Xresources"
alias yay="yay --color=auto"
alias grep="nocorrect grep --color=auto"
alias termdown="termdown --no-figlet -ac 5"
alias matrix="cmatrix -ab"

alias xsh="exec zsh"
alias xssh="exec ssh"
alias su="sudo -i"
alias xsu="exec sudo -i"
alias aptg="sudo apt-get"
alias dpkg="sudo dpkg"
alias dpkgq="dpkg-query"
alias updatedb="sudo updatedb"
alias snap="sudo snap"
alias ufw="sudo ufw"
alias visudo="sudo --preserve-env=EDITOR visudo"
alias svi="sudoedit"
alias radeontop="sudo radeontop -cT"
alias reflect="sudo reflector --protocol https --latest 200 --sort rate \
--save /etc/pacman.d/mirrorlist --verbose"
alias killall="nocorrect killall"

alias rm="trash"
alias du="dust"
alias top="btm -b"
alias htop="btm -b"
alias bot="btm"
alias open="xdg-open"
# alias upzsh="omz update ; ~/.oh-my-zsh-custom/pull-all"
alias emacs="visual"
alias eterm="visual -t"
alias vi="nvim"
alias vim="nvim"
alias scim="sc-im"

alias ls="exa"
alias l="exa -lFbg"
alias lS="exa -lFbs size --color-scale"
alias la="exa -laFb"
alias ldot="ls -ld .*"
unalias lart
unalias ll
unalias lr
unalias lrt
unalias lsa
alias exa="exa --color=automatic"

alias copy="xclip -i -sel clip <"
alias dooms="doom sync ; doom doctor"
alias doomup="doom upgrade && doom sync -p ; doom doctor"
alias f="fortune | cowsay"
alias torrium='chromium --proxy-server="socks5://localhost:9050" --incognito \
--host-resolver-rules="MAP * ~NOTFOUND , EXCLUDE localhost"'
upzsh () {
	~/.oh-my-zsh-custom/pull-all
	echo "\nOhMyZsh:"
	omz update
}

alias homegit='git --git-dir=$HOME/.homegit --work-tree=$HOME'
alias hg="homegit"
alias hga="homegit add --update"
alias hgs="homegit status"
alias hgd="homegit diff"
alias hgdd="homegit diff --cached"
alias hgc="homegit commit -S -m"
alias hgp="read -rs && homegit push origin && homegit push backup"

alias h="\\omz_history"
alias hs="\\history"

# Disabled because spaces are weird in ipython on Emacs libvterm
# py () {
# 	if [[ $# -eq 0 ]]; then
# 		ipython
# 	else
# 		python $@
# 	fi
# }
alias py="python"
alias ipy="ipython"
alias pip="python -m pip"
alias pip3="python3 -m pip"

alias tsc="npx tsc"
alias eslint="npx eslint"

alias cpp="g++ -Wall -ansi -DMITHIC"
alias cppa="g++ -Wall -ansi -DMITHIC -S -fverbose-asm"

GLOBALIAS_FILTER_VALUES=(
	exa
	f
	grep
	homegit
	ipython
	killall
	man
	sl
	sudo
	torrium
	yay
	zshrc
)

# Misc
less_termcap[so]="${fg_bold[yellow]}${bg[trans]}" # Manpage coloring
_comp_options+=(globdots) # Match hidden files in completion

# Keybinds
bindkey "^P" history-substring-search-up
bindkey "^N" history-substring-search-down
bindkey  "^[p" up-line-or-history
bindkey  "^[n" down-line-or-history
bindkey "^[e" edit-command-line
bindkey -M menuselect "^P" up-line-or-history
bindkey -M menuselect "^N" down-line-or-history
bindkey -M menuselect "^F" forward-char
bindkey -M menuselect "^B" backward-char

# Powerlevel10k prompt
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Beam cursor shape
#preexec () { echo -ne '\e[5 q' }
#preexec 


#!/usr/bin/env zsh

# Basic variables
export PATH="$HOME/.emacs.d/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:$PATH"
export LANG=en_US.UTF-8
export MANPATH="$MANPATH:$HOME/.local/man:/usr/local/man"
export INFOPATH="$INFOPATH:$HOME/.local/info"
export N_PREFIX="$HOME/.local"
export FPATH="$FPATH:$HOME/.completions"
export CHKTEXRC="$HOME"
export EDITOR="nvim"
export VISUAL="$HOME/VISUAL"

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
	globalias
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

# Init stuff
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]
then
	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source $ZSH/oh-my-zsh.sh

# Aliases
unalias fd

alias -g C="| xclip -i -sel clip"

alias ln="ln -si"
alias mv="mv -i"
alias md="\\mkdir"
alias mkdir="\\mkdir -p"
alias sl="sl -ea"
alias rmrm="\\rm -i"
alias ls="ls --color=auto"

alias xsh="exec zsh"
alias xssh="exec ssh"
alias su="sudo -i"
alias xsu="exec sudo -i"
alias aptg="sudo apt-get"
alias dpkg="sudo dpkg"
alias dpkgq="dpkg-query"
alias updatedb="sudo updatedb"
alias snap="sudo snap"
alias visudo="sudo --preserve-env=EDITOR visudo"

alias rm="trash"
alias du="dust"
alias open="xdg-open"
alias upcustom="~/.oh-my-zsh-custom/pull-all"
alias visual="$VISUAL"
alias emacs="visual"
alias eterm="visual -t"

alias copy="xclip -i -sel clip <"
alias dooms="doom sync && doom doctor"
alias doomup="doom sync && doom upgrade && doom clean && doom purge -bg && doom env && doom compile && doom build -r && doom sync -pe && doom doctor"
alias f="fortune | /bin/cowsay"
alias torrium='chromium --proxy-server="socks5://localhost:9050" --incognito --host-resolver-rules="MAP * ~NOTFOUND , EXCLUDE localhost"'


alias homegit='git --git-dir=$HOME/.homegit --work-tree=$HOME'
alias hg="homegit"
alias hganew="homegit add"
alias hga="homegit add --update"
alias hgs="homegit status"
alias hgd="homegit diff"
alias hgdd="homegit diff --cached"
alias hgc="homegit commit -S -m"
alias hgp="read -rs && (homegit push origin ; homegit push backup)"

alias h="\\omz_history"
alias hs="\\history"

py () {
	if [[ $# -eq 0 ]]; then
		ipython
	else
		python $@
	fi
}
alias pip="python -m pip"
alias pip3="python3 -m pip"

alias tsc="npx tsc"
alias eslint="npx eslint"

alias cpp="g++ -Wall -ansi -DMITHIC"
alias cppa="g++ -Wall -ansi -DMITHIC -S -fverbose-asm"

GLOBALIAS_FILTER_VALUES=(
	homegit
	sl
	grep
	ls
	man
	sudo
	\*
	\*\*
	\*\*/\*
	\$VISUAL
	torrium
	upcustom
	\~
	visual
)

# Manpage coloring
less_termcap[so]="${fg_bold[yellow]}${bg[trans]}"

# Keybinds
bindkey "^[[A" history-substring-search-up
bindkey "^[[B" history-substring-search-down

# NVM stuff
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

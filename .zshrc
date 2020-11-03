# If you come from bash you might have to change your $PATH.
export PATH="$HOME/.emacs.d/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:$PATH"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM="$HOME/.oh-my-zsh-custom"

# OMZ pre-instant prompt
if [[ -z "$ZSH_CACHE_DIR" ]]; then
	ZSH_CACHE_DIR="$ZSH/cache"
fi
if [ -f ~/.zsh-update ] && [ ! -f ${ZSH_CACHE_DIR}/.zsh-update ]; then
	mv ~/.zsh-update ${ZSH_CACHE_DIR}/.zsh-update
fi
if [ "$DISABLE_AUTO_UPDATE" != "true" ]; then
	env ZSH=$ZSH ZSH_CACHE_DIR=$ZSH_CACHE_DIR DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh -f $ZSH/tools/check_for_upgrade.sh
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
	archlinux
	autojump
	autopep8 
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

source $ZSH/oh-my-zsh.sh

# User configuration
export MANPATH="$MANPATH:$HOME/.local/man:/usr/local/man"
export INFOPATH="$INFOPATH:$HOME/.local/info"
export N_PREFIX="$HOME/.local"
export FPATH="$FPATH:$HOME/.completions"
export CHKTEXRC="$HOME"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
export EDITOR='nvim'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias -g C="| xclip -i -sel clip"

alias du="dust"
alias open="xdg-open"
alias ln="ln -si"
alias mv="mv -i"
alias rm="trash"
alias rmrm="\\rm -i"
alias mkdir="\\mkdir -p"
alias md="\\mkdir"
alias copy="xclip -i -sel clip <"
alias dooms="doom sync && doom doctor"
alias doomup="doom sync && doom upgrade && doom clean && doom purge -bg && doom env && doom compile && doom build -r && doom sync -pe && doom doctor"
alias sl="sl -ea"
unalias fd

alias f="fortune | /bin/cowsay"
alias ls="ls --color=auto"

alias torrium='chromium --proxy-server="socks5://localhost:9050" --incognito --host-resolver-rules="MAP * ~NOTFOUND , EXCLUDE localhost"'

alias homegit='git --git-dir=$HOME/.homegit --work-tree=$HOME'

unalias history
alias h="\\omz_history"
alias hs="\\history"

alias su="sudo su"
alias xsu="exec sudo su"
alias xsh="exec ssh"
alias aptg="\\sudo apt-get"
alias dpkg="sudo dpkg"
alias dpkgq="dpkg-query"
alias updatedb="sudo updatedb"
alias snap="sudo snap"
alias visudo="sudo --preserve-env=EDITOR visudo"

alias py="python"
alias pip="python -m pip"
alias pip3="python3 -m pip"

alias tsc="npx tsc"
alias eslint="npx eslint"

# "MITHIC" is a custom flag I use for my own debugging purposes
alias c++="g++ -Wall -ansi -DMITHIC"
alias c++a="g++ -Wall -ansi -DMITHIC -S -fverbose-asm"

bindkey "^[[A" history-substring-search-up
bindkey "^[[B" history-substring-search-down
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# NVM stuff
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

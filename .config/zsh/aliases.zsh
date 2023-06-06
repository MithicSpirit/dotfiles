alias -g \
	C='| tee >(xclip -i -sel c) | bat -pp' \
	G='| rg' \
	P='| ${=PAGER}' \
	B='&>/dev/null &!' \

alias \
	..='cd ..' \
	c='cal -3' \
	copy='xclip -i -sel c' \
	cp='cp -ir' \
	df='duf' \
	diff='batdiff' \
	doomd='doom doctor' \
	dooms='doom sync; doom doctor' \
	doomup='doom upgrade; doom sync -p; doom doctor' \
	dragon='dragon-drop' \
	du='dust' \
	eemacs='emacs' \
	eterm='emacs -nw' \
	ffupload='ffsend upload -vd 20 -e 7d' \
	ghc='ghc -Wall' \
	gcc='gcc -Wall -Wextra' \
	gpp='g++ -Wall -Wextra' \
	h='history 1' \
	idr='rlwrap -n idris2' \
	im='imv' \
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
	r='rifle' \
	radeontop='sudo radeontop -cT' \
	rm='rmtrash -I' \
	rmdir='rmdirtrash -p' \
	rmrm='rm -iv' \
	rmrmdir='rmdir -pv' \
	sc='systemctl' \
	scim='sc-im' \
	scu='systemctl --user' \
	sl='sl -ae -50' \
	ssc='sudo systemctl' \
	su='sudo -i' \
	svi='sudoedit' \
	swapflush='sudo swapoff -va; sudo swapon -va' \
	tmuxattachall='while tmux attach-session; do done' \
	ufw='sudo ufw' \
	unvenv='deactivate' \
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
	xtm='exec tmux' \
	yt='yt-dlp' \
	za='zathura' \

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
	gf='git fetch --all' \
	gid='git rev-parse HEAD' \
	gl='git pull' \
	gld='git diff HEAD..@{u}' \
	glg='git log' \
	gllg='git log HEAD..@{u}' \
	gp='git push' \
	gr='git remote -v' \
	grm='git restore --staged' \
	grmrm='git restore' \
	glsf='for repo in */; do (cd "$repo" && git fetch) & done; wait' \
	gsh='git show' \
	gst='git status' \
	gsts='git stash' \

command -v git &>/dev/null &&
	homegit () {
		git --git-dir="$HOME/.homegit" --work-tree="$HOME" "$@"
	}
command -v homegit &>/dev/null && alias \
	hg='homegit' \
	hga='homegit add' \
	hgc='homegit commit -m' \
	hgco='homegit checkout --force' \
	hgd='homegit diff' \
	hgdd='homegit diff --cached' \
	hgl='homegit pull --ff' \
	hgp='homegit push origin && homegit push backup' \
	hgrm='homegit restore --staged' \
	hgst='homegit status' \
	hgs='homegit pull --ff && homegit push origin && homegit push backup' \

command -v devour &>/dev/null && alias \
	d='devour' \
	dim='devour imv' \
	dvi='devour neovide --nofork --notabs --' \
	dopen='devour xdg-open' \
	dza='devour zathura' \
	emacs='devour visual' \

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
	pacorphrm='pacman -Qtdq | sudo pacman -Rs -' \
	pacown='pacman -Qo' \
	pacownl='pacman -Ql' \
	pacrem='sudo pacman -Rs' \
	pacri='pacman -Si' \
	pacrl='pacman -Sl' \
	pacrs='pacman -Ss' \
	pacupg='sudo pacman -Syyu' \

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
	paupg='paru -Syyu' \

command -v fasd &>/dev/null && alias \
	f='fasd -s' \
	ff='fasd -si' \

command -v fasd &>/dev/null &&
	fasd_cd() {
		if [ $# -le 1 ]; then
			fasd "$@"
		else
			local _fasd_ret="$(fasd -e 'printf %s' "$@")"
			[ -z "$_fasd_ret" ] && return 1
			[ -d "$_fasd_ret" ] && cd "$_fasd_ret" ||
				printf '%s\n' "$_fasd_ret"
		fi

		local ls=${aliases[ls]}
		if [ -z "$ls" ]; then
			ls
		else
			${=ls}
		fi
	}
command -v fasd_cd &>/dev/null && alias \
	j='fasd_cd -d' \
	jj='fasd_cd -di' \


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

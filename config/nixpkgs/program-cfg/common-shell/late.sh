#########################
# Begin late.sh
#########################
# This file gets executed later in the shell load process;
# notably, after compinit in zsh and the interactive check in bash
# Turn off the terminal bell in every way possible
unsetopt beep
[[ "$osType" == "Linux" ]] && xset -b &
>/dev/null || true

###########
# Aliases #
###########

# Prevents being an idiot and stalling the wm
alias bspwm='bspc'
if command -v "exa" >/dev/null 2>/dev/null; then
	alias ls="exa"
	alias la="exa -la"
	alias tree="exa --tree"
fi
alias sudo="sudo " # allows sudo to be used in aliases
if command -v "bat" >/dev/null 2>/dev/null; then
	alias cat="bat"
	alias bat="cat"
	export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi
if command -v firefox >/dev/null 2>/dev/null; then
	alias firefox="firefox --new-window"
fi
if command -v wifimenu >/dev/null 2>/dev/null; then
	alias wifimenu="sudo wifimenu"
fi
if command -v nix-shell >/dev/null 2>/dev/null; then
	alias nix-shell="nix-shell --command zsh"
fi

####################
# Custom Functions #
####################

# Easier pacman
if [ -f "/etc/arch-release" ]; then
	y() {
		if command -v yay >/dev/null 2>/dev/null; then
			pacfunc="yay --combinedupgrade"
		elif command -v trizen >/dev/null 2>/dev/null; then
			pacfunc="trizen"
		elif command -v pacaur >/dev/null 2>/dev/null; then
			pacfunc="pacaur"
		elif command -v yaourt >/dev/null 2>/dev/null; then
			pacfunc="yaourt"
		else
			pacfunc="sudo pacman"
		fi

		if [ -z "$1" ]; then
			eval "$pacfunc -Syu"
		else
			eval "$pacfunc -S $*"
		fi &&
			# remove to 3 versions of old packages
			sudo paccache -r &&
			# remove all cached uninstalled packages
			sudo paccache -ruk0
	}
fi

# Kitty terminal emulator ssh fix:
if [[ "$TERM" == "xterm-kitty" ]]; then
	ssh() {
		kitty +kitten ssh "$@"
	}
fi

if command -v netctl-auto >/dev/null 2>&1; then
	nc() {
		if [ "$1" == "l" -o "$1" == "list" ]; then
			netctl-auto list
		else
			netctl-auto switch-to $1
		fi
	}
fi

clean_vim() {
	echo "Cleaning ~/.vimbackup/"
	rm -Rf ~/.vimbackup/*
	echo "Cleaning ~/.vimswap/"
	rm -Rf ~/.vimswap/*
	echo "Cleaning ~/.vimviews/"
	rm -Rf ~/.vimviews/*
	echo "Cleaning ~/.vimundo/"
	rm -Rf ~/.vimundo/*
	echo "All done!"
}

#thefuck
if command -v thefuck >/dev/null 2>/dev/null; then
	eval "$(thefuck --alias)"
fi

#######################################
# Programming language specific stuff #
#######################################

export GEM_HOME="$HOME/.local/usr/gems"
export GEM_PATH="$HOME/.local/usr/gems:$GEM_PATH"

export GOPATH="$HOME/g/go"

LS_COLORS='rs=0:di=01;33:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'
export LS_COLORS
#########################
# End late.sh
#########################

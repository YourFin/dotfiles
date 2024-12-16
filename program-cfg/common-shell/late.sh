#########################
# Begin late.sh
#########################
# This file gets executed later in the shell load process;
# notably, after compinit in zsh and the interactive check in bash

# Turn off the terminal bell in every way possible
if command -v unsetopt >/dev/null 2>/dev/null; then
	unsetopt beep
fi
if command -v xset >/dev/null 2>/dev/null; then
	xset -b &
fi

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
	# The backslashes here prevent recursion. See:
	# https://stackoverflow.com/questions/18862777/how-can-i-swap-as-in-alias-command-names-in-zsh
	alias cat="\bat"
	alias bat="\cat"
	export MANROFFOPT='-c'
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
if command -v rsync >/dev/null 2>/dev/null; then
	alias rsync="rsync --progress"
fi
if command -v emacs >/dev/null 2>/dev/null &&
	command -v pkill >/dev/null 2>/dev/null; then
	alias restart-emacs="pushd ~ >/dev/null; while pkill emacs; do ; done && emacs --daemon ; popd >/dev/null"
fi
if command -v python3 >/dev/null 2>/dev/null; then
	alias urldecode='python3 -c "import sys, urllib.parse as ul; \
    print(ul.unquote_plus(sys.argv[1]))"'
	alias urlencode='python3 -c "import sys, urllib.parse as ul; \
		print(ul.quote_plus(sys.argv[1]))"'
fi
if command -v bw >/dev/null 2>/dev/null; then
	alias bitwarden-cli="bw"
fi

####################
# Custom Functions #
####################

# Kitty terminal emulator ssh fix:
if [[ "$TERM" == "xterm-kitty" ]] && command -v kitty >/dev/null 2>/dev/null; then
	sshraw() {
		TERM=xterm-256color ssh "$@"
	}
	alias ssh="kitty +kitten ssh"
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

###########
# Styling #
###########

export MCFLY_PROMPT="❯"

LS_COLORS='rs=0:di=01;33:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'
export LS_COLORS

##################################
# Machine specific configuration #
##################################
read -r -d '' local_machine_rc_template <<EOF
# This file contains configuration run at rc time for both bash and zsh
# If unsure where to put something, put it in local_machine_custom_init
local_machine_custom_early_init () {
    # Put things in this function that need to be run early in the init process
	# It will get called before almost everything else
	#
	# Everything in this file /must/ be idempotent, as there is a decent chance
	# it will get executed multiple times in the init process
	: # Prevents bash from getting pissy about a syntax error
}
local_machine_custom_init () {
    # Put things in this function that can be run later in the init process.
	: # Prevents bash from getting pissy about a syntax error
}
# Do /not/ anything to this file outside of the pre-defined functions
EOF
if [ "$local_machine_rc_exists" = "true" ]; then
	local_machine_custom_init
else
	echo -n "$local_machine_rc_template" >"$local_machine_rc_location"
fi

#########################
# End late.sh
#########################

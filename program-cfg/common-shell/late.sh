#########################
# Begin late.sh
#########################
# This file gets executed later in the shell load process;
# notably, after compinit in zsh and the interactive check in bash

###########
# Aliases #
###########

# Prevents being an idiot and stalling the wm
alias bspwm='bspc'
alias ls="exa"
alias la="exa -la"
alias tree="exa --tree"
alias sudo="sudo " # allows sudo to be used in aliases
# The backslashes here prevent recursion. See:
# https://stackoverflow.com/questions/18862777/how-can-i-swap-as-in-alias-command-names-in-zsh
alias cat="\bat"
alias bat="\cat"
alias nix-shell="nix-shell --command zsh"

####################
# Custom Functions #
####################

sshraw() {
	TERM=xterm-256color ssh "$@"
}

if command -v netctl-auto >/dev/null 2>&1; then
	nc() {
		if [ "$1" == "l" -o "$1" == "list" ]; then
			netctl-auto list
		else
			netctl-auto switch-to $1
		fi
	}
fi

_jnixpkgs() {
	local choice
	choice="$(fd-nixpkgs "$@" | gum choose)"
	if [ -d "$choice" ]; then
		pushd "$choice"
	else
		pushd "$HOME/g/.nixpkgs-reference/$(dirname "$choice")"
	fi
}
alias j-nixpkgs="_jnixpkgs"

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

#######################################
# Programming language specific stuff #
#######################################

export GEM_PATH="$GEM_HOME:$GEM_PATH"

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

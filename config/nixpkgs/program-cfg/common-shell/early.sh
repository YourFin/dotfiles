#########################
# Begin early.sh
#########################
# This file gets executed early in the shell load process;
# notably, before compinit in zsh and before the interactive check in
# bash
#
# WARNING: ALL OPERATIONS IN THIS FILE *MUST* BE IDEMPOTENT, AS THERE IS A
# GOOD CHANCE THEY WILL BE RUN MORE THAN ONCE DURING INIT
#######
# Nix #
#######

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

[ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] && . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

# Set lang
export LANG=en_US.UTF-8

################
# Path Setting #
################

if command -v go >/dev/null 2>/dev/null; then
	export PATH="$PATH:$GOPATH/bin"
fi

if command -v ruby >/dev/null 2>/dev/null &&
	GEM_USER_DIR=$(ruby -e 'print Gem.user_dir' 2>/dev/null); then
	export PATH="$PATH:$GEM_USER_DIR/bin"
fi

# Npm
[[ -d "$HOME/.local/usr/npm/global/bin/" ]] && export PATH="$PATH:$HOME/.local/usr/npm/global/bin/"

# Manually installed local stuff
export PATH="$HOME/.local/opt/yf-scripts/bin:$PATH"

[ -e "$HOME/.emacs.d/bin/" ] &&
	export PATH="$HOME/.emacs.d/bin/:$PATH"

export PATH="$PATH:$HOME/.yarn/bin"
[ -e "/home/pen/.local/usr/cargo/bin" ] &&
	export PATH="/home/pen/.local/usr/cargo/bin:$PATH"

export PATH="$HOME/.local/bin:$PATH"

local_machine_rc_location="$HOME/.config/localrc.sh"
if [ -f "$local_machine_rc_location" ]; then
	source "$local_machine_rc_location"
	local_machine_custom_early_init
	local_machine_rc_exists='true'
else
	local_machine_rc_exists='false'
fi

#########################
# End early.sh
#########################

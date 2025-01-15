#########################
# Begin early.sh
#########################
# This file gets executed early in the shell load process;
# notably, before compinit in zsh and before the interactive check in
# bash
#
# WARNING: ALL OPERATIONS IN THIS FILE *MUST* BE IDEMPOTENT, AS THERE IS A
# GOOD CHANCE THEY WILL BE RUN MORE THAN ONCE DURING INIT

# Profile zsh load
#zmodload zsh/zprof
#######
# Nix #
#######

# Single user mode
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Multi user
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
	. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

[ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] && . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

################
# Path Setting #
################

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

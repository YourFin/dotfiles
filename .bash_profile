#
# ~/.bash_profile
#

XDG_CONFIG_HOME="$HOME/.config"
if [ -f ~/.bashrc ]; then
	. ~/.bashrc_temp
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
. ~/.bashrc

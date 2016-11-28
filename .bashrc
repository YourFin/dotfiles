#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#prevents wm stalling
alias bspwm='bspc'

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

alias startmsf="sudo systemctl start postgresql.service & msfconsole --quiet -x \"db_connect ${USER}@msf\""

alias gdb='gdb -tui'
alias firefox='firefox --new-window'
alias y='yaourt -Syua'
alias sudo='sudo '
alias ls='ls --color=auto'
alias la='ls --color=auto --block-size=M -la'
alias lock='/opt/scripts/fancyi3lock.sh'
alias xclipcp="xclip -selection c"
alias wifimenu="sudo wifi-menu"

set -o vi

#prevent duplicates in the bash history, ie pwd followed by ls by ls, hitting up twice will get pwd
export HISTCONTROL=ignoredups

#Make prompt part white, all comands green
WHITE="\[$(tput setaf 1)\]"
RESET="\[$(tput setaf 10)\]"
PROMPTLINE='[\u@\h \W]\$ '
export PS1="${WHITE}${PROMPTLINE}${RESET}"

#make bash history entries have hour month day etc
export HISTTIMEFORMAT="%h %d %H:%M:%S> "

#saner bash history for multiple shells
#PROMPT_COMMAND="PEN_BASH_THROWAWAY=$BASH_COMMAND; date | xargs echo '>' $PEN_BASH_THROWAWAY >> ~/.bash_pen_history"

LS_COLORS=$LS_COLORS:'di=1;36:' ; export LS_COLORS

nc ()
{
	if [ "$1" == "l" -o "$1" == "list" ] ; then 
		netctl-auto list
	else
		netctl-auto switch-to $1
	fi
}

cpfancy ()
{
	if ! $(hash rsync >/dev/null 2>&1); then
		cp $*
		return
	fi

	while [[ $# -gt 1 ]]
	do
	key="$1"
	
	case $key in
	    -[^-]*[I,u,d,f][^-]*|--attributes-only|--copy-contents)
	    EXTENSION="$2"
	    shift # past argument
	    ;;
	    -s|--searchpath)
	    SEARCHPATH="$2"
	    shift # past argument
	    ;;
	    -l|--lib)
	    LIBPATH="$2"
	    shift # past argument
	    ;;
	    --default)
	    DEFAULT=YES
	    ;;
	    *)
	            # unknown option
	    ;;
	esac
	shift # past argument or value
	done
       		
}

mkcdir ()
{
	mkdir -p -- "$1" &&
	cd -P -- "$1"
}
cpd ()
{
	last=${@: -1}
	if [[ -d $last ]]
	then
		cp "${@}"
		cd -P $last
	else
		cp "${@}"
		cd -P $(dirname $last)

	fi
}
mvd ()
{
	last=${@: -1}
	if [[ -d $last ]]
	then
		mv "${@}"
		cd -P $last
	else
		mv "${@}"
		cd -P $(dirname $last)

	fi
}
scpd ()
{
	last=${@: -1}
	if [[ -d $last ]]
	then
		sudo cp "${@}"
		cd -P $last
	else
		sudo cp "${@}"
		cd -P $(dirname $last)

	fi
}
smvd ()
{
	last=${@: -1}
	if [[ -d $last ]]
	then
		sudo mv "${@}"
		cd -P $last
	else
		sudo mv "${@}"
		cd -P $(dirname $last)

	fi
}

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

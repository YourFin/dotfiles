#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"

linkFile ()
{
	first="$SCRIPTPATH/$1"
	second="$HOME/$2"
	if  [ -e $second ]
	then
		echo "$second already exists; replace it? (Y/n):"
		read userInput
		if [ "$userInput" = "n" ]
		then
			ln -sf $first $second 
		fi
	fi
}
small () 
{
	linkFile .bashrc .bashrc
	linkFile .vimrc .vimrc
	linkFile .vim .vim
}
medium ()
{
	linkFile .xbindkeysrc .xbindkeysrc 
	linkFile .Xmodmap .Xmodmap 
	linkFile .bash_profile .bash_profile 
	linkFile .inputrc .inputrc 
	small
}
large ()
{
	if ! [ -d $HOME/.config ] 
	then
		mkdir $HOME/.config
	fi
	linkFile .vimperatorrc
	linkFile .bashrc .bashrc
	linkFile bspwm/ .config/bspwm
	linkFile sxhkd/ .config/sxhkd
	linkFile gtk-3.0/ .config/gtk-3.0
	linkFile termite/ .config/termite
	linkFile compton.conf .config/compton.conf
	linkFile redshift.conf .config/redshift.conf
	linkFile backlightKeys .config/backlightKeys 
	linkFile .racketrc .racketrc
	medium
}



case $1 in
	s)
		small
		;;
	m)
		medium
		;;
	l)
		large
		;;
esac

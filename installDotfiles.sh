#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"

# First argument: file in this directory
# Second argument: destination relative to $HOME
linkFile ()
{
	first="$SCRIPTPATH/$1"
	second="$HOME/$2"
	if  [ -e $second ]
	then
		echo "$second already exists; replace it? (Y/n):"
		read userInput
		if ! [ "$userInput" = "n" ]
		then
      rm -rf $second
			ln -sf $first $second
		fi
	else
		ln -sf $first $second
	fi
}

#if [ -e "~/.config" ] ; then
#    cp -rn ~/.config/* $SCRIPTPATH/config
#fi
for file in $(ls $SCRIPTPATH/config); do
    linkFile config/$file .config/$file
done

for file in $(ls -a | grep -e '^\.[a-zA-Z0-9]' | grep -v git) ; do
    linkFile $file $file
done

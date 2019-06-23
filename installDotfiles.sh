#!/bin/bash

NO_CLOBBER=false
FORCE_CLOBBER=false

# Parse command line args
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -n|--no-clobber)
            NO_CLOBBER=true
            shift # past argument
            ;;
        -f|--force)
            # Clobber everything
            FORCE_CLOBBER=true
            shift # past argument
            shift # past value
            ;;
        *)    # unknown option
            POSITIONAL+=("$1") # save it in an array for later
            shift # past argument
            ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

# Unofficial Strict mode
set -euo pipefail
IFS=$'\n\t'

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
OLD_DOTFILES_DIR="$HOME/.local/opt/yf-old"

strip_home () {
    echo -En "$@" | sed "s:^$HOME/*::"
}

move_to_old () {
    if [ -e $1 ] ; then
        mkdir -p "$(dirname $OLD_DOTFILES_DIR/$(strip_home $1))"
        mv $1 "$OLD_DOTFILES_DIR/$(strip_home $1)"
    fi
}

# First argument: file in this directory
# Second argument: destination relative to $HOME
linkFile ()
{
	  first="$SCRIPTPATH/$1"
	  second="$HOME/$2"
	  if  [ -e $second ] ; then
        userInput="n"
        if ! $NO_CLOBBER && ! $FORCE_CLOBBER ; then
            echo "$second already exists; replace it? (Y/n):"
            read userInput
        fi
		    if [ ! "$userInput" = "n" ] || $FORCE_CLOBBER ; then
            move_to_old $second
			      ln -sf $first $second
		    fi
	  else
		    ln -sf $first $second
	  fi
}

mkdir -p "$HOME/.local/opt"
mkdir -p "$HOME/.local/usr"
mkdir -p "$HOME/.local/bin"

# Try to install git
if [ ! type git &>/dev/null ] ; then
    read -r -p 'Warning: git not installed; try to install it? [Y/n]'
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]] ; then
       if type apt &>/dev/null ; then
           echo "Attempting to install with apt..."
           sudo apt update
           sudo apt install git
       elif type apt-get &>/dev/null ; then
           echo "Attempting to install with apt-get..."
           sudo apt-get update
           sudo apt-get install git
       elif type pacman &>/dev/null ; then
           echo "Attempting to install with pacman..."
           sudo pacman -Syu git
       elif type yum &>/dev/null ; then
           echo "Attempting to install with yum..."
           sudo yum install git
       fi
    fi
fi

if [ type git &>/dev/null] ; then
    # Install scripts if not installed
    yf_scripts_dir="$HOME/.local/opt/yf-scripts"
    if [ ! -d "$yf_scripts_dir" ] ; then
        git clone https://github.com/YourFin/Scripts.git $yf_scripts_dir
    fi

    # Install spacemacs if not installed
    if [ ! -d "$HOME/.emacs.d" ] ; then
        if ([ -e "$HOME/.emacs" ] || $FORCE_CLOBBER) && ! $NO_CLOBBER ; then
            move_to_old "$HOME/.emacs"
        fi
        if ([ -e "$HOME/.emacs.d" ] || $FORCE_CLOBBER) && ! $NO_CLOBBER ; then
            move_to_old "$HOME/.emacs.d"
        fi
        if ([ -e "$HOME/.spacemacs.d" ] || $FORCE_CLOBBER) && ! $NO_CLOBBER ; then
            move_to_old "$HOME/.spacemacs.d"
        fi
        echo 'Installing spacemacs...'
        git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
        git clone https://github.com/yourfin/.spacemacs.d ~/.spacemacs.d
    fi
fi

mkdir -p "$HOME/.config"
for file in $(ls "$SCRIPTPATH/config"); do
    linkFile config/$file .config/$file
done

for file in $(ls -a | grep -e '^\.[a-zA-Z0-9]' | grep -v git) ; do
    linkFile $file $file
done

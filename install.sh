#!/usr/bin/env bash

NO_CLOBBER=false
FORCE_CLOBBER=false

# Parse command line args
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -n|--no-clobber)
            # Don't copy over anything
            NO_CLOBBER=true
            shift # past argument
            ;;
        -f|--force)
            # Clobber everything
            FORCE_CLOBBER=true
            shift # past argument
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

# shellcheck source=install/general_functions.sh
source "$SCRIPTPATH/install/general_functions.sh"

strip_home () {
    echo -En "$@" | sed "s:^$HOME/*::"
}

move_to_old () {
    if [ -e "$1" ] ; then
        mkdir -p "$(dirname "$OLD_DOTFILES_DIR"/"$(strip_home "$1")")"
        mv "$1" "$OLD_DOTFILES_DIR/$(strip_home "$1")"
    fi
}

# First argument: file in this directory
# Second argument: destination relative to $HOME
linkFile ()
{
	  first="$1"
	  second="$HOME/$2"
	  if  [ -e "$second" ] ; then
        userInput="n"
        if ! $NO_CLOBBER && ! $FORCE_CLOBBER ; then
            echo "$second already exists; replace it? (Y/n):"
            read -r userInput
        fi
		    if [ ! "$userInput" = "n" ] || $FORCE_CLOBBER ; then
            move_to_old "$second"
			      ln -sf "$first" "$second"
		    fi
	  else
		    ln -sf "$first" "$second"
	  fi
}

mkdir -p "$HOME/.local/opt"
mkdir -p "$HOME/.local/usr"
mkdir -p "$HOME/.local/bin"

try_install git

if exists git ; then
    # Install scripts if not installed
    yf_scripts_dir="$HOME/.local/opt/yf-scripts"
    if [ ! -d "$yf_scripts_dir" ] ; then
        git clone https://github.com/YourFin/Scripts.git "$yf_scripts_dir"
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
        echo 'Installing doom...'
        git clone https://github.com/hlissner/doom-emacs.git ~/.emacs.d
    fi
fi

# Copy over files in .config
mkdir -p "$HOME/.config"
for file in "$SCRIPTPATH/config"/*; do
    linkFile "$file" ".config/${file#"$SCRIPTPATH/config"}"
done

# Copy over files in desktop-files
DESKTOP_FILES_DIR=".local/share/applications"
mkdir -p "$HOME/$DESKTOP_FILES_DIR"
for file in "$SCRIPTPATH/desktop-files"/*; do
    linkFile "$file" "$DESKTOP_FILES_DIR/${file#"$SCRIPTPATH/desktop-files"}"
done


for file in $(ls -a | grep -e '^\.[a-zA-Z0-9]' | grep -v git) ; do # all files not starting with git
    linkFile "$SCRIPTPATH/$file" "$file"
done
linkFile "$SCRIPTPATH/.gitconfig" .gitconfig # As it is explicitly ignored otherwise

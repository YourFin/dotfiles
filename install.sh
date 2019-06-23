#!/bin/sh

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

# Try to install bash
if [ ! type bash &>/dev/null ] ; then
    read -r -p 'Warning: bash not installed; try to install it? [Y/n]'
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]] ; then
        if type apt &>/dev/null ; then
            echo "Attempting to install with apt..."
            sudo apt update
            sudo apt install bash
        elif type apt-get &>/dev/null ; then
            echo "Attempting to install with apt-get..."
            sudo apt-get update
            sudo apt-get install bash
        elif type pacman &>/dev/null ; then
            echo "Attempting to install with pacman..."
            sudo pacman -Syu bash
        elif type yum &>/dev/null ; then
            echo "Attempting to install with yum..."
            sudo yum install bash
        fi
    fi
fi

type bash &>/dev/null || (echo "No bash installed, exiting" && exit 1)
type git &>/dev/null || (echo "No git installed, exiting" && exit 1)

mkdir -p "$HOME/gitprojects/"
git clone https://github.com/yourfin/dotfiles "$HOME/gitprojects/dotfiles"
bash "$HOME/gitprojects/dotfiles/installDotfiles.sh"

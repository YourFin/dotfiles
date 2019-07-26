#!/bin/bash

exists () {
    type $1 &>/dev/null
}

try_install () {
    if ! exists $1 ; then
        read -r -p "Warning: $1 not installed; try to install it? [Y/n] "
        if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]] ; then
            if type apt &>/dev/null ; then
                echo "Attempting to install with apt..."
                sudo apt update
                sudo apt install $1
            elif type apt-get &>/dev/null ; then
                echo "Attempting to install with apt-get..."
                sudo apt-get update
                sudo apt-get install $1
            elif type pacman &>/dev/null ; then
                echo "Attempting to install with pacman..."
                sudo pacman -Syu $1
            elif type yum &>/dev/null ; then
                echo "Attempting to install with yum..."
                sudo yum install $1
            fi
        fi
    fi
}

must_install () {
    try_install $1
    if ! exists $1 ; then
        echo "Error: $1 could not be installed"
        exit 1
    fi
}

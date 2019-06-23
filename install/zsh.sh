#!/bin/bash

# Move to script directory
function finish {
    popd >/dev/null
}
pushd "$(dirname "$0")" >/dev/null || exit 1
trap finish EXIT

# Set sane defaults
set -euo pipefail
IFS=$'\n\t'

must_install zsh
must_install git
must_install curl

mkdir -p ~/.local/usr/zsh/

zsh zsh.zsh

sudo chsh -s $(which zsh)

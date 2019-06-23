#!/bin/zsh

# Set sane defaults
set -euo pipefail
IFS=$'\n\t'

# Install zplug
zplugInstalled="true"
if $(! [[ -d ~/.zplug ]]) && which curl >/dev/null 2> /dev/null; then
    zplugInstalled="false"
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
fi

source ~/.zplug/init.zsh

zplug "EslamElHusseiny/aws_manager_plugin"
zplug "zpm-zsh/dropbox"
zplug "djui/alias-tips"
zplug "desyncr/auto-ls"
zplug "mafredri/zsh-async", from:github
zplug "YourFin/pure-agnoster", use:pure-agnoster.zsh, from:github, as:theme
zplug "zsh-users/zsh-autosuggestions"
zplug "srijanshetty/zsh-pip-completion"
zplug "Tarrasch/zsh-bd"

zplug load
if [[ "$zplugInstalled" == "false" ]] ; then
    zplug install
    zplug update
fi

let
  bashrcText = ''
    # If not running interactively, don't do anything else
    [[ $- != *i* ]] && return

    ## History Settings
    #prevent duplicates in the bash history, ie pwd followed by ls by ls, hitting up twice will get pwd
    export HISTCONTROL=ignoredups
    #make bash history entries have hour month day etc
    export HISTTIMEFORMAT="%h %d %H:%M:%S> "

    export HISTSIZE=1000
    export SAVEHIST=1000
    export HISTFILESIZE=1000


    ## Prompt
    # Make prompt part white, all comands green
    WHITE="\[$(tput setaf 1)\]"
    RESET="\[$(tput setaf 10)\]"
    PROMPTLINE='[\u@\h \W]\$ '
    export PS1="''${WHITE}''${PROMPTLINE}''${RESET}"

    # Vi mode
    set -o vi
  '';

in { config, lib, pkgs, ... }:

{
  home.file.".bashrc".text = builtins.readFile ./common-shell/early.sh
    + bashrcText + builtins.readFile ./common-shell/late.sh;
}

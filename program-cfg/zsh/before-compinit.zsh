################################
# Begin before-compinit.sh
################################
# Allows tramp to work by setting the terminal to be as boring as possible
# Requires (setq tramp-terminal-type "tramp") somewhere in the emacs
# load chain
if [[ "$TERM" == "tramp" ]] ; then
	unsetopt zle
	PS1='$ '
	return
fi

# Make sure that we're using the zsh that nix installed, not some
# other one
ZSH=`which zsh`
if test x$SHELL != x$ZSH && test -e $ZSH && $ZSH -c true
then
  SHELL=$ZSH
  # The cryptic -$- passes all the options in effect for this current shell
  # to the replacement shell we are exec'ing.  This ensures a login shell
  # stays a login shell, etc.  See man zshparam, section "Parameters Set By
  # The Shell".
  exec $ZSH -$- "$@"
fi

# Kitty integration
if test -n "$KITTY_INSTALLATION_DIR"; then
    export KITTY_SHELL_INTEGRATION="enabled"
    autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
    kitty-integration
    unfunction kitty-integration
fi

(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"

typeset -g POWERLEVEL9K_CONFIG_FILE=/dev/null

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

zstyle ':completion:*' completer _expand _complete _ignored _match _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' format '%d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+l:|=* r:|=*' '+r:|[._-ABCDEFGHIJKLMNOPQRSTUVWXYZ]=** r:|=**'
zstyle ':completion:*' substitute 1

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
if [[ -t 0 && $- = *i* ]]
then
    stty -ixon
fi
################################
# End before-compinit.sh
################################

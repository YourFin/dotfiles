################################
# Begin init.zsh
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

(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"

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

setopt BANG_HIST # Treat the '!' character specially during expansion.
unsetopt beep
setopt HIST_IGNORE_ALL_DUPS # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS    # Do not display a line previously found.
setopt HIST_REDUCE_BLANKS   # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY          # Don't execute immediately upon history expansion.
setopt appendhistory extendedglob

## Draw a line between commands
# Check if \u2501 is printable
if [ -z "$({ echo -n "\u2501" >/dev/null; } 2>&1)" ]; then
    export DRAW_LINE_CHAR="\u2501"
else
    export DRAW_LINE_CHAR="-"
fi
MAX_SEPERATOR_WIDTH=60
draw_line() {
    print -Pn "%B%F{249}"
    # https://superuser.com/a/86342
    printf "$DRAW_LINE_CHAR"'%.0s' {1..$((COLUMNS < MAX_SEPERATOR_WIDTH ? COLUMNS : MAX_SEPERATOR_WIDTH))}
    print -Pn "%F{reset}%b\n"
}

typeset -a preexec_functions

preexec_functions+=("draw_line")

bindkey '^r' history-incremental-pattern-search-backward

# Direnv

if which "direnv" &>/dev/null; then
    eval "$(direnv hook zsh)"
fi

# Ocaml
test -r "$HOME"/.local/usr/opam/opam-init/init.zsh && . "$HOME"/.local/usr/opam/opam-init/init.zsh >/dev/null 2>/dev/null || true

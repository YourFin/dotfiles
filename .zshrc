# ---------------------------------------------------------------------------- #
# ---------------------------- Tramp Check ----------------------------------- #
# ---------------------------------------------------------------------------- #

# Allows tramp to work by setting the terminal to be as boring as possible
# Requires (setq tramp-terminal-type "tramp") somewhere in the emacs
# load chain
if [[ "$TERM" == "tramp" ]] ; then
	unsetopt zle
	PS1='$ '
	return
fi

# ---------------------------------------------------------------------------- #
# ---------------------- Pre-Powerlevel10k Instant load----------------------- #
# ---------------------------------------------------------------------------- #

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

# ---------------------------------------------------------------------------- #
# ------------------------ Prompt instant load ------------------------------- #
# ---------------------------------------------------------------------------- #
 
export ZDOTDIR="$HOME/.config/zdotdir"
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# ---------------------------------------------------------------------------- #
# ------------------------------ Zplug --------------------------------------- #
# ---------------------------------------------------------------------------- #
export ZPLUG_HOME=$HOME/.local/usr/zplug
if ! [ -d $ZPLUG_HOME ] ; then
    mkdir -p $(dirname $ZPLUG_HOME)
    git clone https://github.com/zplug/zplug $ZPLUG_HOME
    pushd $ZPLUG_HOME
    git checkout $(git describe --tags)
    popd
fi
source $ZPLUG_HOME/init.zsh

zplug "romkatv/powerlevel10k", as:theme, depth:1
zplug "zpm-zsh/dropbox"
zplug "djui/alias-tips"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug load

# ---------------------------------------------------------------------------- #
# ------------------------- Further Aesthetics ------------------------------- #
# ---------------------------------------------------------------------------- #

# See config/zdotdir/.p10k.zsh for prompt things

# Syntax highlighting
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[command]='none'
ZSH_HIGHLIGHT_STYLES[builtin]='none'

# Draw a line between commands
MAX_SEPERATOR_WIDTH=60
draw_line () {
    local line_char="\u2501"
    print -Pn "%B%F{249}"
    repeat $(( COLUMNS < MAX_SEPERATOR_WIDTH ? COLUMNS : MAX_SEPERATOR_WIDTH ))
    do
        echo -n "\u2501"
    done
    print -Pn "%F{reset}%b\n"
}

typeset -a preexec_functions

preexec_functions+=("draw_line")

#-------------------------------------------------#
#-------------------Normal Zsh--------------------#
#-------------------------------------------------#

# The following lines were added by compinstall
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

# Add zfuncs directory
fpath=(
    ~/.zfunc
    ~/.zfunc/**/*~*/(CVS)#(/N)
    ~/.local/opt/yf-scripts/zfunc
    ~/.local/opt/yf-scripts/zfunc/**/*~*/(CVS)#(/N)
    "${fpath[@]}"
)

autoload -Uz compinit
autoload -Uz up

compinit
# End of lines added by compinstall

# Turn off the terminal bell in every way possible
unsetopt beep
[[ "$osType" == "Linux" ]] && xset -b &>/dev/null || true

### Keybinds ###
bindkey -v
bindkey '^r' history-incremental-pattern-search-backward

autoload -U promptinit; promptinit

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt appendhistory extendedglob

#-------------------------------------------------#
#-------------------Sources-----------------------#
#-------------------------------------------------#

[[ -e ~/.local/opt/yf-scripts/common-shell-rc.sh ]] && source ~/.local/opt/yf-scripts/common-shell-rc.sh

[[ -e ~/.profile ]] && source ~/.profile

#Aliases
source <(cat ~/.aliases | sed -e 's/\(.*\)#.*/\1/' | sed -e '/^$/d' | sed -e 's/^/alias /') > /dev/null


HISTFILE=~/.local/usr/zsh/histfile
if ! [ -f "$HISTFILE" ] ; then
    mkdir -p "$(dirname $HISTFILE)"
    touch $HISTFILE
fi

which thefuck >/dev/null 2>/dev/null && eval $(thefuck --alias)

# --------------------------- OS-specific entries ---------------------------- #
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     osType=Linux;;
    Darwin*)    osType=OSx;;
    CYGWIN*)    osType=Cygwin;;
    MINGW*)     osType=MinGw;;
    *)          osType="UNKNOWN:${unameOut}"
esac

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f ~/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . ~/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f ~/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . ~/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f "$ZDOTDIR/.p10k.zsh" ]] || source "$ZDOTDIR/.p10k.zsh"
